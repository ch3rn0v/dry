{-# LANGUAGE DeriveDataTypeable #-}

module JSASTProcessor
    ( RawSourceFile(RawSourceFile)
    , RawSourceCode
    , FunctionData(FunctionData)
    , filePath
    , fName
    , arity
    , entitiesCountMap
    , stmtsCount
    , parseRawSourceFiles
    , partitionASTParsingResults
    )
where

import           Data.Generics.Schemes          ( everything )
import           Data.Generics.Aliases          ( extQ )
import           Data.Either                    ( isRight
                                                , fromLeft
                                                , partitionEithers
                                                )
import           Data.List                      ( foldl'
                                                , isInfixOf
                                                )
import           Data.Data                      ( Data
                                                , Constr
                                                , toConstr
                                                )
import           Data.Map.Strict                ( Map
                                                , empty
                                                , unionWith
                                                , singleton
                                                )
import           Language.JavaScript.Parser     ( parseModule
                                                , renderToString
                                                , JSAST
                                                    ( JSAstStatement
                                                    , JSAstExpression
                                                    )
                                                )
import           Language.JavaScript.Parser.AST ( JSStatement
                                                    ( JSFunction
                                                    , JSMethodCall
                                                    , JSLet
                                                    , JSConstant
                                                    , JSVariable
                                                    )
                                                , JSExpression
                                                    ( JSFunctionExpression
                                                    , JSCallExpression
                                                    , JSCallExpressionDot
                                                    , JSCallExpressionSquare
                                                    , JSMemberExpression
                                                    )
                                                , JSAnnot(JSNoAnnot)
                                                , JSIdent
                                                    ( JSIdentName
                                                    , JSIdentNone
                                                    )
                                                , JSBlock(JSBlock)
                                                , JSUnaryOp
                                                , JSBinOp
                                                )
import           Data.Generics.Uniplate.DataOnly
                                                ( universe
                                                , universeBi
                                                )

instance Ord Constr where
    (<=) c1 c2 = show c1 <= show c2

type RawSourceCode = String
data RawSourceFile = RawSourceFile FilePath RawSourceCode deriving Show

type FunctionIdentifier = String
type Arity = Int

data FunctionData = FunctionData { filePath :: FilePath
                                 , fName :: FunctionIdentifier
                                 , arity :: Arity
                                 , entitiesCountMap :: Map Constr Int
                                 , stmtsCount :: Int
                                 , rawSourceCode :: RawSourceCode }

instance Eq FunctionData where
    (==) (FunctionData filePath1 fIdent1 _ _ _ sc1) (FunctionData filePath2 fIdent2 _ _ _ sc2)
        = filePath1 == filePath2 && fIdent1 == fIdent2 && sc1 == sc2

-- | Returns JavaScript Function's identifier.
getJSIdent :: JSIdent -> String
getJSIdent (JSIdentName _ s) = s
getJSIdent JSIdentNone       = "<anonymous fn>"

-- | Given `JSBlock`, returns the list of its statements.
getJSBlockStatemtns :: JSBlock -> [JSStatement]
getJSBlockStatemtns (JSBlock _ s _) = s

data JSStatementOrExpression a b = Stmt a | Expr b deriving Data
type JSASTFn = JSStatementOrExpression JSStatement JSExpression

-- | Returns function's source code. Works for functions defined as
-- | either `JSStatement` or `JSExpression`.
extractFnSourceCode :: JSASTFn -> RawSourceCode
extractFnSourceCode (Stmt jsf) = renderToString $ JSAstStatement jsf JSNoAnnot
extractFnSourceCode (Expr jsf) = renderToString $ JSAstExpression jsf JSNoAnnot

-- | Returns function's identifier, arity and its `JSBlock`.
-- | Works for functions defined as either `JSStatement` or `JSExpression`.
extractFnParts :: JSASTFn -> (FunctionIdentifier, Arity, JSBlock)
extractFnParts (Stmt (JSFunction _ fIdent _ argsList _ fBlock _)) =
    (getJSIdent fIdent, length (universe argsList), fBlock)
extractFnParts (Expr (JSFunctionExpression _ fIdent _ argsList _ fBlock)) =
    (getJSIdent fIdent, length (universe argsList), fBlock)

-- | Recursively traverses `JSStatement` building a Map,
-- | to count all occurrences of every constructor used within
-- | the given `JSStatement` tree.
countConstructorOccurrences :: JSStatement -> Map Constr Int
countConstructorOccurrences = everything
    (unionWith (+))
    (      const empty
    `extQ` (\x -> singleton (toConstr (x :: JSStatement)) 1)
    `extQ` (\x -> singleton (toConstr (x :: JSExpression)) 1)
    `extQ` (\x -> singleton (toConstr (x :: JSUnaryOp)) 1)
    `extQ` (\x -> singleton (toConstr (x :: JSBinOp)) 1)
    )

-- | Parses function (defined as either `JSStatement` or `JSExpression`)
-- | into `FunctionData`. Only `JSFunction` and `JSFunctionExpression`
-- | data constructors are supported.
jsFunctionToFunctionData :: FilePath -> JSASTFn -> FunctionData
jsFunctionToFunctionData filePath jsf =
    let (fIdent, arity, fBlock) = extractFnParts jsf
        statements              = getJSBlockStatemtns fBlock
        rawFunctionSourceCode   = extractFnSourceCode jsf
    in  FunctionData
            filePath
            fIdent
            arity
            (foldl'
                (\countersMap stmts -> unionWith (+) countersMap
                    $ countConstructorOccurrences stmts
                )
                empty
                statements
            )
            (length statements)
            rawFunctionSourceCode

parseJSASTToFunctionData
    :: FilePath -> Either String JSAST -> Either String [FunctionData]
parseJSASTToFunctionData path (Left s) =
    Left ("Unable to parse file `" ++ path ++ "`.\n" ++ s)
parseJSASTToFunctionData path (Right jsAST) =
    let
        parseJSFn = jsFunctionToFunctionData path
        jsFunctions =
            [ jsStatements | jsStatements@JSFunction{} <- universeBi jsAST ]
        jsFunctionsInExpr =
            [ jsStatements
            | jsStatements@JSFunctionExpression{} <- universeBi jsAST
            ]
        parsedJSFunctions       = map (parseJSFn . Stmt) jsFunctions
        parsedJSFunctionsInExpr = map (parseJSFn . Expr) jsFunctionsInExpr
    in
        Right (parsedJSFunctions ++ parsedJSFunctionsInExpr)

-- | Parses `RawSourceFile` and returns
-- | either a String with error, or a list of `FunctionData`.
parseRawSourceFile :: RawSourceFile -> Either String [FunctionData]
parseRawSourceFile (RawSourceFile path sourceCode) =
    parseJSASTToFunctionData path $ parseModule sourceCode path

-- | Outputs parsing errors (if any),
-- | otherwise outputs nothing.
outputASTParsingErrors :: [String] -> IO ()
outputASTParsingErrors []     = pure ()
outputASTParsingErrors errors = do
    putStrLn
        $  "\nFiles failed to be parsed: "
        ++ show (length errors)
        ++ "\n\nDetails:"
    mapM_ putStrLn errors

-- | Splits AST parsing results into errors and `[FunctionData]`,
-- | outputs errors (if any), and returns `[FunctionData]`.
partitionASTParsingResults
    :: [Either String [FunctionData]] -> IO [FunctionData]
partitionASTParsingResults astParsingResults =
    let (errors, parsedFunctionData) = partitionEithers astParsingResults
    in  do
            outputASTParsingErrors errors
            pure $ concat parsedFunctionData

-- | Parses a list of `RawSourceFile` into a list of `FunctionData`.
parseRawSourceFiles :: [RawSourceFile] -> [Either String [FunctionData]]
parseRawSourceFiles = map parseRawSourceFile
