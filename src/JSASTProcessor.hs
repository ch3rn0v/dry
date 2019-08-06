{-# LANGUAGE DeriveDataTypeable #-}

module JSASTProcessor
    ( RawSourceFile(RawSourceFile)
    , FunctionData(FunctionData)
    , ConstrCountMap
    , filePath
    , lineNumber
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
import           Data.Either                    ( partitionEithers )
import           Data.List                      ( foldl' )
import           Data.Data                      ( Data
                                                , Constr
                                                , toConstr
                                                )
import           Numeric.Extra                  ( intToDouble )
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
import           Language.JavaScript.Parser.SrcLocation
                                                ( TokenPosn(TokenPn) )
import           Language.JavaScript.Parser.AST ( JSStatement(JSFunction)
                                                , JSExpression
                                                    ( JSFunctionExpression
                                                    )
                                                , JSAnnot(JSAnnot, JSNoAnnot)
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
import           JSSettings                     ( defaultConstructorWeight
                                                , constructorWeightsMap
                                                )
import           Helpers                        ( combineWithKeyAndDefault )

instance Ord Constr where
    (<=) c1 c2 = show c1 <= show c2

type ConstrCountMap = Map String Double

type RawSourceCode = String
data RawSourceFile = RawSourceFile FilePath RawSourceCode deriving Show

type FunctionIdentifier = String
type Arity = Int
type LineNumber = String

data FunctionData = FunctionData { filePath :: FilePath
                                 , lineNumber :: LineNumber
                                 , fName :: FunctionIdentifier
                                 , arity :: Arity
                                 , entitiesCountMap :: ConstrCountMap
                                 , stmtsCount :: Int
                                 , rawSourceCode :: RawSourceCode }

instance Eq FunctionData where
    (==) (FunctionData _ _ fIdent1 _ _ _ sc1) (FunctionData _ _ fIdent2 _ _ _ sc2)
        = fIdent1 == fIdent2 && sc1 == sc2

-- | Returns JavaScript Function's identifier.
getJSIdent :: JSIdent -> String
getJSIdent (JSIdentName _ s) = s
getJSIdent JSIdentNone       = "<anonymous fn>"

-- | Given `JSBlock`, returns the list of its statements.
getJSBlockStatemtns :: JSBlock -> [JSStatement]
getJSBlockStatemtns (JSBlock _ s _) = s

-- | Given `JSAnnot` constructed with `JSAnnot`, returns the token's line number.
-- | In case no data about token's position is available, returns an empty string.
getLineNumber :: JSAnnot -> LineNumber
getLineNumber (JSAnnot (TokenPn _ lineNr _) _) = show lineNr
getLineNumber _ = ""

data JSStatementOrExpression a b = Stmt a | Expr b deriving Data
type JSASTFn = JSStatementOrExpression JSStatement JSExpression

-- | Returns function's source code. Works for functions defined as
-- | either `JSStatement` or `JSExpression`.
extractFnSourceCode :: JSASTFn -> RawSourceCode
extractFnSourceCode (Stmt jsf) = renderToString $ JSAstStatement jsf JSNoAnnot
extractFnSourceCode (Expr jsf) = renderToString $ JSAstExpression jsf JSNoAnnot

-- | Returns function's identifier, arity and its `JSBlock`.
-- | Works for functions defined as either `JSStatement` or `JSExpression`.
extractFnParts :: JSASTFn -> (FunctionIdentifier, Arity, JSBlock, LineNumber)
extractFnParts (Stmt (JSFunction fAnnot fIdent _ argsList _ fBlock _)) =
    ( getJSIdent fIdent
    , length (universe argsList)
    , fBlock
    , getLineNumber fAnnot
    )
extractFnParts (Expr (JSFunctionExpression fAnnot fIdent _ argsList _ fBlock))
    = ( getJSIdent fIdent
      , length (universe argsList)
      , fBlock
      , getLineNumber fAnnot
      )

-- | Given the count of a constructor's occurrences and the corresponding
-- | weight, returns the weighted constructor's occurrences count.
multiplyConstructorCountByWeight :: Int -> Double -> Double
multiplyConstructorCountByWeight c w = w * intToDouble c

-- | Recursively traverses `JSStatement` building a Map,
-- | to count all occurrences of every constructor used within
-- | the given `JSStatement` tree.
constructorsFromStatement :: JSStatement -> ConstrCountMap
constructorsFromStatement =
    combineWithKeyAndDefault multiplyConstructorCountByWeight
                             defaultConstructorWeight
                             constructorWeightsMap
        . everything
              (unionWith (+))
              (      const empty
              `extQ` (\x -> singleton (show $ toConstr (x :: JSStatement)) 1)
              `extQ` (\x -> singleton (show $ toConstr (x :: JSExpression)) 1)
              `extQ` (\x -> singleton (show $ toConstr (x :: JSUnaryOp)) 1)
              `extQ` (\x -> singleton (show $ toConstr (x :: JSBinOp)) 1)
              )

-- | Recursively traverses each `JSStatement` building a Map,
-- | then unions the maps with (+), returning the map of all
-- | the constructors and their total respective counts.
constructorsFromStatements :: [JSStatement] -> ConstrCountMap
constructorsFromStatements =
    foldl' (unionWith (+)) empty . map constructorsFromStatement

-- | Parses function (defined as either `JSStatement` or `JSExpression`)
-- | into `FunctionData`. Only `JSFunction` and `JSFunctionExpression`
-- | data constructors are supported.
jsFunctionToFunctionData :: FilePath -> JSASTFn -> FunctionData
jsFunctionToFunctionData filePath jsf =
    let (fIdent, arity, fBlock, lineNumber) = extractFnParts jsf
        statements                          = getJSBlockStatemtns fBlock
        rawFunctionSourceCode               = extractFnSourceCode jsf
    in  FunctionData filePath
                     lineNumber
                     fIdent
                     arity
                     (constructorsFromStatements statements)
                     (length statements)
                     rawFunctionSourceCode

-- | Extracts all functions from the given `JSAST`,
-- | returns them as a list of `FunctionData`
-- | (or a `String` containing error message).
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
