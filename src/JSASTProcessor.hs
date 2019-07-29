{-# LANGUAGE DeriveDataTypeable #-}

module JSASTProcessor
    ( RawSourceFile(RawSourceFile)
    , RawSourceCode
    , FunctionData(FunctionData)
    , filePath
    , fName
    , arity
    , purity
    , explicitReturn
    , stmts
    , parseRawSourceFiles
    )
where

import           Data.List                      ( isInfixOf )
import           Data.Data
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
                                                    )
                                                , JSExpression
                                                    ( JSFunctionExpression
                                                    , JSCallExpression
                                                    , JSCallExpressionDot
                                                    , JSCallExpressionSquare
                                                    )
                                                , JSAnnot(JSNoAnnot)
                                                , JSIdent
                                                    ( JSIdentName
                                                    , JSIdentNone
                                                    )
                                                , JSBlock(JSBlock)
                                                )
import           Data.Generics.Uniplate.DataOnly
                                                ( universe
                                                , universeBi
                                                )

type RawSourceCode = String
data RawSourceFile = RawSourceFile FilePath RawSourceCode deriving Show

type FunctionIdentifier = String
type Arity = Int
type IsPure = Bool
type IsReturnExplicit = Bool

data FunctionData = FunctionData { filePath :: FilePath
                                 , fName :: FunctionIdentifier
                                 , arity :: Arity
                                 , purity :: IsPure
                                 , explicitReturn :: IsReturnExplicit
                                 , stmts :: [JSStatement]
                                 , rawSourceCode :: RawSourceCode }

instance Eq FunctionData where
    (==) (FunctionData filePath1 fIdent1 _ _ _ _ sc1) (FunctionData filePath2 fIdent2 _ _ _ _ sc2)
        = filePath1 == filePath2 && fIdent1 == fIdent2 && sc1 == sc2

-- | Returns JavaScript Function's identifier.
getJSIdent :: JSIdent -> String
getJSIdent (JSIdentName _ s) = s
getJSIdent JSIdentNone       = "<anonymous fn>"

-- | Given `JSBlock`, returns the list of its statements.
getJSBlockStatemtns :: JSBlock -> [JSStatement]
getJSBlockStatemtns (JSBlock _ s _) = s

-- | Returns `True` if the provided source code has an occurrence of a `return`.
-- | Otherwise `False`.
isReturnExplicit :: RawSourceCode -> Bool
isReturnExplicit sc = "return" `isInfixOf` sc

-- | Returns `True` if there are exacly zero calls of other functions/object methods.
-- | Otherwise `False`.
isPure
    :: [JSExpression]
    -> [JSExpression]
    -> [JSExpression]
    -> [JSStatement]
    -> Bool
isPure callExprs callExprsDot callExprsSqr callMethods =
    null callExprs && null callExprsDot && null callExprsSqr && null callMethods

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

-- | Parses function (defined as either `JSStatement` or `JSExpression`)
-- | into `FunctionData`. Only `JSFunction` and `JSFunctionExpression`
-- | data constructors are supported.
jsFunctionToFunctionData :: FilePath -> JSASTFn -> FunctionData
jsFunctionToFunctionData filePath jsf =
    let
        rawFunctionSourceCode   = extractFnSourceCode jsf
        (fIdent, arity, fBlock) = extractFnParts jsf
        callExprs =
            [ callExprs | callExprs@JSCallExpression{} <- universeBi jsf ]
        callExprsDot =
            [ callExprsDot
            | callExprsDot@JSCallExpressionDot{} <- universeBi jsf
            ]
        callExprsSqr =
            [ callExprsSqr
            | callExprsSqr@JSCallExpressionSquare{} <- universeBi jsf
            ]
        callMethods =
            [ callMethods | callMethods@JSMethodCall{} <- universeBi jsf ]
        statements = getJSBlockStatemtns fBlock
    in
        FunctionData
            filePath
            fIdent
            arity
            (isPure callExprs callExprsDot callExprsSqr callMethods)
            (isReturnExplicit rawFunctionSourceCode)
            statements
            rawFunctionSourceCode
jsStmtFunctionToFunctionData _ =
    error
        "Only `JSFunction` and `JSFunctionExpression` data constructors are supported"

-- | Parses `RawSourceFile` into a list of `FunctionData`.
parseRawSourceFile :: RawSourceFile -> [FunctionData]
parseRawSourceFile (RawSourceFile path sourceCode) =
    let
        parseJSFn = jsFunctionToFunctionData path
        jsAST     = parseModule sourceCode path
        jsFunctions =
            [ jsStatements | jsStatements@JSFunction{} <- universeBi jsAST ]
        jsFunctionsInExpr =
            [ jsStatements
            | jsStatements@JSFunctionExpression{} <- universeBi jsAST
            ]
        parsedJSFunctions       = map (parseJSFn . Stmt) jsFunctions
        parsedJSFunctionsInExpr = map (parseJSFn . Expr) jsFunctionsInExpr
    in
        (parsedJSFunctions ++ parsedJSFunctionsInExpr)

-- | Parses a list of `RawSourceFile` into a list of `FunctionData`.
parseRawSourceFiles :: [RawSourceFile] -> [FunctionData]
parseRawSourceFiles = concatMap parseRawSourceFile
