{-# LANGUAGE DeriveDataTypeable #-}

module JSASTProcessor
    ( RawSourceFile(RawSourceFile)
    , RawSourceCode
    , FunctionData(FunctionData)
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

type FunctionName = String
type Arity = Int
type IsPure = Bool
type IsReturnExplicit = Bool

data FunctionData = FunctionData FilePath FunctionName Arity IsPure IsReturnExplicit [JSStatement] RawSourceCode

instance Eq FunctionData where
    (==) (FunctionData filePath1 fIdent1 _ _ _ _ sc1) (FunctionData filePath2 fIdent2 _ _ _ _ sc2)
        = filePath1 == filePath2
        && fIdent1 == fIdent2
        && sc1 == sc2

instance Show FunctionData where
    show (FunctionData filePath fIdent _ _ _ _ _) = "Function `" ++ fIdent ++ "` (" ++ filePath ++ ")\n"

getJSIdent :: JSIdent -> String
getJSIdent (JSIdentName _ s) = s
getJSIdent JSIdentNone       = ""

getJSBlockStatemtns :: JSBlock -> [JSStatement]
getJSBlockStatemtns (JSBlock _ s _) = s

isReturnExplicit :: RawSourceCode -> Bool
isReturnExplicit sc = "return" `isInfixOf` sc

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

extractFnSourceCode :: JSASTFn -> RawSourceCode
extractFnSourceCode (Stmt jsf) = renderToString $ JSAstStatement jsf JSNoAnnot
extractFnSourceCode (Expr jsf) = renderToString $ JSAstExpression jsf JSNoAnnot

extractFnParts :: JSASTFn -> (FunctionName, Arity, JSBlock)
extractFnParts (Stmt (JSFunction _ fIdent _ argsList _ fBlock _)) =
    (getJSIdent fIdent, length (universe argsList), fBlock)
extractFnParts (Expr (JSFunctionExpression _ fIdent _ argsList _ fBlock)) =
    (getJSIdent fIdent, length (universe argsList), fBlock)

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

parseRawSourceFiles :: [RawSourceFile] -> [FunctionData]
parseRawSourceFiles = concatMap parseRawSourceFile
