{-# LANGUAGE DeriveDataTypeable #-}

module JSASTProcessor
    ( RawSourceFile(RawSourceFile)
    , RawSourceCode
    , FunctionData(FunctionData)
    , filePath
    , fName
    , arity
    , impureCallsCount
    , explicitReturn
    , stmts
    , declarationsCount
    , parseRawSourceFiles
    )
where

import           Debug.Trace                    ( trace )
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
                                                    , JSLet
                                                    , JSConstant
                                                    , JSVariable
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
type IsReturnExplicit = Bool

data FunctionData = FunctionData { filePath :: FilePath
                                 , fName :: FunctionIdentifier
                                 , arity :: Arity
                                 , impureCallsCount :: Int
                                 , explicitReturn :: IsReturnExplicit
                                 , stmts :: [JSStatement]
                                 , declarationsCount :: Int
                                 , rawSourceCode :: RawSourceCode }

instance Eq FunctionData where
    (==) (FunctionData filePath1 fIdent1 _ _ _ _ _ sc1) (FunctionData filePath2 fIdent2 _ _ _ _ _ sc2)
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

-- | Counts every identifier that has been declared using `let`, `const`, or `var`.
-- | Returns the total count of the identifiers, declared in the given `JSStatement`.
countDeclaredJSIdentifiers :: JSStatement -> Int
countDeclaredJSIdentifiers stmt =
    let
        jsLetDeclarations =
            [ letDeclrs | letDeclrs@JSLet{} <- universeBi stmt ]
        jsConstDeclarations =
            [ letDeclrs | letDeclrs@JSConstant{} <- universeBi stmt ]
        jsVarDeclarations =
            [ letDeclrs | letDeclrs@JSVariable{} <- universeBi stmt ]
    in
        sum $ map
            length
            [jsLetDeclarations, jsConstDeclarations, jsVarDeclarations]

-- | Counts every identifier that has been declared using `let`, `const`, or `var`.
-- | Returns the total count of the identifiers, declared in the given list of `JSStatement`.
getDeclarationsCount :: [JSStatement] -> Int
getDeclarationsCount = sum . map countDeclaredJSIdentifiers

data JSStatementOrExpression a b = Stmt a | Expr b deriving Data
type JSASTFn = JSStatementOrExpression JSStatement JSExpression

-- | Counts every occurrence of a function call within the given function.
countExternalCalls :: JSASTFn -> Int
countExternalCalls jsf =
    let
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
    in
        length callMethods -- because `callMethods` is of type `[JSStatement]`,
                           -- while the other lists are `[JSExpression]`
            + sum (map length [callExprs, callExprsDot, callExprsSqr])

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
    let rawFunctionSourceCode   = extractFnSourceCode jsf
        (fIdent, arity, fBlock) = extractFnParts jsf
        statements              = getJSBlockStatemtns fBlock
    in  FunctionData filePath
                     fIdent
                     arity
                     (countExternalCalls jsf)
                     (isReturnExplicit rawFunctionSourceCode)
                     statements
                     (getDeclarationsCount statements)
                     rawFunctionSourceCode
jsStmtFunctionToFunctionData _ =
    error
        "Only `JSFunction` and `JSFunctionExpression` data constructors are supported"

-- | Parses `RawSourceFile` into a list of `FunctionData`.
parseRawSourceFile :: RawSourceFile -> [FunctionData]
parseRawSourceFile (RawSourceFile path sourceCode) =
    let
        parseJSFn = jsFunctionToFunctionData path
        jsAST     = parseModule sourceCode path -- TODO: Add error processing in case AST parsing failed
        jsFunctions =
            [ jsStatements | jsStatements@JSFunction{} <- universeBi jsAST ]
        jsFunctionsInExpr =
            [ jsStatements
            | jsStatements@JSFunctionExpression{} <- universeBi jsAST
            ]
        parsedJSFunctions       = map (parseJSFn . Stmt) jsFunctions
        parsedJSFunctionsInExpr = map (parseJSFn . Expr) jsFunctionsInExpr
    in
        parsedJSFunctions ++ parsedJSFunctionsInExpr

-- | Parses a list of `RawSourceFile` into a list of `FunctionData`.
parseRawSourceFiles :: [RawSourceFile] -> [FunctionData]
parseRawSourceFiles = concatMap parseRawSourceFile
