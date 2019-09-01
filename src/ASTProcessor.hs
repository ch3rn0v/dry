{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : ASTProcessor
Description : Parses AST related data from .json files

Parses AST that is output by semantic library
(https://github.com/github/semantic/) in .json format.
-}
module ASTProcessor
    ( parseRawJSONFile
    )
where

import           GHC.Generics
import           Data.Data
import           Control.Applicative            ( optional
                                                , (<$>)
                                                )
import           Data.Maybe                     ( catMaybes )
import           Data.Text.Lazy                 ( pack )
import           Data.Text.Lazy.Encoding        ( encodeUtf8 )
import           Data.Aeson
import           Data.Aeson.Types               ( Parser )
import           FileProcessor                  ( RawJSONFile(RawJSONFile) )

type JSONParsingError = String
type LineNumber = Int
type Language = String
type FunctionIdentifier = String
type Arity = Int
data FunctionData = FunctionData { filePath :: FilePath
                                 , lineNumber :: LineNumber
                                 , language :: Language
                                 , name :: FunctionIdentifier
                                 , arity :: Arity
                                 -- , entitiesCountMap :: ConstrCountMap
                                 , stmtsCount :: Int
                                 }
                  | FunctionParsingError { functionParsingError :: String
                  } deriving (Generic, Data, Typeable, Show)

newtype FunctionDataList = FDL { fds :: [FunctionData] } deriving Show

-- | Parses the tree if its term is `Statements`
-- | Fails otherwise.
parseTree :: (Object, b, c) -> Parser ([Object], b, c)
parseTree (tree, path, language) = do
    (term :: Value) <- tree .: "term"
    case term of
        "Statements" -> do
            (statements :: [Object]) <- tree .: "statements"
            return (statements, path, language)
        other -> fail "Error: No `statements` node"

parseTree' :: (Object, b, c) -> Parser (Maybe ([Object], b, c))
parseTree' treeData = optional (parseTree treeData)

-- | Parses a statement from a tree.
-- | If it is a `Function`, returns `FunctionData`.
-- | Fails otherwise.
parseStmtObject :: FilePath -> Language -> Object -> Parser FunctionData
parseStmtObject path language o = do
    t <- o .: "term"
    case t of
        (Just "Function") -> do
            sourceSpan              <- o .: "sourceSpan"
            [lineNumber, _]         <- sourceSpan .: "start"
            functionName            <- o .: "functionName"
            name                    <- functionName .: "name"
            (parameters :: [Value]) <- o .: "functionParameters"
            body                    <- o .: "functionBody"
            (statements :: [Value]) <- body .: "statements"
            return $ FunctionData path
                                  lineNumber
                                  language
                                  name
                                  (length parameters)
                                  (length statements) -- TODO: count _every_ statement, not just the outer ones
        (Just other) -> -- TODO: traverse the child nodes recursively
            fail ("No Function term, `" ++ other ++ "` found instead")
        Nothing -> fail "No Function term found"

parseStmtObject'
    :: FilePath -> Language -> Object -> Parser (Maybe FunctionData)
parseStmtObject' path language o = optional (parseStmtObject path language o)

-- | Parses FunctionData from all statements.
-- | Omits fails and returns successful results.
parseFunction :: Parser ([Object], FilePath, Language) -> Parser [FunctionData]
parseFunction statementParser = do
    (statementObjects, path, language) <- statementParser
    catMaybes <$> traverse (parseStmtObject' path language) statementObjects

instance FromJSON FunctionDataList where
    parseJSON = withObject "FunctionData" $ \o -> do
        (treeRootNodes :: [Object]    ) <- o .: "trees"
        (treesContents :: [Object]    ) <- mapM (.: "tree") treeRootNodes
        (pathObjects :: [FilePath]    ) <- mapM (.: "path") treeRootNodes
        (languageObjects :: [Language]) <- mapM (.: "language") treeRootNodes

        let trees = zip3 treesContents pathObjects languageObjects

        successfullyParsedTrees <- catMaybes <$> traverse parseTree' trees
        let statementParsers = map return successfullyParsedTrees

        fds <- mapM parseFunction statementParsers
        return $ FDL $ concat fds

-- | Given `RawJSONFile` tries to parse its contents.
-- | Returns either an error's description, or `ParsedFiles`.
parseRawJSONFile :: RawJSONFile -> Either String [FunctionData]
parseRawJSONFile (RawJSONFile _ contents) =
    let r = eitherDecode $ encodeUtf8 $ pack contents
    in  case r of
            (Right (FDL fds)) -> Right fds
            (Left  error    ) -> Left error
