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

import           Data.Either                    ( rights )
import           Data.Text.Lazy                 ( pack )
import           Data.Text.Lazy.Encoding        ( encodeUtf8 )
import           Data.Aeson
import           Data.Aeson.Types               ( Parser
                                                , parseEither
                                                )
import           Data.Aeson.Lens
import           Control.Lens                   ( cosmos
                                                , filtered
                                                , (^?)
                                                , toListOf
                                                )
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
                                 } deriving Show

isFunction :: Value -> Bool
isFunction v = v ^? (key "term" . _String) == Just "Function"

getAllFunctions :: Value -> [Object]
getAllFunctions = toListOf (cosmos . filtered isFunction . _Object)

parseFunctionObject :: Object -> Parser FunctionData
parseFunctionObject o = do
    sourceSpan              <- o .: "sourceSpan"
    [lineNumber, _]         <- sourceSpan .: "start"
    functionName            <- o .: "functionName"
    name                    <- functionName .: "name"
    (parameters :: [Value]) <- o .: "functionParameters"
    body                    <- o .: "functionBody"
    (statements :: [Value]) <- body .: "statements"
    return $ FunctionData "path"
                          lineNumber
                          "language"
                          name
                          (length parameters)
                          (length statements) -- TODO: count _every_ statement, not just the outer ones

-- | Given `RawJSONFile` tries to parse its `contents`.
-- | Returns either an error's description, or `ParsedFiles`.
parseRawJSONFile :: RawJSONFile -> Either String [FunctionData]
parseRawJSONFile (RawJSONFile _ contents) =
    let decodedJSON = eitherDecode $ encodeUtf8 $ pack contents
    in  case decodedJSON of
            (Left error) -> Left error
            (Right rootNode) ->
                Right
                    $ rights
                    $ map (parseEither parseFunctionObject)
                    $ getAllFunctions rootNode
