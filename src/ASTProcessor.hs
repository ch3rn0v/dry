{-# LANGUAGE TupleSections #-}
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
    , FunctionData
    , StatementsData
    , StatementsCountMap
    , filePath
    , lineNumber
    , name
    , arity
    , stmtsData
    , stmtsCount
    )
where

import qualified Data.IntMap.Strict            as IM
                                                ( IntMap
                                                , fromListWith
                                                , map
                                                , elems
                                                )
import qualified Data.HashMap.Strict           as HM
                                                ( HashMap
                                                , fromListWith
                                                , keys
                                                , lookup
                                                , mapWithKey
                                                )
import           Data.Maybe                     ( isJust
                                                , fromJust
                                                )
import           Data.Either                    ( rights )
import           Data.Text                      ( Text )
import           Data.Text.Lazy                 ( pack )
import           Data.Text.Lazy.Encoding        ( encodeUtf8 )
import           Data.Aeson
import           Data.Aeson.Types               ( Parser
                                                , parseEither
                                                )
import           Data.Aeson.Lens                ( _Object
                                                , _String
                                                , key
                                                , nonNull
                                                )
import           Control.Lens                   ( cosmos
                                                , filtered
                                                , (^?)
                                                , toListOf
                                                )
import           FileProcessor                  ( RawJSONFile(RawJSONFile) )
import           StatementWeights               ( defaultWeight
                                                , stmtWeights
                                                )

type DepthStatementData = (Int, [Text])

type LineNumber = Int
type Language = String
type FunctionIdentifier = String
type Arity = Int
type StatementsCountMap = HM.HashMap Text Double
type RawStatementsData = IM.IntMap [Text]
type StatementsData = IM.IntMap StatementsCountMap
data FunctionData = FunctionData { filePath :: FilePath
                                 , lineNumber :: LineNumber
                                 , language :: Language
                                 , name :: FunctionIdentifier
                                 , arity :: Arity
                                 , stmtsData :: StatementsData
                                 , stmtsCount :: Int
                                 } deriving Show

data TreeData = TreeData { rootNode :: Value
                         , treeFilePath :: FilePath
                         , treeLanguage :: Language
                         } deriving Show

isKeyPresent :: Text -> Value -> Bool
isKeyPresent keyName v = isJust $ v ^? (key keyName . nonNull)

isTree :: Value -> Bool
isTree = isKeyPresent "tree"

isFunction :: Value -> Bool
isFunction v = v ^? (key "term" . _String) == Just "Function"

getAllElements :: (Value -> Bool) -> Value -> [Object]
getAllElements filterFn = toListOf (cosmos . filtered filterFn . _Object)

getAllFunctions :: Value -> [Object]
getAllFunctions = getAllElements isFunction

getAllTrees :: Value -> [Object]
getAllTrees = getAllElements isTree

-- | Recursively parses the given object and all its descendants,
-- | building a list of (depth, [termName]) items.
getAllStatements
    :: Int
    -> Object
    -> Parser [DepthStatementData]
    -> Parser [DepthStatementData]
getAllStatements depth stmtNode parserSDs = do
    (curTermName :: Maybe Text) <- stmtNode .:? "term"
    let curTerm = [ (depth, [fromJust curTermName]) | isJust curTermName ]

    let nodeKeys = HM.keys stmtNode
    let (innerNodes :: [Object]) =
            rights $ map (parseEither (\k -> stmtNode .: k)) nodeKeys
    let (innerNodeArrays :: [[Object]]) =
            rights $ map (parseEither (\k -> stmtNode .: k)) nodeKeys
    let allInnerNodes = innerNodes ++ concat innerNodeArrays

    fmap concat
        $ sequence
        $ parserSDs
        : return curTerm
        : map
              (\nodeObject ->
                  getAllStatements (depth + 1) nodeObject $ return []
              )
              allInnerNodes

parseAllStatements :: [Object] -> [DepthStatementData]
parseAllStatements =
    concat . rights . map (parseEither (\n -> getAllStatements 1 n $ return []))

buildStatementsHashMap :: [Text] -> StatementsCountMap
buildStatementsHashMap = HM.fromListWith (+) . map (, 1)

multiplyHashMapByWeights :: StatementsCountMap -> StatementsCountMap
multiplyHashMapByWeights = HM.mapWithKey
    (\k v -> maybe (v * defaultWeight) (v *) $ HM.lookup k stmtWeights)

parseFunctionObject :: FilePath -> Language -> Object -> Parser FunctionData
parseFunctionObject path language o = do
    sourceSpan                    <- o .: "sourceSpan"
    [lineNumber, _]               <- sourceSpan .: "start"
    functionName                  <- o .: "functionName"
    name                          <- functionName .: "name"
    (parameters :: [Value])       <- o .: "functionParameters"
    body                          <- o .: "functionBody"
    (outerStatements :: [Object]) <- body .: "statements"

    let (allStatements :: [DepthStatementData]) =
            parseAllStatements outerStatements
    let (statementsDepthMap :: RawStatementsData) =
            IM.fromListWith (++) allStatements
    let (weightedStatementsData :: StatementsData) = IM.map
            (multiplyHashMapByWeights . buildStatementsHashMap)
            statementsDepthMap

    return $ FunctionData path
                          lineNumber
                          language
                          name
                          (length parameters)
                          weightedStatementsData
                          (length $ concat $ IM.elems statementsDepthMap)

parseTreeFunctions :: TreeData -> [Either String FunctionData]
parseTreeFunctions (TreeData rootNode treeFilePath treeLanguage) = map
    (parseEither $ parseFunctionObject treeFilePath treeLanguage)
    (getAllFunctions rootNode)

parseTreeObject :: Object -> Parser TreeData
parseTreeObject o = do
    (rootNode :: Value) <- o .: "tree"
    treeFilePath        <- o .: "path"
    treeLanguage        <- o .: "language"

    return $ TreeData rootNode treeFilePath treeLanguage

-- | Given `RawJSONFile`, tries to parse its `contents`.
-- | Returns either an error's description, or `ParsedFiles`.
parseRawJSONFile :: RawJSONFile -> Either String [FunctionData]
parseRawJSONFile (RawJSONFile _ contents) =
    let decodedJSON = eitherDecode $ encodeUtf8 $ pack contents
    in  case decodedJSON of
            (Left errorMessage) -> Left errorMessage
            (Right rootNode) ->
                Right
                    $ rights
                    $ concatMap parseTreeFunctions
                    $ rights
                    $ map (parseEither parseTreeObject)
                    $ getAllTrees rootNode
