module FileProcessor
    ( RawJSONFile(RawJSONFile)
    , readJSONFile
    )
where

data RawJSONFile = RawJSONFile FilePath String

-- | Read a file given its path, return its contents along with its path.
readJSONFile :: FilePath -> IO RawJSONFile
readJSONFile filePath = RawJSONFile filePath <$> readFile filePath
