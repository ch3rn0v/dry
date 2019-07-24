module Analyser
    ( SourceFile(..)
    )
where

data SourceFile = SourceFile FilePath String deriving (Eq, Show)
