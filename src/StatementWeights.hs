{-# LANGUAGE OverloadedStrings #-}
                                  
module StatementWeights where

import           Data.Text                      ( Text )
import           Data.HashMap.Strict            ( HashMap
                                                , fromList )

defaultWeight = 0.1

stmtWeights :: HashMap Text Double
stmtWeights = fromList [ ("If", 1.0)

                       , ("Try", 1.0)
                       , ("Catch", 1.0)
                       , ("Finally", 1.0)

                       , ("For", 1.0)
                       , ("While", 1.0)
                       , ("Each", 1.0)
                       , ("Break", 1.0)
                       , ("Continue", 1.0)

                       , ("New", 1.0)

                       , ("Throw", 1.0)
                       , ("Return", 1.0)

                       , ("And", 0.7)
                       , ("Or", 0.7)
                       , ("Negate", 0.7)
                       , ("Not", 0.7)

                       , ("Plus", 0.7)
                       , ("Minus", 0.7)

                       , ("Typeof", 0.7)

                       , ("Assignment", 0.7)
                       , ("VariableDeclaration", 0.7)

                       , ("TextElement", 0.6)
                       , ("Regex", 0.6)
                       , ("KeyValue", 0.6)
                       , ("Array", 0.6)
                       , ("Function", 0.6)

                       , ("MemberAccess", 0.2)
                       , ("Call", 0.2)
                       ]
