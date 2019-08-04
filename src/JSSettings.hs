module JSSettings where

import           Data.Map.Strict                ( Map
                                                , fromList
                                                )

constructorWeightsMap :: Map String Double
constructorWeightsMap = fromList
    [ ("JSAssignStatement"  , 0.7)
    , ("JSDoWhile"          , 1.0)
    , ("JSWhile"            , 1.0)
    , ("JSFor"              , 1.0)
    , ("JSForIn"            , 1.0)
    , ("JSForVar"           , 1.0)
    , ("JSForVarIn"         , 1.0)
    , ("JSForLet"           , 1.0)
    , ("JSForLetIn"         , 1.0)
    , ("JSForLetOf"         , 1.0)
    , ("JSForOf"            , 1.0)
    , ("JSForVarOf"         , 1.0)
    , ("JSIf"               , 1.0)
    , ("JSIfElse"           , 1.0)
    , ("JSSwitch"           , 1.0)
    , ("JSThrow"            , 1.0)
    , ("JSTry"              , 1.0)
    , ("JSExpressionTernary", 0.8)
    , ("JSUnaryOpDecr"      , 0.7)
    , ("JSUnaryOpDelete"    , 0.7)
    , ("JSUnaryOpIncr"      , 0.7)
    , ("JSUnaryOpMinus"     , 0.7)
    , ("JSUnaryOpNot"       , 0.7)
    , ("JSUnaryOpPlus"      , 0.7)
    , ("JSUnaryOpTilde"     , 0.7)
    , ("JSUnaryOpTypeof"    , 0.7)
    , ("JSUnaryOpVoid"      , 0.7)
    ]

defaultConstructorWeight = 0.3
