{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck

import Slick.Animation

testForNullAnimation animation =
    case animation of
        Animation{..} →
            testProperty "length 0" $ \(t::Int) (x::Int) →
                fst (animationFunction t x animationCache) == x

tests =
    [testGroup "serial"
        [testForNullAnimation . serial $ []
        ]
    ,testGroup "parallel"
        [testForNullAnimation . parallel $ []
        ]
    ]

main = defaultMain tests
