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

testForNullInactiveAnimation animation =
    case animation of
        InactiveAnimation Animation{..} →
            testProperty "length 0" $ \(t::Int) (x::Int) →
                fst (animationFunction t x animationInitialCache) == x

tests =
    [testGroup "serial"
        [testForNullInactiveAnimation . serial $ []
        ]
    ,testGroup "parallel"
        [testForNullInactiveAnimation . parallel $ []
        ]
    ]

main = defaultMain tests
