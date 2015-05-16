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

test_animation = cachelessAnimation 1 (\t x → t+x)

testForNullAnimationWithCombiner combiner =
    case combiner [] of
        Animation{..} →
            testProperty "length 0" $ \(t::Int) (x::Int) →
                fst (animationFunction t x animationCache) == x

testMiddleOfSingleAnimationWithCombiner combiner =
    case combiner [test_animation] of
        Animation{..} →
            testProperty "middle test" $ \(x::Int) → do
                t ← choose (0,1)
                return $ fst (animationFunction t x animationCache) == t+x

testLeftClampingOfSingleAnimationWithCombiner combiner =
    case combiner [test_animation] of
        Animation{..} →
            testProperty "middle test" $ \(x::Int) → do
                t ← choose (-10,-1)
                return $ fst (animationFunction t x animationCache) == x

testRightClampingOfSingleAnimationWithCombiner combiner =
    case combiner [test_animation] of
        Animation{..} →
            testProperty "middle test" $ \(x::Int) → do
                t ← choose (2,10)
                return $ fst (animationFunction t x animationCache) == 1+x

tests =
    [testGroup "serial"
        [testForNullAnimationWithCombiner serial
        ,testGroup "length 1"
            [testMiddleOfSingleAnimationWithCombiner serial
            ,testLeftClampingOfSingleAnimationWithCombiner serial
            ,testRightClampingOfSingleAnimationWithCombiner serial
            ]
        ]
    ,testGroup "parallel"
        [testForNullAnimationWithCombiner parallel
        ,testGroup "length 1"
            [testMiddleOfSingleAnimationWithCombiner parallel
            ,testLeftClampingOfSingleAnimationWithCombiner parallel
            ,testRightClampingOfSingleAnimationWithCombiner parallel
            ]
        ]
    ]

main = defaultMain tests
