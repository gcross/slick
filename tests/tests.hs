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

testSingletonGroup combiner = testGroup "length 1" $
    [testCase "correct duration" $
        durationOf (combiner [test_animation]) @=? 1
    ,testProperty "correct interior" $ \(x::Int) → do
        t ← choose (0,1)
        return $
            case combiner [test_animation] of
                Animation{..} → fst (animationFunction t x animationCache) == t+x
    ,testProperty "correctly clamps the left" $ \(x::Int) → do
        t ← choose (-10,-1)
        return $
            case combiner [test_animation] of
                Animation{..} → fst (animationFunction t x animationCache) == x
    ,testProperty "correctly clamps the right" $ \(x::Int) → do
        t ← choose (2,10)
        return $
            case combiner [test_animation] of
                Animation{..} → fst (animationFunction t x animationCache) == 1+x
    ]

tests =
    [testGroup "serial"
        [testForNullAnimationWithCombiner serial
        ,testSingletonGroup serial
        ]
    ,testGroup "parallel"
        [testForNullAnimationWithCombiner parallel
        ,testSingletonGroup parallel
        ]
    ]

main = defaultMain tests
