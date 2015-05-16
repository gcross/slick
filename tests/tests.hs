{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

import Control.Lens (_1, _2, _3, (.~))

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck

import Slick.Animation

testForNullAnimationWithCombiner combiner =
    case combiner [] of
        Animation{..} →
            testProperty "length 0" $ \(t::Int) (x::Int) →
                fst (animationFunction t x animationCache) == x

testLength1Group combiner = testGroup "length 1" $
    [testCase "correct duration" $
        durationOf (combiner [test_animation]) @=? 1
    ,testProperty "correct interior" $ \x → do
        t ← choose (0,1)
        return $ runAnimationState t x (combiner [test_animation]) == t+x
    ,testProperty "correctly clamps the left" $ \x → do
        t ← choose (-10,-1)
        return $ runAnimationState t x (combiner [test_animation]) == x
    ,testProperty "correctly clamps the right" $ \x → do
        t ← choose (2,10)
        return $ runAnimationState t x (combiner [test_animation]) == 1+x
    ]
  where
    test_animation :: Animation Int Int
    test_animation = cachelessAnimation 1 (\t x → t+x)

tests =
    [testGroup "serial"
        [testForNullAnimationWithCombiner serial
        ,testLength1Group serial
        ]
    ,testGroup "parallel"
        [testForNullAnimationWithCombiner parallel
        ,testLength1Group parallel
        ,testGroup "length 2" $
            [testCase "correct duration" $
                durationOf (parallel [statelessAnimation 1 id, statelessAnimation 2 id]) @?= 2
            ,testProperty "correct behavior" $ do
                t ← choose (0::Int, 2::Int)
                let animation =
                        (parallel
                            [cachelessAnimation (2::Int) (\t → (_1 .~ t))
                            ,cachelessAnimation (1::Int) (\t → (_2 .~ t*t))
                            ]
                        )
                return $ case runAnimation t (undefined::Int,undefined::Int) animation of
                    ((x,y), _)
                      | t >= 2 → (x == t) && (y == 1)
                      | otherwise → (x == t) && (y == t*t)
            ]
        ,testGroup "length 3" $
            [testCase "correct duration" $
                durationOf (parallel [statelessAnimation 1 id, statelessAnimation 2 id, statelessAnimation 1 id]) @?= 2
            ,testProperty "correct behavior" $ do
                t ← choose (0::Int, 2::Int)
                let animation =
                        (parallel
                            [cachelessAnimation (1::Int) (\t → (_1 .~ t))
                            ,cachelessAnimation (2::Int) (\t → (_2 .~ t*t))
                            ,cachelessAnimation (1::Int) (\t → (_3 .~ t*t*t))
                            ]
                        )
                return $ case runAnimation t (undefined::Int,undefined::Int,undefined::Int) animation of
                    ((x,y,z), _)
                      | t >= 2 → (x == 1) && (y == t*t) && (z == 1)
                      | otherwise → (x == t) && (y == t*t) && (z == t*t*t)
            ]
        ]
    ]

main = defaultMain tests
