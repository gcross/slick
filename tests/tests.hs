{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

import Control.Lens (_1, _2, _3, (.~))

import Data.List (intercalate)

import Text.Printf (printf)

import System.IO.Unsafe (unsafePerformIO)

import System.Random (Random)

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

checkAnimationCorrectness ::
    (Num α, Random α, Show α, Show β) ⇒
    β →
    (α → β → Bool) →
    Animation α β →
    Property
checkAnimationCorrectness initial_state correctState animation = property $
    listOf (choose (0, durationOf animation))
    >>=
    return . ioProperty . go initial_state animation []
  where
    go _ _ _ [] = return True
    go state animation past_reversed (t:future) = do
        let times = t:reverse past_reversed
            message = printf "After times %s got incorrect state %s."
                (intercalate ", " (map show times))
                (show new_state)
            (new_state, new_animation) = runAnimation t state animation
        assertBool message (correctState t new_state)
        go new_state new_animation (t:past_reversed) future

tests =
    [testGroup "serial"
        [testForNullAnimationWithCombiner serial
        ,testLength1Group serial
        ,testGroup "length 2" $
            [testCase "correct duration" $
                durationOf
                    (serial
                        [statelessAnimation 1 id
                        ,statelessAnimation 2 id
                        ]
                    )
                @?= 3
            ,testGroup "correct behavior"
                [testProperty "function of time only" $
                    checkAnimationCorrectness
                        (0::Float)
                        (\t x → if t < 1 then x == t else x == 1-(t-1)/2)
                        (serial
                            [cachelessAnimation (1::Float) (\t _ → t)
                            ,cachelessAnimation (2::Float) (\t _ → 1-t/2)
                            ]
                        )
                ,testProperty "2-tuple" $
                    checkAnimationCorrectness
                        (0::Float,0::Float)
                        (\t (x,y) → if t < 1 then x == t else x == 1 && y == (t-1)**2)
                        (serial
                            [cachelessAnimation (1::Float) (\t → (_1 .~ t))
                            ,cachelessAnimation (1::Float) (\t → (_2 .~ t**2))
                            ]
                        )
                ]
            ]
        ,testGroup "length 3" $
            [testCase "correct duration" $
                durationOf
                    (serial
                        [statelessAnimation 1 id
                        ,statelessAnimation 3 id
                        ,statelessAnimation 2 id
                        ]
                    )
                @?= 6
            ,testGroup "correct behavior"
                [testProperty "function of time only" $
                    checkAnimationCorrectness
                        (0::Float)
                        (\t x → if
                            | t < 1 → x == t
                            | t >= 1 && t < 4 → x == 1-(t-1)/3
                            | otherwise → x == (t-4)**2
                        )
                        (serial
                            [cachelessAnimation (1::Float) (\t _ → t)
                            ,cachelessAnimation (3::Float) (\t _ → 1-t/3)
                            ,cachelessAnimation (2::Float) (\t _ → t**2)
                            ]
                        )
                ,testProperty "3-tuple" $
                    checkAnimationCorrectness
                        (0::Float,0::Float,0::Float)
                        (\t (x,y,z) → if
                            | t < 2 → x == t
                            | t >= 2 && t < 3 → x == 2 && y == (t-2)**2
                            | otherwise → x == 2 && y == 1 && z == (t-3)**3
                        )
                        (serial
                            [cachelessAnimation (2::Float) (\t → (_1 .~ t))
                            ,cachelessAnimation (1::Float) (\t → (_2 .~ t**2))
                            ,cachelessAnimation (3::Float) (\t → (_3 .~ t**3))
                            ]
                        )
                ]
            ]
        ]
    ,testGroup "parallel"
        [testForNullAnimationWithCombiner parallel
        ,testLength1Group parallel
        ,testGroup "length 2" $
            [testCase "correct duration" $
                durationOf
                    (parallel
                        [statelessAnimation 1 id
                        ,statelessAnimation 2 id
                        ]
                    )
                @?= 2
            ,testProperty "correct behavior" $
                checkAnimationCorrectness
                    (0::Float,0::Float)
                    (\t (x,y) → x == t && if t <= 1 then y == t*t else y == 1)
                    (parallel
                        [cachelessAnimation 2  (\t → (_1 .~ t))
                        ,cachelessAnimation 1  (\t → (_2 .~ t*t))
                        ]
                    )
            ]
        ,testGroup "length 3" $
            [testCase "correct duration" $
                durationOf
                    (parallel
                        [statelessAnimation 1 id
                        ,statelessAnimation 3 id
                        ,statelessAnimation 2 id
                        ]
                    )
                @?= 3
            ,testProperty "correct behavior" $
                checkAnimationCorrectness
                    (0::Float,0::Float,0::Float)
                    (\t (x,y,z) →
                        and [if t <= 1 then x == t else x == 1
                            ,y == t**2
                            ,if t <= 2 then z == t**3 else z == 8
                            ])
                    (parallel
                        [cachelessAnimation 1  (\t → (_1 .~ t))
                        ,cachelessAnimation 3  (\t → (_2 .~ t**2))
                        ,cachelessAnimation 2  (\t → (_3 .~ t**3))
                        ]
                    )
            ]
        ]
    ]

main = defaultMain tests
