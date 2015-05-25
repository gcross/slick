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
import Slick.AnimationM
import Slick.Transition

testAnimationsEqual :: (Eq t, Num t, Random t, Show t, Eq s, Show s) ⇒ String → s → Animation t s → Animation t s → Test.Framework.Test
testAnimationsEqual label initial_state correct_animation actual_animation =
    testGroup label
        [testCase "same duration" $
            durationOf correct_animation @=? durationOf actual_animation
        ,testProperty "same behavior" . property $
            let go _ _ _ _ [] = return True
                go state previous_correct_animation previous_actual_animation past_reversed (t:future) = do
                    let times = t:reverse past_reversed
                        message = printf "After times %s got incorrect state %s != %s."
                            (intercalate ", " (map show times))
                            (show new_actual_state)
                            (show new_correct_state)
                        (new_correct_state, new_correct_animation) = runAnimation correct_animation t state
                        (new_actual_state, new_actual_animation) = runAnimation correct_animation t state
                    assertEqual message new_actual_state new_correct_state
                    go new_correct_state new_correct_animation new_actual_animation (t:past_reversed) future
            in listOf (choose (0, durationOf correct_animation))
               >>=
               return . ioProperty . go initial_state correct_animation actual_animation []
        ]

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
            (new_state, new_animation) = runAnimation animation t state
        assertBool message (correctState t new_state)
        go new_state new_animation (t:past_reversed) future

test_animation :: Animation Int Int
test_animation = cachelessAnimation 1 (\t x → t+x)

tests =
    [testGroup "Slick.Animation"
        [testGroup "serial"
            [testAnimationsEqual "length 0" 0 (null_animation :: Animation Int Int) (serial [])
            ,testAnimationsEqual "length 1" 0 test_animation (serial [test_animation])
            ,testGroup "length 2" $
                [testGroup "function of time only" $
                    let animation :: Animation Float Float
                        animation = serial
                            [cachelessAnimation 1 (\t _ → t)
                            ,cachelessAnimation 2 (\t _ → 1-t/2)
                            ]
                    in  [testCase "correct duration" $ durationOf animation @?= 3
                        ,testProperty "correct behavior" $
                             checkAnimationCorrectness
                                (0)
                                (\t x → if t < 1 then x == t else x == 1-(t-1)/2)
                                animation
                        ]
                ,testGroup "2-tuple" $
                    let animation :: Animation Float (Float,Float)
                        animation = serial
                            [cachelessAnimation 1 (\t → (_1 .~ t))
                            ,cachelessAnimation 1 (\t → (_2 .~ t**2))
                            ]
                     in [testCase "correct duration" $ durationOf animation @?= 2
                        ,testProperty "correct behavior" $
                            checkAnimationCorrectness
                                (0,0)
                                (\t (x,y) → if t < 1 then x == t else x == 1 && y == (t-1)**2)
                                animation
                        ]
                ]
            ,testGroup "length 3" $
                [testGroup "function of time only" $
                    let animation :: Animation Float Float
                        animation = serial
                            [cachelessAnimation (1::Float) (\t _ → t)
                            ,cachelessAnimation (3::Float) (\t _ → 1-t/3)
                            ,cachelessAnimation (2::Float) (\t _ → t**2)
                            ]
                    in [testCase "correct duration" $ durationOf animation @?= 6
                       ,testProperty "function of time only" $
                            checkAnimationCorrectness
                                (0::Float)
                                (\t x → if
                                    | t < 1 → x == t
                                    | t >= 1 && t < 4 → x == 1-(t-1)/3
                                    | otherwise → x == (t-4)**2
                                )
                                animation
                       ]
                ,testGroup "3-tuple" $
                    [testGroup "finite length" $
                        let animation :: Animation Float (Float,Float,Float)
                            animation = serial
                                [cachelessAnimation (2::Float) (\t → (_1 .~ t))
                                ,cachelessAnimation (1::Float) (\t → (_2 .~ t**2))
                                ,cachelessAnimation (3::Float) (\t → (_3 .~ t**3))
                                ]
                        in [testCase "correct duration" $ durationOf animation @?= 6
                           ,testProperty "finite length" $
                                checkAnimationCorrectness
                                    (0::Float,0::Float,0::Float)
                                    (\t (x,y,z) → if
                                        | t < 2 → x == t
                                        | t >= 2 && t < 3 → x == 2 && y == (t-2)**2
                                        | otherwise → x == 2 && y == 1 && z == (t-3)**3
                                    )
                                    animation
                           ]
                    ,testGroup "zero length" $
                        let animation :: Animation Float (Float,Float,Float)
                            animation = serial
                                [cachelessAnimation (2::Float) (\t → (_1 .~ t))
                                ,cachelessAnimation (0::Float) (\_ → (_2 .~ 1))
                                ,cachelessAnimation (3::Float) (\t → (_3 .~ t**3))
                                ]
                        in [testCase "correct duration" $ durationOf animation @?= 5
                           ,testProperty "zero length" $
                                checkAnimationCorrectness
                                    (0::Float,0::Float,0::Float)
                                    (\t (x,y,z) →
                                        if t < 2
                                            then x == t
                                            else x == 2 && y == 1 && z == (t-2)**3
                                    )
                                    animation
                           ]
                    ]
                ]
            ]
        ,testGroup "parallel"
            [testAnimationsEqual "length 0" 0 (null_animation :: Animation Int Int) (parallel [])
            ,testAnimationsEqual "length 1" 0 test_animation (serial [test_animation])
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
                                ,y == 1
                                ,if t <= 2 then z == t**3 else z == 8
                                ])
                        (parallel
                            [cachelessAnimation 1  (\t → (_1 .~ t))
                            ,cachelessAnimation 0  (\t → (_2 .~ 1))
                            ,cachelessAnimation 2  (\t → (_3 .~ t**3))
                            ]
                        )
                ]
            ]
        ]
    ,testGroup "Slick.AnimationM"
        [testGroup "runAnimationMIn"
            [testGroup "return ()"
                [testCase "serial" $
                    durationOf (runAnimationMIn Serial (return ()) ()) @?= 0
                ,testCase "parallel" $
                    durationOf (runAnimationMIn Parallel (return ()) ()) @?= 0
                ]
            ,testGroup "single animation"
                [testCase "serial" $
                    durationOf (runAnimationMIn Serial (return ()) ()) @?= 0
                ,testCase "parallel" $
                    durationOf (runAnimationMIn Parallel (return ()) ()) @?= 0
                ]
            ]
        ]
    ]
main = defaultMain tests
