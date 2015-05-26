{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

import Control.Lens (_1, _2, _3, (.~), (%~), simple)

import Data.List (intercalate)
import Data.Function (on)

import Text.Printf (printf)

import System.IO.Unsafe (unsafePerformIO)

import System.Random (Random)

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck

import Slick.Animation
import Slick.Presentation
import Slick.Transition

testAnimationsEqual :: (Timelike t, Random t, Show t, Eq s, Show s) ⇒ String → s → Animation t s → Animation t s → Test.Framework.Test
testAnimationsEqual label initial_state correct_animation actual_animation =
    testGroup label
        [testCase "same duration" $
            correct_animation_duration @=? actual_animation_duration
        ,testCase "same left endpoint" $
             ((@=?) `on` fst)
                (runAnimation correct_animation 0 initial_state)
                (runAnimation actual_animation 0 initial_state)
        ,testCase "same right endpoint" $
             ((@=?) `on` fst)
                (runAnimation correct_animation duration initial_state)
                (runAnimation actual_animation duration initial_state)
        ,testProperty "same behavior" . property $
            let go _ _ _ _ [] = return True
                go state previous_correct_animation previous_actual_animation past_reversed (t:future) = do
                    let times = t:reverse past_reversed
                        message = printf "After times %s got incorrect state %s != %s."
                            (intercalate ", " (map show times))
                            (show new_actual_state)
                            (show new_correct_state)
                        (new_correct_state, new_correct_animation) = runAnimation correct_animation t state
                        (new_actual_state, new_actual_animation) = runAnimation actual_animation t state
                    assertEqual message new_actual_state new_correct_state
                    go new_correct_state new_correct_animation new_actual_animation (t:past_reversed) future
            in listOf (choose (-duration/6, duration*7/6))
               >>=
               return . ioProperty . go initial_state correct_animation actual_animation []
        ]
  where
    correct_animation_duration = durationOf correct_animation
    actual_animation_duration = durationOf actual_animation
    duration = correct_animation_duration

test_animation :: Animation Float Float
test_animation = cachelessAnimation 1 (\t x → t+x)

test_animation2 :: Animation Float Float
test_animation2 = cachelessAnimation 1 (\t x → t*x)

test_tuple_animation :: Animation Float (Float,Float)
test_tuple_animation = cachelessAnimation 1 (\t → _1 %~ (+t))

test_tuple_animation2 :: Animation Float (Float,Float)
test_tuple_animation2 = cachelessAnimation 1 (\t → _2 %~ (*t))

tests =
    [testGroup "Slick.Animation"
        [testGroup "serial"
            [testAnimationsEqual "length 0" 0 (null_animation :: Animation Float Float) (serial [])
            ,testAnimationsEqual "length 1" 0 test_animation (serial [test_animation])
            ,testGroup "length 2" $
                [testAnimationsEqual "function of time only" 0
                    (statelessAnimation 3 $ \t → if t < 1 then t else 1-(t-1)/2)
                    (serial
                        [cachelessAnimation 1 (\t _ → t)
                        ,cachelessAnimation 2 (\t _ → 1-t/2)
                        ]
                     :: Animation Float Float
                    )
                ,testAnimationsEqual "2-tuple" (0::Float,0::Float)
                    (cachelessAnimation 2 $
                        \t → if t < 1 then _1 .~ t else  (_1 .~ 1) . (_2 .~ (t-1)**2)
                    )
                    (serial
                        [cachelessAnimation 1 (\t → (_1 .~ t))
                        ,cachelessAnimation 1 (\t → (_2 .~ t**2))
                        ]
                    )
                ]
            ,testGroup "length 3" $
                [testAnimationsEqual "function of time only" (0::Float)
                    (statelessAnimation (6::Float) $ \t →
                        if  | t < 1 → t
                            | t >= 1 && t < 4 → 1-(t-1)/3
                            | otherwise → (t-4)**2
                    )
                    (serial
                        [cachelessAnimation (1::Float) (\t _ → t)
                        ,cachelessAnimation (3::Float) (\t _ → 1-t/3)
                        ,cachelessAnimation (2::Float) (\t _ → t**2)
                        ]
                    )
                ,testGroup "3-tuple" $
                    [testAnimationsEqual "3-tuple" (0::Float,0::Float,0::Float)
                        (cachelessAnimation (6::Float) $
                            \t → if
                                | t < 2 → _1 .~t
                                | t >= 2 && t < 3 → (_1 .~ 2) . (_2 .~ (t-2)**2)
                                | otherwise → (_1 .~ 2) . (_2 .~ 1) . (_3 .~ (t-3)**3)
                        )
                        (serial
                            [cachelessAnimation (2::Float) (\t → (_1 .~ t))
                            ,cachelessAnimation (1::Float) (\t → (_2 .~ t**2))
                            ,cachelessAnimation (3::Float) (\t → (_3 .~ t**3))
                            ]
                        )
                    ,testAnimationsEqual "zero length" (0::Float,0::Float,0::Float)
                        (cachelessAnimation (5::Float) $ \t →
                            if t < 2
                            then _1 .~ t
                            else (_1 .~ 2) . (_2 .~ 1) . (_3 .~ (t-2)**3)
                        )
                        (serial
                            [cachelessAnimation (2::Float) (\t → (_1 .~ t))
                            ,cachelessAnimation (0::Float) (\_ → (_2 .~ 1))
                            ,cachelessAnimation (3::Float) (\t → (_3 .~ t**3))
                            ]
                        )
                    ]
                ]
            ]
        ,testGroup "parallel"
            [testAnimationsEqual "length 0" 0 (null_animation :: Animation Float Float) (parallel [])
            ,testAnimationsEqual "length 1" 0 test_animation (serial [test_animation])
            ,testAnimationsEqual "length 2" (0::Float,0::Float)
                (cachelessAnimation 2 $ \t → (_1 .~ t) . (if t <= 1 then _2 .~ t*t else _2 .~ 1))
                (parallel
                    [cachelessAnimation 2  (\t → (_1 .~ t))
                    ,cachelessAnimation 1  (\t → (_2 .~ t*t))
                    ]
                )
            ,testAnimationsEqual ",length 3" (0::Float,0::Float,0::Float)
                (cachelessAnimation 2 $ \t →
                    (if t <= 1 then _1 .~ t else _1 .~ 1)
                    .
                    (_2 .~ 1)
                    .
                    (_3 .~ t**3)
                )
                (parallel
                    [cachelessAnimation 1  (\t → (_1 .~ t))
                    ,cachelessAnimation 0  (\t → (_2 .~ 1))
                    ,cachelessAnimation 2  (\t → (_3 .~ t**3))
                    ]
                )
            ]
        ]
    ,testGroup "Slick.Presentation"
        [testGroup "runPresentationIn"
            [testGroup "return ()"
                [testCase "serial" $
                    durationOf (runPresentationIn Serial () (return ())) @?= 0
                ,testCase "parallel" $
                    durationOf (runPresentationIn Parallel () (return ())) @?= 0
                ]
            ,testGroup "single animation"
                [testAnimationsEqual "serial" (0::Float)
                    test_animation
                    (runPresentationIn Serial (0::Float) (appendAnimation test_animation))
                ,testAnimationsEqual "parallel" (0::Float)
                    test_animation
                    (runPresentationIn Parallel (0::Float) (appendAnimation test_animation) )
                ]
            ,testGroup "two animations"
                [testAnimationsEqual "serial" (0::Float)
                    (serial [test_animation,test_animation2])
                    (runPresentationIn Serial (0::Float) $ do
                        appendAnimation test_animation
                        appendAnimation test_animation2
                    )
                ,testAnimationsEqual "parallel" (0::Float,0::Float)
                    (parallel [test_tuple_animation,test_tuple_animation2])
                    (runPresentationIn Parallel (0::Float,0::Float) $ do
                        appendAnimation test_tuple_animation
                        appendAnimation test_tuple_animation2
                    )
                ]
            ]
        ]
    ,testGroup "Slick.Transition"
        [testAnimationsEqual "linearFromTo" (0::Float)
            (statelessAnimation (2::Float) $ \t → t)
            (runPresentationIn Serial (0::Float) $ linearFromTo simple (2::Float) (0::Float) (2::Float))
        ]
    ]

main = defaultMain tests
