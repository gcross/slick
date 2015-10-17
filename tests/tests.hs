{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

import Control.Lens (_1, _2, _3, (.~), (%~), (^.), simple)
import Control.Monad (liftM)

import Data.Composition ((.**))
import Data.Function (on)
import Data.List (intercalate)

import Text.Printf (printf)

import System.IO.Unsafe (unsafePerformIO)

import System.Random (Random(randomR,random))

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck

import Slick.Animation
import Slick.Presentation
import Slick.Transition

instance Random Rational where
    randomR (lo,hi) g = (toRational double, g')
      where
        (double::Double, g') = randomR (fromRational lo, fromRational hi) g

    random g = (toRational double, g')
      where
        (double::Double, g') = random g

runAndReturnAnimation :: CombinationMode → s → PresentationM s α → Animation s
runAndReturnAnimation = (^. (p_animation_and_state . as_animation)) .** execPresentationIn

testAnimationsEqual :: (Eq s, Show s) ⇒ String → s → Animation s → Animation s → Test.Framework.Test
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
            in liftM
                (ioProperty . go initial_state correct_animation actual_animation [])
                (listOf (choose (-duration/6, duration*7/6)))
        ]
  where
    correct_animation_duration = durationOf correct_animation
    actual_animation_duration = durationOf actual_animation
    duration = correct_animation_duration

test_animation :: Animation Rational
test_animation = cachelessAnimation 1 (+)

test_animation2 :: Animation Rational
test_animation2 = cachelessAnimation 1 (+)

test_tuple_animation :: Animation (Rational,Rational)
test_tuple_animation = cachelessAnimation 1 (\t → _1 %~ (+t))

test_tuple_animation2 :: Animation (Rational,Rational)
test_tuple_animation2 = cachelessAnimation 1 (\t → _2 %~ (*t))

testTransition :: String → (Rational → Rational) → Bool → Test.Framework.Test
testTransition label transition check_bounds =
    testGroup label $
        [testCase "Correct left endpoint" $
            assertBool
                ("transition 0 (" ++ show (transition 0) ++ ") != 0")
                (abs (transition 0) < 1e-7)
        ,testCase "Correct right endpoint" $
            assertBool
                ("transition 1 (" ++ show (transition 1) ++ ") != 1")
                (abs (transition 1 - 1) < 1e-7)
        ] ++
        [testProperty "Within bounds" $ do
            t ← choose (-0.25,1.25)
            let tt = transition t
                is_valid =
                    if | t <= 0 → tt == 0
                        | t >= 1 → tt == 1
                        | otherwise → tt >= 0 && tt <= 1
            if is_valid
                then return True
                else error $ "At time " ++ show t ++ " transition had value " ++ show tt
        | check_bounds
        ]

tests =
    [testGroup "Slick.Animation"
        [testGroup "serial"
            [testAnimationsEqual "length 0" 0 (null_animation :: Animation Rational) (serial [])
            ,testAnimationsEqual "length 1" 0 test_animation (serial [test_animation])
            ,testGroup "length 2"
                [testAnimationsEqual "function of time only" 0
                    (statelessAnimation 3 $ \t → if t < 1 then t else 1-(t-1)/2)
                    (serial
                        [cachelessAnimation 1 const
                        ,cachelessAnimation 2 (\t _ → 1-t/2)
                        ]
                     :: Animation Rational
                    )
                ,testAnimationsEqual "2-tuple" (0::Rational,0::Rational)
                    (cachelessAnimation 2 $
                        \t → if t < 1 then _1 .~ t else  (_1 .~ 1) . (_2 .~ (t-1)^2)
                    )
                    (serial
                        [cachelessAnimation 1 (\t → _1 .~ t)
                        ,cachelessAnimation 1 (\t → _2 .~ t^2)
                        ]
                    )
                ]
            ,testGroup "length 3"
                [testAnimationsEqual "function of time only" (0::Rational)
                    (statelessAnimation (6::Rational) $ \t →
                        if  | t < 1 → t
                            | t >= 1 && t < 4 → 1-(t-1)/3
                            | otherwise → (t-4)^2
                    )
                    (serial
                        [cachelessAnimation (1::Rational) const
                        ,cachelessAnimation (3::Rational) (\t _ → 1-t/3)
                        ,cachelessAnimation (2::Rational) (\t _ → t^2)
                        ]
                    )
                ,testGroup "3-tuple"
                    [testAnimationsEqual "3-tuple" (0::Rational,0::Rational,0::Rational)
                        (cachelessAnimation (6::Rational) $
                            \t → if
                                | t < 2 → _1 .~t
                                | t >= 2 && t < 3 → (_1 .~ 2) . (_2 .~ (t-2)^2)
                                | otherwise → (_1 .~ 2) . (_2 .~ 1) . (_3 .~ (t-3)^3)
                        )
                        (serial
                            [cachelessAnimation (2::Rational) (\t → _1 .~ t^1)
                            ,cachelessAnimation (1::Rational) (\t → _2 .~ t^2)
                            ,cachelessAnimation (3::Rational) (\t → _3 .~ t^3)
                            ]
                        )
                    ,testAnimationsEqual "zero length" (0::Rational,0::Rational,0::Rational)
                        (cachelessAnimation (5::Rational) $ \t →
                            if t < 2
                            then _1 .~ t
                            else (_1 .~ 2) . (_2 .~ 1) . (_3 .~ (t-2)^3)
                        )
                        (serial
                            [cachelessAnimation (2::Rational) (\t → _1 .~ t)
                            ,cachelessAnimation (0::Rational) (\_ → _2 .~ 1)
                            ,cachelessAnimation (3::Rational) (\t → _3 .~ t^3)
                            ]
                        )
                    ]
                ]
            ]
        ,testGroup "parallel"
            [testAnimationsEqual "length 0" 0 (null_animation :: Animation Rational) (parallel [])
            ,testAnimationsEqual "length 1" 0 test_animation (serial [test_animation])
            ,testAnimationsEqual "length 2" (0::Rational,0::Rational)
                (cachelessAnimation 2 $ \t → (_1 .~ t) . (if t <= 1 then _2 .~ t*t else _2 .~ 1))
                (parallel
                    [cachelessAnimation 2  (\t → _1 .~ t^1)
                    ,cachelessAnimation 1  (\t → _2 .~ t^2)
                    ]
                )
            ,testAnimationsEqual ",length 3" (0::Rational,0::Rational,0::Rational)
                (cachelessAnimation 2 $ \t →
                    (if t <= 1 then _1 .~ t else _1 .~ 1)
                    .
                    (_2 .~ 1)
                    .
                    (_3 .~ t^3)
                )
                (parallel
                    [cachelessAnimation 1  (\t → _1 .~ t)
                    ,cachelessAnimation 0  (\t → _2 .~ 1)
                    ,cachelessAnimation 2  (\t → _3 .~ t^3)
                    ]
                )
            ]
        ]
    ,testGroup "Slick.Presentation"
        [testGroup "animation equivalence"
            [testGroup "return ()"
                [testCase "serial" $
                    durationOf (runAndReturnAnimation Serial () (return ())) @?= 0
                ,testCase "parallel" $
                    durationOf (runAndReturnAnimation Parallel () (return ())) @?= 0
                ]
            ,testGroup "single animation"
                [testAnimationsEqual "serial" (0::Rational)
                    test_animation
                    (runAndReturnAnimation Serial (0::Rational) (appendAnimation test_animation))
                ,testAnimationsEqual "parallel" (0::Rational)
                    test_animation
                    (runAndReturnAnimation Parallel (0::Rational) (appendAnimation test_animation) )
                ]
            ,testGroup "two animations"
                [testAnimationsEqual "serial" (0::Rational)
                    (serial [test_animation,test_animation2])
                    (runAndReturnAnimation Serial (0::Rational) $ do
                        appendAnimation test_animation
                        appendAnimation test_animation2
                    )
                ,testAnimationsEqual "parallel" (0::Rational,0::Rational)
                    (parallel [test_tuple_animation,test_tuple_animation2])
                    (runAndReturnAnimation Parallel (0::Rational,0::Rational) $ do
                        appendAnimation test_tuple_animation
                        appendAnimation test_tuple_animation2
                    )
                ]
            ]
        ]
    ,testGroup "Slick.Transition"
        [testGroup "transitions"
            [testTransition "linear" linear_easing True
            ,testTransition "smooth" smooth_easing True
            ,testTransition "accelerate" acceleration_easing True
            ,testTransition "decelerate" deceleration_easing True
            ]
        ,testAnimationsEqual "linearFromTo" (0::Rational)
            (statelessAnimation (2::Rational) id)
            (runAndReturnAnimation Serial (0::Rational) $ linearFromTo simple (2::Rational) (0::Rational) (2::Rational))
        ]
    ]

main = defaultMain tests
