module LuhnValidationTests exposing (..)

{-| LuhnValidation module is adapted directly from <https://github.com/ersocon/creditcard-validation> to enable support for Elm 0.19
The original package has not been updated to Elm 0.19 yet.
-}

import Expect exposing (..)
import LuhnValidation as CCV
import Test exposing (..)


all : Test
all =
    describe "Credit Card Validation Test Suite"
        [ describe "invalid"
            [ test "check incorrect numbers as negative" <|
                \() ->
                    Expect.equal False (CCV.isValid invalidCreditCardNumber)
            ]
        , describe "valid" <|
            List.map
                (\validCreditCardNumber ->
                    test ("check correct number is " ++ toString validCreditCardNumber) <|
                        \() ->
                            Expect.equal True <| CCV.isValid validCreditCardNumber
                )
                validCreditCardNumbers
        ]


validCreditCardNumbers : List String
validCreditCardNumbers =
    [ "4716292509375978"
    , "4556737586899855"
    , "3531168289082072776"
    , "5165931105525576"
    , "6011614571497239441"
    , "374419708153838"
    , "5402037708659565"
    , "6759852307247696"
    , "4913039979863954"
    ]


invalidCreditCardNumber : String
invalidCreditCardNumber =
    "4716292509375979"
