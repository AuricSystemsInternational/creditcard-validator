-- Copyright (c) 2017-2018 Auric Systems International. All rights reserved.
-- License: 3-Clause BSD License. See accompanying LICENSE file.

module CreditCardValidator exposing  
    ( CardType(AM,DS,MC,VI,DC,UK)
    , CardTypeInfo
    , CreditCardNumber
    , ValidationResult
    , Range
    , validate
    , mopToCardType
    , validCardLength
    , findNumberInBetweenRange
    , allCardTypes
    , cardTypeByBinRange
    , numberInStartsWithRange
    , cardMatchesRange
    , filterByCardTypes
    , toCleanCCNumber
    )

{-| This library allows validation of a creditcard number potentially limiting by accepted card types. 
For example, if your business only accepts Mastercard and Visa you can limit the valid cards to those types.

# Definition
@docs CardType, CardTypeInfo, CreditCardNumber, ValidationResult, Range

# Validation
@docs validate

# Helpers
@docs mopToCardType, validCardLength, findNumberInBetweenRange, allCardTypes, cardTypeByBinRange, numberInStartsWithRange, cardMatchesRange, filterByCardTypes, toCleanCCNumber
-}

import Payment.CreditCard.Validation as LuhnValidation
import Regex exposing (regex)


{-| Card types supported by this library are: American Express (AM), Discover (DS), Diners Club (DC), Mastercard (MC) and VISA (VI).
-}
type CardType
    = AM
    | DS
    | MC
    | VI
    | DC
    | UK -- unknown card


{-| Represents possible ranges for a creditcard number. It can be a string or a range with starting and ending digits
-}
type Range
    = StartsWithRange String
    | InBetweenRange String String

{-| 
-}
type alias CardTypeInfo =
    { name : String
    , cardType : CardType
    , bins : List Range
    , valid_lengths : List Int
    }

{-| A card number can match multiple card types. 
Overall, a number is valid when it's valid according to [luhn algorithm](https://en.wikipedia.org/wiki/Luhn_algorithm) with valid length and type.  
-}
type alias ValidationResult =
    { card_types : List (Maybe CardTypeInfo) -- can match multiple card types
    , valid : Bool -- overall valid
    , luhn_valid : Bool
    , length_valid : Bool
    , cardTypeValid : Bool
    }

{-|-}
type alias CreditCardNumber =
    String


{-| List of all card type information
-}
allCardTypes : List CardTypeInfo
allCardTypes =
    [ { name = "amex"
      , cardType = AM
      , bins =
            [ StartsWithRange "34"
            , StartsWithRange "37"
            ]
      , valid_lengths = [ 15 ]
      }
    , { name = "discover"
      , cardType = DS
      , bins =
            [ StartsWithRange "6011"
            , InBetweenRange "622126" "622925"
            , InBetweenRange "644" "649"
            , StartsWithRange "65"
            ]
      , valid_lengths = [ 16, 19 ]
      }
    , { name = "mastercard"
      , cardType = MC
      , bins =
            [ InBetweenRange "2221" "2720"
            , InBetweenRange "51" "55"
            ]
      , valid_lengths = [ 16 ]
      }
    , { name = "visa"
      , cardType = VI
      , bins = [ StartsWithRange "4" ]
      , valid_lengths = [ 13, 14, 15, 16, 17, 18, 19 ]
      }
    , { name = "visa_electron"
      , cardType = VI
      , bins =
            [ StartsWithRange "4026"
            , StartsWithRange "417500"
            , StartsWithRange "4508"
            , StartsWithRange "4844"
            , StartsWithRange "4913"
            , StartsWithRange "4917"
            ]
      , valid_lengths = [ 16 ]
      }
    , { name = "diners_club_carte_blanche"
      , cardType = DC
      , bins =
            [ InBetweenRange "300" "305"
            ]
      , valid_lengths = [ 14 ]
      }
    , { name = "diners_club_international"
      , cardType = DC
      , bins =
            [ StartsWithRange "309"
            , StartsWithRange "36"
            , InBetweenRange "38" "39"
            ]
      , valid_lengths = [ 14 ]
      }
    ]


{-| Converts from strin MOP (Method of Payment) to CardType
-}
mopToCardType : String -> CardType
mopToCardType mop =
    case mop of
        "AM" ->
            AM

        "DS" ->
            DS

        "MC" ->
            MC

        "VI" ->
            VI

        "DC" ->
            DC

        _ ->
            UK

{-|-}
filterByCardTypes : List CardTypeInfo -> List CardType -> List CardTypeInfo
filterByCardTypes cardInfoList cardTypeList =
    let
        filteredCardTypeList =
            if List.length cardTypeList == 0 then
                cardInfoList
            else
                List.filter
                    (\el1 ->
                        let
                            present =
                                List.filter
                                    (\el2 -> el1.cardType == el2)
                                    cardTypeList
                        in
                        List.length present > 0
                    )
                    cardInfoList
    in
    filteredCardTypeList


{-|-}
toCleanCCNumber : String -> CreditCardNumber
toCleanCCNumber raw =
    raw
        |> Regex.replace Regex.All (regex "[ -]+") (always "")

{-|-}
numberInStartsWithRange : String -> String -> Bool
numberInStartsWithRange range ccNumber =
    String.startsWith range ccNumber

{-|-}
findNumberInBetweenRange : CreditCardNumber -> String -> String -> Bool
findNumberInBetweenRange ccNumber startRange endRange =
    let
        -- Range cannot start with 0
        startsWithZero =
            ccNumber |> String.startsWith "0"

        rangeCard =
            String.left (String.length startRange) ccNumber

        rangeCardInt =
            rangeCard |> String.toInt |> Result.withDefault -10

        startRangeInt =
            startRange |> String.toInt |> Result.withDefault -1

        endRangeInt =
            endRange |> String.toInt |> Result.withDefault -1
    in
    not startsWithZero && (rangeCardInt >= startRangeInt && rangeCardInt <= endRangeInt)

{-|-}
cardMatchesRange : CreditCardNumber -> List Range -> Bool
cardMatchesRange cardNumber rangeList =
    let
        matches =
            rangeList
                |> List.any
                    (\el ->
                        case el of
                            StartsWithRange range ->
                                numberInStartsWithRange range cardNumber

                            InBetweenRange startRange endRange ->
                                findNumberInBetweenRange cardNumber startRange endRange
                    )
    in
    matches

{-|-}
cardTypeByBinRange : CreditCardNumber -> List CardTypeInfo -> Maybe CardTypeInfo
cardTypeByBinRange ccNumber cardInfoList =
    -- find card info whose ranges contain a pattern that the card number matches
    cardInfoList
        |> List.filter
            (\cardInfo ->
                cardMatchesRange ccNumber cardInfo.bins
            )
        --   |> Debug.log "All matched card types: "
        |> List.head

{-| Finds whether card length is one of the valid lengths for the type
-}
validCardLength : CardTypeInfo -> CreditCardNumber -> Bool
validCardLength cardTypeInfo cleanedNumber =
    let
        cardLen =
            cleanedNumber |> String.length
    in
    cardTypeInfo.valid_lengths |> List.any (\len -> len == cardLen)

{-| Find whether a creditcard number is valid. You can limit what card types are eligible for the validation attempt.
-}
validate : String -> List CardType -> ValidationResult
validate rawNumber limitToCardTypes =
    -- 1. filter allCardTypes by limitToCardTypes
    -- 2. strip out extra chars from the rawNumber
    -- 3. find the card type whose range the card number matches
    -- 4. ensure length is one of the valid lengths
    -- 5. if still valid, check for luhn valid
    -- 6. prep ValidationResult
    let
        --
        filteredCardInfoList =
            filterByCardTypes allCardTypes limitToCardTypes

        -- |> Debug.log "FilteredCardInfoList: "
        cleanedCCNumber =
            rawNumber |> toCleanCCNumber

        -- find card type that has the matching bin range in the filtered card info list
        matchingCardType =
            cardTypeByBinRange cleanedCCNumber filteredCardInfoList

        cardTypeValid =
            case matchingCardType of
                Nothing ->
                    False

                Just ccType ->
                    True

        --   |> Debug.log "Matching card type: "
        lengthValid =
            case matchingCardType of
                Nothing ->
                    False

                Just matchedType ->
                    cleanedCCNumber
                        |> validCardLength matchedType

        ccNumInt =
            cleanedCCNumber
                |> String.toInt
                |> Result.withDefault -1

        luhnValid =
            LuhnValidation.isValid ccNumInt            
    in
    ValidationResult
        [ matchingCardType ]
        (luhnValid
            && lengthValid
            && cardTypeValid
        )
        luhnValid
        lengthValid
        cardTypeValid
