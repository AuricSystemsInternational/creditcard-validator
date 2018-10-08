module CreditCardValidatorTests exposing (TestCard, all, allTestCards, cardInfoForType, sample, unknownCardLength, unknownCardType, validateTest)

--import Fuzz exposing (Fuzzer, int, list, string)

import CreditCardValidator as CCV exposing (..)
import Expect exposing (Expectation)
import Test exposing (..)


unknownCardLength : Int
unknownCardLength =
    1000


unknownCardType : CCV.CardTypeInfo
unknownCardType =
    { name = "Unknown"
    , cardType = CCV.UK
    , bins = []
    , valid_lengths = [ unknownCardLength ]
    }


cardInfoForType : CCV.CardType -> Maybe CCV.CardTypeInfo
cardInfoForType theType =
    CCV.allCardTypes
        |> List.filter
            (\ct ->
                ct.cardType == theType
            )
        |> List.head


type alias TestCard =
    { cardType : String
    , cardNumber : String
    }


allTestCards : List TestCard
allTestCards =
    [ TestCard "AM" "341134113411347"
    , TestCard "AM" "377777777777770" 
    , TestCard "DS" "6011000994116667"
    , TestCard "DS" "6011000995500000" 
    , TestCard "DS" "6011016011016011"
    , TestCard "DS" "6559906559906557"
    , TestCard "MC" "5110969999999990"
    , TestCard "MC" "5112345112345114"
    , TestCard "MC" "5116561111111119"
    , TestCard "MC" "5193911111111112"
    , TestCard "MC" "5473551111111117"
    , TestCard "MC" "5555555555555565"
    , TestCard "MC" "5555556666666663"
    , TestCard "VI" "4001579999999992"
    , TestCard "VI" "4077041111111112"
    , TestCard "VI" "4112344112344113"
    , TestCard "VI" "4142621111111112"
    , TestCard "VI" "4257021111111116"
    , TestCard "VI" "4444444444444455"
    , TestCard "VI" "4444445555555559"
    ]


validateTest : TestCard -> CCV.ValidationResult
validateTest testCard =
    let
        l =
            testCard

        rawNumber =
            testCard.cardNumber

        limitToCardTypes =
            [ CCV.mopToCardType testCard.cardType ]

        result =
            CCV.validate rawNumber limitToCardTypes
    in
    result


sample : Test
sample =
    describe "Sample"
        [ describe "sample1"
            [ test "add" <|
                \_ ->
                    Expect.equal 2 (1 + 1)
            ]
        ]


all : Test
all =
    describe "CC Validator tests"
        [ describe "By CC numbers"
            [ test "FilterByCardTypes gets correct card types" <|
                \_ ->
                    let
                        cardTypes =
                            [ CCV.MC ]

                        cardInfos =
                            CCV.filterByCardTypes CCV.allCardTypes cardTypes
                    in
                    cardInfos
                        |> List.length
                        |> Expect.equal 1
            , test "FilterByCardTypes gets 2 correct card types" <|
                \_ ->
                    let
                        cardTypes =
                            [ CCV.MC, CCV.DS ]

                        cardInfos =
                            CCV.filterByCardTypes CCV.allCardTypes cardTypes
                    in
                    cardInfos
                        |> List.length
                        |> Expect.equal 2
            , test "Card matches a range for card type " <|
                \_ ->
                    let
                        diCardType =
                            CCV.allCardTypes
                                |> List.filter (\ct -> ct.cardType == CCV.DS)
                                |> List.head
                                |> Maybe.withDefault unknownCardType
                    in
                    CCV.cardMatchesRange "6011000995500000"
                        diCardType.bins
                        |> Expect.equal True
            , test "Card DOES NOT match a range for card type " <|
                \_ ->
                    let
                        diCardType =
                            CCV.allCardTypes
                                |> List.filter (\ct -> ct.cardType == CCV.AM)
                                |> List.head
                                |> Maybe.withDefault unknownCardType
                    in
                    CCV.cardMatchesRange "6011000995500000"
                        diCardType.bins
                        |> Expect.equal False
            , test "Number in StartsWith range" <|
                \_ ->
                    CCV.numberInStartsWithRange "34" "341134113411347"
                        |> Expect.equal True
            , test "Find card type when in bin range" <|
                \_ ->
                    CCV.cardTypeByBinRange "341134113411347" CCV.allCardTypes
                        |> Expect.equal (CCV.allCardTypes |> List.filter (\ct -> ct.cardType == CCV.AM) |> List.head)
            , test "Find Mastercard type" <|
                \_ ->
                    CCV.cardTypeByBinRange "5473551111111117" CCV.allCardTypes
                        |> Expect.equal (CCV.allCardTypes |> List.filter (\ct -> ct.cardType == CCV.MC) |> List.head)
            , test "Find number in range" <|
                \_ ->
                    CCV.findNumberInBetweenRange "5473551111111117" "51" "55"
                        |> Expect.equal True
            , test "Valid card length for type" <|
                \_ ->
                    let
                        mcCardType =
                            cardInfoForType CCV.MC
                                |> Maybe.withDefault unknownCardType

                        cardLen =
                            16
                    in
                    CCV.validCardLength
                        mcCardType
                        "5473551111111117"
                        |> Expect.equal True
            , test "Validate card successful with valid clean number" <|
                \_ ->
                    let
                        rawNumber =
                            "5473551111111117"

                        limitToCardTypes =
                            [ CCV.MC ]

                        result =
                            CCV.validate rawNumber limitToCardTypes
                    in
                    result.valid |> Expect.equal True
            , test "Validate card FAILED with invalid clean number" <|
                \_ ->
                    let
                        rawNumber =
                            "9473551111111117"

                        limitToCardTypes =
                            [ CCV.MC ]

                        result =
                            CCV.validate rawNumber limitToCardTypes
                    in
                    result.valid |> Expect.equal False
            , test
                "mop to card Type"
              <|
                \_ ->
                    CCV.mopToCardType "DS"
                        |> Expect.equal CCV.DS
            , test
                "Card number starting with 0 fails validation"
              <|
                \_ ->
                    let
                        card =
                            TestCard "VI" "0444445555555559"

                        result =
                            validateTest card
                    in
                    result.valid |> Expect.notEqual True

            , test "validate all test cards" <|
                \_ ->
                    let
                        testResults =
                            allTestCards
                                |> List.map
                                    (\card ->
                                        let
                                            result =
                                                validateTest card
                                        in
                                        result.valid |> Expect.equal True
                                    )
                    in
                    testResults
                        |> List.all
                            (\result ->
                                result == Expect.pass
                            )
                        |> Expect.equal True
            ]
        ]
