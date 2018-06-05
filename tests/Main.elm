port module Main exposing (..)

import Test.Runner.Node exposing (run, TestProgram)
import Json.Encode exposing (Value)
import CreditCardValidatorTests

main : TestProgram
main =
    run emit CreditCardValidatorTests.all

port emit : ( String, Value ) -> Cmd msg
