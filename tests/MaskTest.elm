module MaskTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Mask exposing (defaultDecimalOptions)
import Test exposing (..)


suite : Test
suite =
    describe "The Mask module"
        [ describe "Mask.floatDecimal"
            [ test "Mask 1 million with default options" <|
                \_ ->
                    Mask.floatDecimal defaultDecimalOptions 1000000.0
                        |> Expect.equal "1,000,000.00"
            , test "Mask negative 1 million default options" <|
                \_ ->
                    Mask.floatDecimal defaultDecimalOptions -1000000.0
                        |> Expect.equal "-1,000,000.00"
            , test "Mask 1 million with 3 precision" <|
                \_ ->
                    Mask.floatDecimal { defaultDecimalOptions | precision = 3 } 1000000.0
                        |> Expect.equal "100,000.000"
            , test "Mask 1 million with 0 precision" <|
                \_ ->
                    Mask.floatDecimal { defaultDecimalOptions | precision = 0 } 1000000.0
                        |> Expect.equal "100,000,000"
            , test "Mask 1 million with -1 precision" <|
                \_ ->
                    Mask.floatDecimal { defaultDecimalOptions | precision = -1 } 1000000.0
                        |> Expect.equal "100,000,000"
            , test "Mask 1 million with 4 precision and brazilian notation" <|
                \_ ->
                    Mask.floatDecimal
                        { defaultDecimalOptions
                            | precision = 4
                            , decimal = ","
                            , thousand = "."
                        }
                        1000000.0
                        |> Expect.equal "10.000,0000"
            ]
        , describe "Mask.intDecimal"
            [ test "Mask 1 million with default options" <|
                \_ ->
                    Mask.floatDecimal defaultDecimalOptions 1000000
                        |> Expect.equal "1,000,000.00"
            , test "Mask negative 1 million with default options" <|
                \_ ->
                    Mask.floatDecimal defaultDecimalOptions -1000000
                        |> Expect.equal "-1,000,000.00"
            , test "Mask 1 million with 3 precision" <|
                \_ ->
                    Mask.floatDecimal { defaultDecimalOptions | precision = 3 } 1000000
                        |> Expect.equal "100,000.000"
            , test "Mask 1 million with 0 precision" <|
                \_ ->
                    Mask.floatDecimal { defaultDecimalOptions | precision = 0 } 1000000
                        |> Expect.equal "100,000,000"
            , test "Mask 1 million with -1 precision" <|
                \_ ->
                    Mask.floatDecimal { defaultDecimalOptions | precision = -1 } 1000000
                        |> Expect.equal "100,000,000"
            , test "Mask 1 million with 4 precision and brazilian notation" <|
                \_ ->
                    Mask.floatDecimal
                        { defaultDecimalOptions
                            | precision = 4
                            , decimal = ","
                            , thousand = "."
                        }
                        1000000
                        |> Expect.equal "10.000,0000"
            ]
        , describe "Mask.number"
            [ test "Mask phone filtering numbers" <|
                \_ ->
                    Mask.number "(##) ####-####" "q1w2e3r4t5y6u7i8o9p0"
                        |> Expect.equal "(12) 3456-7890"
            ]
        , describe "Mask.string"
            [ test "Mask simple string" <|
                \_ ->
                    Mask.string "##-##" "abcd"
                        |> Expect.equal "ab-cd"
            , test "Mask string with pattern bigger than value" <|
                \_ ->
                    Mask.string "#-##-#" "q1w"
                        |> Expect.equal "q-1w"
            , test "Mask string with pattern with suffix" <|
                \_ ->
                    Mask.string "A##B" "ab"
                        |> Expect.equal "AabB"
            ]
        ]
