module Mask exposing
    ( string, number
    , floatDecimal, intDecimal, DecimalOptions, defaultDecimalOptions
    )

{-| A mask is a formatting pattern that you can apply to a value. This module has some functions to help you create and apply masks.


# Common Helpers

@docs string, number


# Formating numbers

@docs floatDecimal, intDecimal, defaultDecimalOptions, DecimalOptions

-}


{-| Options used to format numbers. `decimal` and `thousand` are the characters that separate each section of the number
-}
type alias DecimalOptions =
    { decimal : String
    , thousand : String
    , precision : Int
    }


{-| Default options to format numbers. Extend this if
you want to customize the way numbers are formatted.
-}
defaultDecimalOptions : DecimalOptions
defaultDecimalOptions =
    { decimal = "."
    , thousand = ","
    , precision = 2
    }


{-| Formats a `Float` number according to the specified options

    floatDecimal defaultDecimalOptions 10500.5 == "10.500.50"

    floatDecimal { defaultDecimalOptions | precision = 3, decimal = ",", thousand = "." } 10500.5 == "1.050,050"

-}
floatDecimal : DecimalOptions -> Float -> String
floatDecimal options dec =
    let
        intValue =
            dec
                |> (*) (10 ^ (toFloat options.precision + 2.0))
                |> ceiling
                |> (\n -> n // (10 ^ options.precision))
    in
    intDecimal options intValue


{-| Formats a `Int` number according to the specified options

    intDecimal defaultDecimalOptions 105005 == "1,050.05"

    intDecimal { defaultDecimalOptions | precision = 3, decimal = "," } 105005 == "105,005"

-}
intDecimal : DecimalOptions -> Int -> String
intDecimal options signedDec =
    let
        ( dec, signFun ) =
            if signedDec < 0 then
                ( signedDec * -1
                , \s -> "-" ++ s
                )

            else
                ( signedDec, identity )

        decStr =
            dec
                |> String.fromInt
                |> String.reverse

        ( initCounter, hasDecimal ) =
            if options.precision > 0 then
                ( 0, True )

            else
                ( -1, False )
    in
    String.foldl
        (\ch ( counter, isDecimal, result ) ->
            if isDecimal && counter == options.precision then
                ( 0, False, result ++ options.decimal ++ String.fromChar ch )

            else if not isDecimal && counter == 2 then
                ( 0, False, result ++ options.thousand ++ String.fromChar ch )

            else
                ( counter + 1, isDecimal, result ++ String.fromChar ch )
        )
        ( initCounter, hasDecimal, "" )
        decStr
        |> (\( _, _, result ) -> result)
        |> String.reverse
        |> (\s ->
                if options.precision > 0 && options.precision >= String.length s then
                    "0" ++ options.decimal ++ String.repeat (options.precision - String.length s) "0" ++ s

                else
                    s
           )
        |> signFun


{-| Formats a string filtering only the numbers of it

    number "(##) ####-####" "1234567890" == "(12) 3456-7890"

    number "#-##-#" "1b0a" == "1-0"

-}
number : String -> String -> String
number pattern value =
    String.filter Char.isDigit value
        |> (\v -> maskStringHelper pattern v "")


{-| Formats a string using a pattern. A pattern is a string where the `#` is going to be replaced by a character of the input

    string "#-##-#" "1b0a" == "1-b0-a"

    string "FOO##.###.##BAR" "EXAMPLE" == "FOEX.AMP.LEBAR"

-}
string : String -> String -> String
string pattern value =
    maskStringHelper pattern value ""


maskStringHelper : String -> String -> String -> String
maskStringHelper pattern value result =
    case String.uncons pattern of
        Just ( headPattern, tailPattern ) ->
            case String.uncons value of
                Just ( charValue, rest ) ->
                    case headPattern of
                        '#' ->
                            maskStringHelper tailPattern rest (result ++ String.fromChar charValue)

                        notHashtagHeadPattern ->
                            maskStringHelper tailPattern value (result ++ String.fromChar notHashtagHeadPattern)

                Nothing ->
                    if String.contains "#" pattern then
                        result

                    else
                        result ++ pattern

        Nothing ->
            result
