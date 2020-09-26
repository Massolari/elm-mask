module Mask exposing (brDate, cardNumber, cardValid, cardValidShort, cnpj, cpf, cpfCnpj, floatDecimal, intDecimal, number, phone, string, zipCode)


floatDecimal : Float -> String
floatDecimal dec =
    let
        intValue =
            dec
                |> (*) 10000
                |> ceiling
                |> (\n -> n // 100)
    in
    intDecimal intValue


intDecimal : Int -> String
intDecimal signedDec =
    let
        ( dec, signFun ) =
            if signedDec < 0 then
                ( signedDec * -1
                , \s -> "-" ++ s
                )

            else
                ( signedDec, identity )

        moneyStr =
            dec
                |> String.fromInt
                |> String.padLeft 3 '0'
                |> String.reverse
    in
    String.foldl
        (\ch ( counter, isDecimal, result ) ->
            if isDecimal && counter == 2 then
                ( 0, False, result ++ "," ++ String.fromChar ch )

            else if not isDecimal && counter == 2 then
                ( 0, False, result ++ "." ++ String.fromChar ch )

            else
                ( counter + 1, isDecimal, result ++ String.fromChar ch )
        )
        ( 0, True, "" )
        moneyStr
        |> (\( _, _, result ) -> result)
        |> String.reverse
        |> signFun


cpfCnpj : String -> String
cpfCnpj doc =
    if String.length doc > 14 then
        number "##.###.###/####-##" doc

    else
        number "###.###.###-##" doc


cpf : String -> String
cpf c =
    number "###.###.###-##" c


cnpj : String -> String
cnpj c =
    number "##.###.###/####-##" c


zipCode : String -> String
zipCode zip =
    number "#####-###" zip


phone : String -> String
phone ph =
    number "(##) # ####-####" ph


brDate : String -> String
brDate =
    number "##/##/####"


cardNumber : String -> String
cardNumber =
    number "#### #### #### ####"


cardValid : String -> String
cardValid =
    number "##/####"


cardValidShort : String -> String
cardValidShort =
    number "##/##"


{-| Formata a string permitindo apenas números

    maskStringNumber "(##) ####-####" "1234567890" == "(12) 3456-7890"

    maskStringNumber "####" "1b0a" == "10"

-}
number : String -> String -> String
number pattern value =
    String.filter Char.isDigit value
        |> (\v -> maskStringHelper pattern v "")


{-| Formata a string

    maskString "(##) ####-####" "1234567890" == "(12) 3456-7890"

-}
string : String -> String -> String
string pattern value =
    maskStringHelper pattern value ""


maskStringHelper : String -> String -> String -> String
maskStringHelper pattern value result =
    let
        valueUncons =
            String.uncons value
    in
    case valueUncons of
        Just ( charValue, rest ) ->
            case String.uncons pattern of
                Just ( a, b ) ->
                    if a == '#' then
                        maskStringHelper b rest (result ++ String.fromChar charValue)

                    else
                        maskStringHelper b value (result ++ String.fromChar a)

                Nothing ->
                    result

        Nothing ->
            result
