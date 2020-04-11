module ParserCombinator exposing (..)


type alias ParseResult t =
    Result String ( String, t )


type alias Parser t =
    String -> ParseResult t


theLetterA : Parser String
theLetterA input =
    if String.startsWith "a" input then
        Ok ( String.dropLeft 1 input, "a" )

    else
        Err input


matchLiteral : String -> Parser String
matchLiteral literal =
    \input ->
        if String.startsWith literal input then
            Ok ( String.dropLeft (String.length literal) input, literal )

        else
            Err input


identifier : Parser String
identifier input =
    case String.toList input of
        [] ->
            Err input

        head :: tail ->
            if isAlphanumeric head then
                let
                    ( restOfIdent, rest ) =
                        splitWhile (\char -> isAlphanumeric char || char == '-') tail

                    ident =
                        String.fromList (head :: restOfIdent)

                    remaining =
                        String.fromList rest
                in
                Ok ( remaining, ident )

            else
                Err input


pair : Parser a -> Parser b -> Parser ( a, b )
pair firstParser secondParser =
    \input ->
        input
            |> firstParser
            |> Result.andThen
                (\( nextInput, firstParsed ) ->
                    nextInput
                        |> secondParser
                        |> Result.andThen
                            (\( finalInput, secondParsed ) ->
                                Ok ( finalInput, ( firstParsed, secondParsed ) )
                            )
                )


left : Parser a -> Parser b -> Parser a
left lhsParser rhsParser =
    rhsParser
        |> pair lhsParser
        |> map Tuple.first


right : Parser a -> Parser b -> Parser b
right lhsParser rhsParser =
    rhsParser
        |> pair lhsParser
        |> map Tuple.second


map : (a -> b) -> Parser a -> Parser b
map mapper parser =
    \input ->
        input
            |> parser
            |> Result.andThen (\( rest, parsed ) -> Ok ( rest, mapper parsed ))


isAlphanumeric : Char -> Bool
isAlphanumeric char =
    List.member (Char.toCode char)
        (List.concat
            [ List.range (Char.toCode 'A') (Char.toCode 'Z')
            , List.range (Char.toCode 'a') (Char.toCode 'z')
            , List.range (Char.toCode '0') (Char.toCode '9')
            ]
        )


splitAt : Int -> List a -> ( List a, List a )
splitAt index list =
    ( List.take index list, List.drop index list )


splitWhile : (Char -> Bool) -> List Char -> ( List Char, List Char )
splitWhile check chars =
    case chars of
        [] ->
            ( [], [] )

        head :: tail ->
            if check head then
                let
                    ( nextMatch, nextRest ) =
                        splitWhile check tail
                in
                ( head :: nextMatch, nextRest )

            else
                ( [], head :: tail )
