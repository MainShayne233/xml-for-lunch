module ParserCombinator exposing (..)

import Xml


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


oneOrMore : Parser a -> Parser (List a)
oneOrMore parser =
    \input ->
        case zeroOrMore parser input of
            Ok ( _, [] ) ->
                Err input

            other ->
                other


zeroOrMore : Parser a -> Parser (List a)
zeroOrMore parser =
    \input ->
        doZeroOrMore parser input []


doZeroOrMore : Parser a -> String -> List a -> ParseResult (List a)
doZeroOrMore parser input matches =
    case parser input of
        Err error ->
            Ok ( input, matches )

        Ok ( nextInput, match ) ->
            doZeroOrMore parser nextInput (matches ++ [ match ])


quotedString : Parser String
quotedString =
    matchLiteral "\""
        |> left (zeroOrMore (pred (isNotChar '"') anyChar))
        |> right (matchLiteral "\"")
        |> map String.fromList


elementStart : Parser ( String, List ( String, String ) )
elementStart =
    right (matchLiteral "<") (pair identifier attributes)


singleElement : Parser Xml.Element
singleElement =
    left elementStart (matchLiteral "/>")
        |> map
            (\( name, attrs ) ->
                Xml.newElement name attrs []
            )


attributePair : Parser ( String, String )
attributePair =
    pair identifier (right (matchLiteral "=") quotedString)


attributes : Parser (List ( String, String ))
attributes =
    zeroOrMore (right someWhitespace attributePair)


anyChar : Parser Char
anyChar input =
    case String.toList input of
        head :: tail ->
            Ok ( String.fromList tail, head )

        [] ->
            Err input


someWhitespace : Parser (List Char)
someWhitespace =
    oneOrMore whitespaceChar


someOrNoWhitespace : Parser (List Char)
someOrNoWhitespace =
    zeroOrMore whitespaceChar


whitespaceChar : Parser Char
whitespaceChar =
    pred isWhitespace anyChar


pred : (a -> Bool) -> Parser a -> Parser a
pred predicate parser =
    \input ->
        input
            |> parser
            |> Result.andThen
                (\( restInput, match ) ->
                    if predicate match then
                        Ok ( restInput, match )

                    else
                        Err input
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


isWhitespace : Char -> Bool
isWhitespace char =
    List.member char [ ' ' ]


isNotChar : Char -> Char -> Bool
isNotChar lhsChar rhsChar =
    lhsChar /= rhsChar
