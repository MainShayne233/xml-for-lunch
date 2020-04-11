module ParserCombinator exposing (..)


type alias Parser =
    String -> Result String ( String, String )


theLetterA : Parser
theLetterA input =
    if String.startsWith "a" input then
        Ok ( String.dropLeft 1 input, "a" )

    else
        Err input


matchLiteral : String -> Parser
matchLiteral literal =
    \input ->
        if String.startsWith literal input then
            Ok ( String.dropLeft (String.length literal) input, literal )

        else
            Err input


identifier : Parser
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


pair : Parser -> Parser -> (String -> Result String ( String, ( String, String ) ))
pair firstParser secondParser =
    \input ->
        case firstParser input of
            Ok ( nextInput, firstParsed ) ->
                case secondParser nextInput of
                    Ok ( finalInput, secondParsed ) ->
                        Ok ( finalInput, ( firstParsed, secondParsed ) )

                    Err error ->
                        Err error

            Err error ->
                Err error


map : Parser -> (String -> a) -> (String -> Result String ( String, a ))
map parser mapper =
    \input ->
        case parser input of
            Ok ( rest, parsed ) ->
                Ok ( rest, mapper parsed )

            Err error ->
                Err error


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
