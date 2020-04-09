module XmlParse exposing (..)

import Xml exposing (Element)


type alias Parser =
    String -> Result String String


parse : String -> Result String Element
parse input =
    Err "Nope"


parseTheLetterA : String -> Result String String
parseTheLetterA input =
    if String.startsWith "a" input then
        Ok (String.slice 1 (String.length input) input)

    else
        Err input


matchLiteral : String -> Parser
matchLiteral literal =
    \input ->
        if String.startsWith literal input then
            Ok (String.slice (String.length literal) (String.length input) input)

        else
            Err input
