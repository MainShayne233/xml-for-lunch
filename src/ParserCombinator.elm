module ParserCombinator exposing (..)


type alias Parser =
    String -> Result String ( String, String )


theLetterA : Parser
theLetterA input =
    if String.startsWith "a" input then
        Ok ( String.dropLeft 1 input, "a" )

    else
        Err input
