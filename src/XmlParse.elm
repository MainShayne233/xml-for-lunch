module XmlParse exposing (..)

import Xml exposing (Element)


parse : String -> Result String Element
parse input =
    Err "Nope"



-- parseTheLetterA : String -> Result String String
-- parseTheLetterA input =
--     if String.startsWith "a" input then
--         Ok (String.slice 1 (String.length input) input)
--     else
--         Err input
-- matchLiteral : String -> Parser
-- matchLiteral literal =
--     \input ->
--         if String.startsWith literal input then
--             Ok ( literal, String.slice (String.length literal) (String.length input) input )
--         else
--             Err input
-- identifier : Parser
-- identifier input =
--     case splitWhile isAlphabetic (String.toList input) of
--         ( [], _ ) ->
--             Err input
--         ( identChars, rest ) ->
--             Ok ( String.fromList identChars, String.fromList rest )
-- isAlphabetic : Char -> Bool
-- isAlphabetic char =
--     List.member (Char.toCode char) (List.range 65 122)
-- splitWhile : (Char -> Bool) -> List Char -> ( List Char, List Char )
-- splitWhile check chars =
--     case chars of
--         [] ->
--             ( [], [] )
--         head :: tail ->
--             if check head then
--                 let
--                     ( nextMatch, nextRest ) =
--                         splitWhile check tail
--                 in
--                 ( head :: nextMatch, nextRest )
--             else
--                 ( [], head :: tail )
