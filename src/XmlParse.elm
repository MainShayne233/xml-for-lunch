module XmlParse exposing (..)

import ParserCombinator
import Xml exposing (Element)


parse : String -> Result String Element
parse input =
    case ParserCombinator.element input of
        Ok ( "", elem ) ->
            Ok elem

        Ok ( _, _ ) ->
            Err "XML document must be enclosed in a single element"

        Err err ->
            Err err
