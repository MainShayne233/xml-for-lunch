module Xml exposing (..)


type alias Attribute =
    ( String, String )


type alias Children =
    List Element


type Element
    = Element ElementData


type alias ElementData =
    { name : String
    , attributes : List Attribute
    , children : List Element
    }


linesFromElement : Int -> Element -> List String
linesFromElement padding element =
    let
        { name, attributes, children } =
            data element

        childrenLines =
            List.concatMap (linesFromElement (padding + 4)) children
    in
    [ "Name: " ++ name
    , "Attributes: " ++ stringFromAttributes attributes
    , "Children:\n" ++ String.join "\n" childrenLines
    ]
        |> List.map
            (\line ->
                let
                    indent =
                        String.repeat padding "-"
                in
                indent ++ line
            )


stringFromElement : Element -> String
stringFromElement element =
    element
        |> linesFromElement 0
        |> String.join "\n"


stringFromAttributes : List Attribute -> String
stringFromAttributes attributes =
    attributes
        |> List.map stringFromAttribute
        |> String.join ", "


stringFromAttribute : Attribute -> String
stringFromAttribute ( key, value ) =
    key ++ ": " ++ value


newElement : String -> List Attribute -> List Element -> Element
newElement name attributes children =
    Element (ElementData name attributes children)


data : Element -> ElementData
data element =
    case element of
        Element elementData ->
            elementData
