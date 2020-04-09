module Main exposing (..)

import Browser
import Html exposing (Html, div, h1, img, p, text, textarea)
import Html.Attributes exposing (cols, rows, src, value)
import Html.Events exposing (onInput)
import Xml
import XmlParse



---- MODEL ----


type alias Model =
    { input : String }


init : ( Model, Cmd Msg )
init =
    ( { input = "" }, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp
    | InputChanged String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        InputChanged newInput ->
            ( { model | input = newInput }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ textarea [ rows 20, cols 80, onInput InputChanged ] []
        , viewParseResult model
        ]


viewParseResult : Model -> Html Msg
viewParseResult model =
    case XmlParse.parse model.input of
        Err errorMessage ->
            p [] [ text ("Error: " ++ errorMessage) ]

        Ok element ->
            p [] [ text (Xml.stringFromElement element) ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
