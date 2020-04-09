module Tests exposing (..)

import Expect
import Test exposing (..)
import XmlParse



-- Check out https://package.elm-lang.org/packages/elm-explorations/test/latest to learn more about testing in Elm!


all : Test
all =
    describe "A Test Suite"
        [ test "XmlParse.matchLiteral: success" <|
            \_ ->
                let
                    parser =
                        XmlParse.matchLiteral "asdf"
                in
                Expect.equal (Ok "hello") (parser "asdfhello")
        , test "XmlParse.matchLiteral: failure" <|
            \_ ->
                let
                    parser =
                        XmlParse.matchLiteral "asdf"
                in
                Expect.equal (Err "helloasdf") (parser "helloasdf")
        , test "String.left" <|
            \_ ->
                Expect.equal "a" (String.left 1 "abcdefg")
        ]
