module Tests exposing (..)

import Expect
import ParserCombinator
import Test exposing (..)



-- Check out https://package.elm-lang.org/packages/elm-explorations/test/latest to learn more about testing in Elm!


all : Test
all =
    describe "ParserCombinator"
        [ describe "theLetterA"
            [ test "should match on the letter a" <|
                \_ ->
                    let
                        result =
                            ParserCombinator.theLetterA "a"
                    in
                    Expect.equal (Ok ( "", "a" )) result
            , test "should result in an error for any other char" <|
                \_ ->
                    let
                        result =
                            ParserCombinator.theLetterA "b"
                    in
                    Expect.equal (Err "b") result
            ]
        , describe "matchLiteral"
            [ test "should produce a parser that will match on the given literal" <|
                \_ ->
                    let
                        parser =
                            ParserCombinator.matchLiteral "asdf"

                        result =
                            parser "asdfhello"
                    in
                    Expect.equal (Ok ( "hello", "asdf" )) result
            , test "should produce a parser that will not match on a value that is not the given literal" <|
                \_ ->
                    let
                        parser =
                            ParserCombinator.matchLiteral "asdf"

                        result =
                            parser "helloasdf"
                    in
                    Expect.equal (Err "helloasdf") result
            ]
        , describe "identifier"
            [ test "should successfully parse a valid identifier" <|
                \_ ->
                    let
                        result =
                            ParserCombinator.identifier "i-am-an-identifier asdf"
                    in
                    Expect.equal (Ok ( " asdf", "i-am-an-identifier" )) result
            , test "should fail to parse an invalid identifier" <|
                \_ ->
                    let
                        result =
                            ParserCombinator.identifier "-not-an-identifier"
                    in
                    Expect.equal (Err "-not-an-identifier") result
            ]
        , describe "pair"
            [ test "should produce a parser that is a combination of two other parsers" <|
                \_ ->
                    let
                        firstParser =
                            ParserCombinator.identifier

                        secondParser =
                            ParserCombinator.matchLiteral "!"

                        pairParser =
                            ParserCombinator.pair firstParser secondParser

                        result =
                            pairParser "woah!"
                    in
                    Expect.equal (Ok ( "", ( "woah", "!" ) )) result
            , test "should produce a parser that will fail if the first parser fails" <|
                \_ ->
                    let
                        firstParser =
                            ParserCombinator.identifier

                        secondParser =
                            ParserCombinator.matchLiteral "!"

                        pairParser =
                            ParserCombinator.pair firstParser secondParser

                        result =
                            pairParser "_woah!"
                    in
                    Expect.equal (Err "_woah!") result
            , test "should produce a parser that will fail if the second parser fails" <|
                \_ ->
                    let
                        firstParser =
                            ParserCombinator.identifier

                        secondParser =
                            ParserCombinator.matchLiteral "!"

                        pairParser =
                            ParserCombinator.pair firstParser secondParser

                        result =
                            pairParser "woah?"
                    in
                    Expect.equal (Err "?") result
            ]
        , describe "map"
            [ test "should produce a function that will map the successful result of a parser" <|
                \_ ->
                    let
                        parser =
                            ParserCombinator.identifier

                        mappedParser =
                            ParserCombinator.map parser (\_ -> 100)

                        result =
                            mappedParser "asdf"
                    in
                    Expect.equal (Ok ( "", 100 )) result
            , test "should produce a function that drop the error result through for a failed parse" <|
                \_ ->
                    let
                        parser =
                            ParserCombinator.identifier

                        mappedParser =
                            ParserCombinator.map parser (\_ -> 100)

                        result =
                            mappedParser "_asdf"
                    in
                    Expect.equal (Err "_asdf") result
            ]
        ]
