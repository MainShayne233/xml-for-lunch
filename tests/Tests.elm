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

        -- test "ParserCombinator.matchLiteral: success" <|
        --     \_ ->
        --         let
        --             parser =
        --                 ParserCombinator.matchLiteral "asdf"
        --         in
        --         Expect.equal (Ok ( "asdf", "hello" )) (parser "asdfhello")
        -- , test "ParserCombinator.matchLiteral: failure" <|
        --     \_ ->
        --         let
        --             parser =
        --                 ParserCombinator.matchLiteral "asdf"
        --         in
        --         Expect.equal (Err "helloasdf") (parser "helloasdf")
        -- , test "ParserCombinator.splitWhile" <|
        --     \_ ->
        --         let
        --             actual =
        --                 ParserCombinator.splitWhile ParserCombinator.isAlphabetic [ 'a', 'b', 'c', '1' ]
        --         in
        --         Expect.equal ( [ 'a', 'b', 'c' ], [ '1' ] ) actual
        -- , test "ParserCombinator.identifier" <|
        --     \_ ->
        --         let
        --             actual =
        --                 ParserCombinator.identifier "doIt 1234"
        --         in
        --         Expect.equal (Ok ( "doIt", " 1234" )) actual
        -- , test "String.left" <|
        --     \_ ->
        --         Expect.equal "a" (String.left 1 "abcdefg")
        ]
