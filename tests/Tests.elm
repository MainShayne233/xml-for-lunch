module Tests exposing (..)

import Expect
import ParserCombinator
import Test exposing (..)
import Xml



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
                            ParserCombinator.map (\_ -> 100) parser

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
                            ParserCombinator.map (\_ -> 100) parser

                        result =
                            mappedParser "_asdf"
                    in
                    Expect.equal (Err "_asdf") result
            ]
        , describe "right"
            [ test "should produce a parser that's success result will be the rhs parser's value" <|
                \_ ->
                    let
                        parser =
                            ParserCombinator.right (ParserCombinator.matchLiteral "<") ParserCombinator.identifier

                        result =
                            parser "<my-first-element/>"
                    in
                    Expect.equal (Ok ( "/>", "my-first-element" )) result
            , test "produced parser should err if the first parser fails" <|
                \_ ->
                    let
                        parser =
                            ParserCombinator.right (ParserCombinator.matchLiteral "<") ParserCombinator.identifier

                        result =
                            parser "oops"
                    in
                    Expect.equal (Err "oops") result
            , test "produced parser should err if the second parser fails" <|
                \_ ->
                    let
                        parser =
                            ParserCombinator.right (ParserCombinator.matchLiteral "<") ParserCombinator.identifier

                        result =
                            parser "<!oops"
                    in
                    Expect.equal (Err "!oops") result
            ]
        , describe "oneOrMore"
            [ test "success: parser one match" <|
                \_ ->
                    let
                        parser =
                            ParserCombinator.oneOrMore (ParserCombinator.matchLiteral "asdf")

                        result =
                            parser "asdf"
                    in
                    Expect.equal (Ok ( "", [ "asdf" ] )) result
            , test "success: parser more than one match" <|
                \_ ->
                    let
                        parser =
                            ParserCombinator.oneOrMore (ParserCombinator.matchLiteral "asdf")

                        result =
                            parser "asdfasdfasdf"
                    in
                    Expect.equal (Ok ( "", [ "asdf", "asdf", "asdf" ] )) result
            , test "failure: returns err if not a single match was made" <|
                \_ ->
                    let
                        parser =
                            ParserCombinator.oneOrMore (ParserCombinator.matchLiteral "asdf")

                        result =
                            parser "!asdf"
                    in
                    Expect.equal (Err "!asdf") result
            ]
        , describe "zeroOrMore"
            [ test "success: parser one match" <|
                \_ ->
                    let
                        parser =
                            ParserCombinator.zeroOrMore (ParserCombinator.matchLiteral "asdf")

                        result =
                            parser "asdf"
                    in
                    Expect.equal (Ok ( "", [ "asdf" ] )) result
            , test "success: parser more than one match" <|
                \_ ->
                    let
                        parser =
                            ParserCombinator.zeroOrMore (ParserCombinator.matchLiteral "asdf")

                        result =
                            parser "asdfasdfasdf"
                    in
                    Expect.equal (Ok ( "", [ "asdf", "asdf", "asdf" ] )) result
            , test "success: should return an empty list for no matches" <|
                \_ ->
                    let
                        parser =
                            ParserCombinator.zeroOrMore (ParserCombinator.matchLiteral "asdf")

                        result =
                            parser "!asdf"
                    in
                    Expect.equal (Ok ( "!asdf", [] )) result
            ]
        , describe "anyChar"
            [ test "success: parses a char" <|
                \_ ->
                    let
                        result =
                            ParserCombinator.anyChar "asdf"
                    in
                    Expect.equal (Ok ( "sdf", 'a' )) result
            , test "failure: no char in empty string" <|
                \_ ->
                    let
                        result =
                            ParserCombinator.anyChar ""
                    in
                    Expect.equal (Err "") result
            ]
        , describe "pred"
            [ test "success: produces a parser that requires a predicate function to pass" <|
                \_ ->
                    let
                        parser =
                            ParserCombinator.pred (\ident -> String.length ident > 1) ParserCombinator.identifier

                        result =
                            parser "asdf"
                    in
                    Expect.equal (Ok ( "", "asdf" )) result
            , test "failure: returns err if predicate returns false" <|
                \_ ->
                    let
                        parser =
                            ParserCombinator.pred (\ident -> String.length ident > 1) ParserCombinator.identifier

                        result =
                            parser "a"
                    in
                    Expect.equal (Err "a") result
            ]
        , describe "quotedString"
            [ test "success: parses a quoted string" <|
                \_ ->
                    let
                        result =
                            ParserCombinator.quotedString "\"hello, world!\""
                    in
                    Expect.equal (Ok ( "", "hello, world!" )) result
            ]
        , describe "attributePair"
            [ test "success: should parse a valid attribute pair" <|
                \_ ->
                    let
                        result =
                            ParserCombinator.attributePair "key=\"value\""
                    in
                    Expect.equal (Ok ( "", ( "key", "value" ) )) result
            ]
        , describe "someWhitespace"
            [ test "success: should parse a single space" <|
                \_ ->
                    let
                        result =
                            ParserCombinator.someWhitespace " "
                    in
                    Expect.equal (Ok ( "", [ ' ' ] )) result
            ]
        , describe "attributes"
            [ test "success: should parse all valid attribute pairs" <|
                \_ ->
                    let
                        result =
                            ParserCombinator.attributes " key=\"value\" otherkey=\"other value\""
                    in
                    Expect.equal (Ok ( "", [ ( "key", "value" ), ( "otherkey", "other value" ) ] )) result
            ]
        , describe "elementStart"
            [ test "success: should parse the start of an element" <|
                \_ ->
                    let
                        result =
                            ParserCombinator.elementStart "<elem key=\"value\" other=\"another\""
                    in
                    Expect.equal (Ok ( "", ( "elem", [ ( "key", "value" ), ( "other", "another" ) ] ) )) result
            ]
        , describe "singleElement"
            [ test "success: should parse a valid single element" <|
                \_ ->
                    let
                        result =
                            ParserCombinator.singleElement "<elem/>"

                        expected =
                            Xml.newElement "elem" [] []
                    in
                    Expect.equal (Ok ( "", expected )) result
            , test "success: should parse a valid single element with attrs" <|
                \_ ->
                    let
                        result =
                            ParserCombinator.singleElement "<elem key=\"value\" other=\"another\"/>"

                        expected =
                            Xml.newElement "elem" [ ( "key", "value" ), ( "other", "another" ) ] []
                    in
                    Expect.equal (Ok ( "", expected )) result
            ]
        , describe "openElement"
            [ test "success: should parse a valid open element" <|
                \_ ->
                    let
                        result =
                            ParserCombinator.openElement "<elem key=\"value\" other=\"another\">"

                        expected =
                            Xml.newElement "elem" [ ( "key", "value" ), ( "other", "another" ) ] []
                    in
                    Expect.equal (Ok ( "", expected )) result
            ]
        , describe "either"
            [ test "success: will return the result if the first parser if successful" <|
                \_ ->
                    let
                        lhsParser =
                            ParserCombinator.matchLiteral "asdf"

                        rhsParser =
                            ParserCombinator.matchLiteral "hello"

                        eitherParser =
                            ParserCombinator.either lhsParser rhsParser

                        result =
                            eitherParser "asdf"
                    in
                    Expect.equal (Ok ( "", "asdf" )) result
            , test "success: will return the result if the second parser if successful" <|
                \_ ->
                    let
                        lhsParser =
                            ParserCombinator.matchLiteral "asdf"

                        rhsParser =
                            ParserCombinator.matchLiteral "hello"

                        eitherParser =
                            ParserCombinator.either lhsParser rhsParser

                        result =
                            eitherParser "hello"
                    in
                    Expect.equal (Ok ( "", "hello" )) result
            , test "failure: will return an err if both parsers fail" <|
                \_ ->
                    let
                        lhsParser =
                            ParserCombinator.matchLiteral "asdf"

                        rhsParser =
                            ParserCombinator.matchLiteral "hello"

                        eitherParser =
                            ParserCombinator.either lhsParser rhsParser

                        result =
                            eitherParser "hey"
                    in
                    Expect.equal (Err "hey") result
            ]
        , describe "element"
            [ test "success: should parse a single element" <|
                \_ ->
                    let
                        result =
                            ParserCombinator.element "<elem key=\"value\" other=\"another\"/>"

                        expected =
                            Xml.newElement "elem" [ ( "key", "value" ), ( "other", "another" ) ] []
                    in
                    Expect.equal (Ok ( "", expected )) result
            , test "success: should tolerate white space" <|
                \_ ->
                    let
                        result =
                            ParserCombinator.element """
                                                      <cool key="value">
                                                        <hi name="sean"/>
                                                      </cool>
                                                      """

                        expected =
                            Xml.newElement "cool" [ ( "key", "value" ) ] [ Xml.newElement "hi" [ ( "name", "sean" ) ] [] ]
                    in
                    Expect.equal (Ok ( "", expected )) result
            , test "success: should parse a parent element" <|
                \_ ->
                    let
                        result =
                            ParserCombinator.element "<elem key=\"value\" other=\"another\"></elem>"

                        expected =
                            Xml.newElement "elem" [ ( "key", "value" ), ( "other", "another" ) ] []
                    in
                    Expect.equal (Ok ( "", expected )) result
            ]
        , describe "closeElement"
            [ test "success: should parse a closed element" <|
                \_ ->
                    let
                        parser =
                            ParserCombinator.closeElement "cool"

                        result =
                            parser "</cool>"
                    in
                    Expect.equal (Ok ( "", "cool" )) result
            ]
        , describe "parentElement"
            [ test "success: should parse an element with children" <|
                \_ ->
                    let
                        result =
                            ParserCombinator.parentElement "<cool><hi/></cool>"

                        expected =
                            Xml.newElement "cool" [] [ Xml.newElement "hi" [] [] ]
                    in
                    Expect.equal (Ok ( "", expected )) result
            ]
        ]
