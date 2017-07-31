module Tests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Test exposing (..)
import CsvDecode exposing (..)
import Dict


testCsv : String -> List a -> Decoder a -> (() -> Expectation)
testCsv source exp decoder =
    \_ ->
        case CsvDecode.run decoder source of
            Ok actual ->
                Expect.equal exp actual

            Err s ->
                Expect.fail ("fail to decode: " ++ s)


testCsvWithOptions : Options -> String -> List a -> Decoder a -> (() -> Expectation)
testCsvWithOptions options source exp decoder =
    \_ ->
        case CsvDecode.runWithOptions options decoder source of
            Ok actual ->
                Expect.equal exp actual

            Err s ->
                Expect.fail ("fail to decode: " ++ s)


testCsvFail : String -> Decoder a -> (() -> Expectation)
testCsvFail source decoder =
    \_ ->
        case CsvDecode.run decoder source of
            Ok actual ->
                Expect.fail ("unexpectedly succeeded: " ++ toString actual)

            Err _ ->
                Expect.pass


testCsvWithOptionsFail : Options -> String -> Decoder a -> (() -> Expectation)
testCsvWithOptionsFail options source decoder =
    \_ ->
        case CsvDecode.runWithOptions options decoder source of
            Ok actual ->
                Expect.fail ("unexpectedly succeeded: " ++ toString actual)

            Err _ ->
                Expect.pass


testRunAll : String -> List a -> Int -> Decoder a -> (() -> Expectation)
testRunAll source exp expectedErrorCount decoder =
    \_ ->
        let
            ( actual, errors ) =
                CsvDecode.runAll decoder source
        in
            Expect.equal ( exp, expectedErrorCount ) ( actual, List.length errors )


testRunAllWithOptions : Options -> String -> List a -> Int -> Decoder a -> (() -> Expectation)
testRunAllWithOptions options source exp expectedErrorCount decoder =
    \_ ->
        let
            ( actual, errors ) =
                CsvDecode.runAllWithOptions options decoder source
        in
            Expect.equal ( exp, expectedErrorCount ) ( actual, List.length errors )


suite : Test
suite =
    describe "CsvDecode"
        [ describe "succeed"
            [ test "always succeeds" <|
                testCsv "1,2,3\nfoo" [ 1 ] <|
                    succeed 1
            , test "works if records are empty" <|
                testCsv "1,2,3\n" [] <|
                    succeed 1
            ]
        , describe "fail"
            [ test "always fails" <|
                testCsvFail "1,2,3\na,b,c" <|
                    fail ""
            ]
        , describe "field"
            [ test "works" <|
                testCsv "1,2,3\na,b,c" [ "b" ] <|
                    field "2"
            , test "fails if it does not exist" <|
                testCsvFail "1,2,3\na,b,c" <|
                    field "foo"
            , test "fails if value is empty" <|
                testCsvFail "1,2,3\na,,c" <|
                    field "2"
            , test "fails if passed field is not unique" <|
                testCsvFail "1,1,3\na,b,c" <|
                    field "1"
            ]
        , describe "index"
            [ test "works" <|
                testCsv "1,2,3\na,b,c" [ "b" ] <|
                    index 1
            , test "fails if value is empty" <|
                testCsvFail "1,2,3\na,,c" <|
                    index 1
            , test "fails if it is out of range" <|
                testCsvFail "1,2,3\na,b,c" <|
                    index 3
            ]
        , describe "fieldsAfter"
            [ test "works" <|
                testCsv "1,2,3\na,b,c" [ Dict.fromList [ ( "2", "b" ), ( "3", "c" ) ] ] <|
                    fieldsAfter "1"
            , test "fails if passed field does not exist" <|
                testCsvFail "1,2,3\na,b,c" <|
                    fieldsAfter "4"
            , test "fails if passed field is not unique" <|
                testCsvFail "1,1,3\na,b,c" <|
                    fieldsAfter "1"
            , test "works if passed field is unique" <|
                testCsv "1,1,2,3,4\na,b,c,d,e" [ Dict.fromList [ ( "3", "d" ), ( "4", "e" ) ] ] <|
                    fieldsAfter "2"
            , test "fails if rest field is not unique" <|
                testCsvFail "1,2,3,3\na,b,c,d" <|
                    fieldsAfter "1"
            ]
        , describe "fieldsBetween"
            [ test "works" <|
                testCsv "1,2,3\na,b,c" [ Dict.fromList [ ( "2", "b" ) ] ] <|
                    fieldsBetween "1" "3"
            , test "still works if result is empty" <|
                testCsv "1,2,3\na,b,c" [ Dict.empty ] <|
                    fieldsBetween "1" "2"
            , test "fails if one of passed fields does not exist" <|
                testCsvFail "1,2,3\na,b,c" <|
                    fieldsBetween "4" "3"
            , test "fails if one of passed fields does not exist 2" <|
                testCsvFail "1,2,3\na,b,c" <|
                    fieldsBetween "1" "4"
            , test "fails if passed field is not unique" <|
                testCsvFail "1,1,3\na,b,c" <|
                    fieldsBetween "1" "3"
            , test "fails if passed field is not unique 2" <|
                testCsvFail "1,3,3\na,b,c" <|
                    fieldsBetween "1" "3"
            , test "works if passed field is unique" <|
                testCsv "1,1,2,3,4,5,5\na,b,c,d,e,f,g" [ Dict.fromList [ ( "3", "d" ) ] ] <|
                    fieldsBetween "2" "4"
            , test "fails if rest field is not unique" <|
                testCsvFail "1,2,2,3\na,b,c,d" <|
                    fieldsBetween "1" "3"
            ]
        , describe "int"
            [ test "converts string value into int value" <|
                testCsv "1,2,3\n10,20,30" [ 20 ] <|
                    int (index 1)
            , test "fails if value is float" <|
                testCsvFail "1,2,3\n10,20.5,30" <|
                    int (index 1)
            , test "fails if value is not a number" <|
                testCsvFail "1,2,3\n10,foo,30" <|
                    int (index 1)
            ]
        , describe "float"
            [ test "converts string value into float value" <|
                testCsv "1,2,3\n10,20.5,30" [ 20.5 ] <|
                    float (index 1)
            , test "fails if value is not float" <|
                testCsvFail "1,2,3\n10,foo,30" <|
                    float (index 1)
            ]
        , describe "string"
            [ test "succeeds if value is not empty" <|
                testCsv "1,2,3\n1,2,3" [ "2" ] <|
                    string (index 1)
            , test "still succeeds if value is empty" <|
                testCsv "1,2,3\n1,,3" [ "" ] <|
                    string (index 1)
            ]
        , describe "optional"
            [ test "results in Just if the value exists" <|
                testCsv "1,2,3\na,b,c" [ Just "b" ] <|
                    optional (index 1)
            , test "works for int" <|
                testCsv "1,2,3\n10,20,30" [ Just 20 ] <|
                    optional (int (index 1))
            , test "fails if conversion fails" <|
                testCsvFail "1,2,3\n10,foo,30" <|
                    optional (int (index 1))
            , test "fails on accessing non-unique column" <|
                testCsvFail "1,1,3\n10,20,30" <|
                    optional (field "1")
            , test "results in Nothing if value is empty" <|
                testCsv "1,2,3\n10,,30" [ Nothing ] <|
                    optional (index 1)
            , test "results in Nothing if value is empty and int is expected" <|
                testCsv "1,2,3\n10,,30" [ Nothing ] <|
                    optional (int (index 1))
            , test "results in Just if value is empty and string is expected" <|
                testCsv "1,2,3\n10,,30" [ Just "" ] <|
                    optional (string (index 1))
            , test "results in Nothing if index is out of bounds" <|
                testCsv "1,2,3\n10,20,30" [ Nothing ] <|
                    optional (index 3)
            , test "results in Nothing if field does not exist" <|
                testCsv "1,2,3\n10,20,30" [ Nothing ] <|
                    optional (field "4")
            ]
        , test "pipeline" <|
            testCsv "1,2,3\na,b,c\nd,e,f" [ ( "a", "b", "c" ), ( "d", "e", "f" ) ] <|
                succeed (,,)
                    |= index 0
                    |= field "2"
                    |= index 2
        , describe "map"
            [ test "works" <|
                testCsv "1,2,3\na,b,c" [ "B" ] <|
                    map String.toUpper (index 1)
            ]
        , describe "andThen"
            [ test "works" <|
                testCsv "1,2,3\na,b,c\nA,b,c" [ Just "b", Just "c" ] <|
                    (index 0
                        |> andThen
                            (\s ->
                                if s == "a" then
                                    map Just (index 1)
                                else
                                    map Just (index 2)
                            )
                    )
            , test "works2" <|
                testCsvFail "1,2,3\na,b,c" <|
                    (index 0
                        |> andThen
                            (\s ->
                                if s == "foo" then
                                    succeed ""
                                else
                                    fail ""
                            )
                    )
            , test "works with optional" <|
                testCsv "1,2,3\na,b,c" [ 1 ] <|
                    (optional (field "1")
                        |> andThen
                            (\maybe ->
                                case maybe of
                                    Just a ->
                                        succeed 1

                                    Nothing ->
                                        succeed 2
                            )
                    )
            , test "works with optional 2" <|
                testCsv "1,2,3\na,b,c" [ 2 ] <|
                    (optional (field "4")
                        |> andThen
                            (\maybe ->
                                case maybe of
                                    Just a ->
                                        succeed 1

                                    Nothing ->
                                        succeed 2
                            )
                    )
            , test "does not chain if first decoder fails" <|
                testCsvFail "1,2,3\na,b,c" <|
                    (index 10
                        |> andThen succeed
                    )
            ]
        , describe "run"
            [ test "does not cause error on empty source" <|
                testCsv "" [] <|
                    index 1
            ]
        , describe "runWithOptions"
            [ test "does not cause error on empty source" <|
                testCsvWithOptions defaultOptions "" [] <|
                    index 1
            , test "works with custom separator" <|
                testCsvWithOptions
                    { separator = ";"
                    , noHeader = False
                    }
                    "1;2;3\na;b;c"
                    [ "b" ]
                <|
                    index 1
            , test "works with noHeader option" <|
                testCsvWithOptions
                    { separator = ","
                    , noHeader = True
                    }
                    "a,b,c\nd,e,f"
                    [ "b", "e" ]
                <|
                    index 1
            , test "works with noHeader option 2" <|
                testCsvWithOptionsFail
                    { separator = ","
                    , noHeader = True
                    }
                    "a,b,c\nd,e,f"
                <|
                    field "dummy"
            ]
        , describe "runAll"
            [ test "does not cause error on empty source" <|
                testRunAll "" [] 0 <|
                    index 1
            , test "collect errors correctly" <|
                testRunAll "a,b\n1,2\n3,foo\n4,\n5" [ 2 ] 3 <|
                    int (index 1)
            , test "collect errors correctly 2" <|
                testRunAll "a,b\n1,2\n3,foo\n4,\n5" [ Just 2, Nothing, Nothing ] 1 <|
                    optional (int (field "b"))
            , test "collect errors correctly 3" <|
                testRunAll "a,b\n1,2\n3,foo\n4,\n5" [] 4 <|
                    int (field "c")
            , test "collect errors correctly 4" <|
                testRunAll "a,b\n1,2\n3,foo\n4,\n5" [ Nothing, Nothing, Nothing, Nothing ] 0 <|
                    optional (int (field "c"))
            , test "collect errors correctly 5" <|
                testRunAll "b,b\n1,2\n3,foo\n4,\n5" [] 4 <|
                    int (field "b")
            , test "collect errors correctly 6" <|
                testRunAll "b,b\n1,2\n3,foo\n4,\n5" [] 4 <|
                    optional (int (field "b"))
            ]
        ]


largeTable : String
largeTable =
    "1,2,3\n"
        ++ (List.range 1 10000
                |> List.map (\i -> "a,b,c")
                |> String.join "\n"
           )


largeTableTest : Test
largeTableTest =
    describe "Large Table"
        [ test "does not cause stack overflow" <|
            testCsv largeTable (List.repeat 10000 1) <|
                succeed 1
        ]



-- EXAMPLE


type alias User =
    { id : String
    , name : String
    , age : Int
    , mail : Maybe String
    }


userDecoder : Decoder User
userDecoder =
    succeed User
        |= field "id"
        |= field "name"
        |= int (field "age")
        |= optional (field "mail")


source : String
source =
    """
id,name,age,mail
1,John Smith,20,john@example.com
2,Jane Smith,19,
"""


example : Test
example =
    test "example works" <|
        testCsv source
            [ { id = "1", name = "John Smith", age = 20, mail = Just "john@example.com" }
            , { id = "2", name = "Jane Smith", age = 19, mail = Nothing }
            ]
            userDecoder
