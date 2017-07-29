module Tests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Test exposing (..)
import CsvDecode exposing (..)


testCsv : String -> List a -> Decoder a -> (() -> Expectation)
testCsv source exp decoder =
    \_ ->
        case CsvDecode.run decoder source of
            Ok actual ->
                Expect.equal exp actual

            Err e ->
                Expect.fail ("fail to decode: " ++ formatError e)


testCsvFail : String -> Decoder a -> (() -> Expectation)
testCsvFail source decoder =
    \_ ->
        case CsvDecode.run decoder source of
            Ok actual ->
                Expect.fail ("unexpectedly succeeded: " ++ toString actual)

            Err _ ->
                Expect.pass


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
            ]
        , describe "index"
            [ test "works" <|
                testCsv "1,2,3\na,b,c" [ "b" ] <|
                    index 1
            , test "still works if value is empty" <|
                testCsv "1,2,3\na,,c" [ "" ] <|
                    index 1
            , test "fails if it is out of range" <|
                testCsvFail "1,2,3\na,b,c" <|
                    index 3
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
        , describe "optional"
            [ test "results in Just if the value exists" <|
                testCsv "1,2,3\na,b,c" [ Just "b" ] <|
                    optional (index 1)
            , test "works for int" <|
                testCsv "1,2,3\n10,20,30" [ Just 20 ] <|
                    optional (int (index 1))
            , test "results in Nothing if conversion fails" <|
                testCsv "1,2,3\n10,fooo,30" [ Nothing ] <|
                    optional (int (index 1))
            , test "results in Just if value is empty string" <|
                testCsv "1,2,3\n10,,30" [ Just "" ] <|
                    optional (index 1)
            , test "results in Nothing if index is out of bounds" <|
                testCsv "1,2,3\n10,20,30" [ Nothing ] <|
                    optional (index 3)
            , test "results in Nothing if field does not exist" <|
                testCsv "1,2,3\n10,20,30" [ Nothing ] <|
                    optional (field "4")
            ]
        , describe "optionalString"
            [ test "results in Just if the value exists" <|
                testCsv "1,2,3\na,b,c" [ Just "b" ] <|
                    optionalString (index 1)
            , test "results in Nothing if value is empty string" <|
                testCsv "1,2,3\na,,b" [ Nothing ] <|
                    optionalString (index 1)
            , test "results in Nothing if index is out of bounds" <|
                testCsv "1,2,3\n10,20,30" [ Nothing ] <|
                    optionalString (index 3)
            , test "results in Nothing if field does not exist" <|
                testCsv "1,2,3\n10,20,30" [ Nothing ] <|
                    optionalString (field "4")
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
id,name,age
1,John Smith,20
2,Jane Smith,19
"""


example : Test
example =
    test "example works" <|
        testCsv source
            [ { id = "1", name = "John Smith", age = 20, mail = Nothing }
            , { id = "2", name = "Jane Smith", age = 19, mail = Nothing }
            ]
            userDecoder
