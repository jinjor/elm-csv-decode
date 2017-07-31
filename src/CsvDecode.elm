module CsvDecode
    exposing
        ( Decoder
        , Options
        , succeed
        , fail
        , field
        , index
        , fieldsAfter
        , fieldsBetween
        , int
        , float
        , string
        , optional
        , (|=)
        , map
        , andThen
        , run
        , runWithOptions
        , defaultOptions
        )

{-| CSV Decoder.

# Types
@docs Decoder, Options

# Primitives
@docs succeed, fail, field, index, fieldsAfter, fieldsBetween

# Conversion
@docs int, float, string, optional

# Transform
@docs (|=), map, andThen

# Run
@docs run, runWithOptions, defaultOptions
-}

import Dict exposing (Dict)
import Csv


-- TYPES


{-| Type of decoder. For example, `Decoder Int` decodes each line into integer value.
-}
type Decoder a
    = Decoder (Header -> Int -> Item -> Result Error a)


type Error
    = ColumnNotFoundInHeader Int String
    | ColumnNotFoundInBody Int Int (Maybe String)
    | EmptyValue Int String
    | InvalidDataType Int String
      -- TODO: column index (and name) is available in most cases
    | AmbiguousField Int String
    | Fail Int String


{-| You can pass options to `runWithOptions`.

- `separator : String` is used to separate each line into string list. Default is `,`.
- `noHeader : Bool` is used to indicate that csv has no header. Default is `False`.

-}
type alias Options =
    { separator : String
    , noHeader : Bool
    }


type alias Header =
    -- nameToIndex returns -1 if name is duplicated
    { nameToIndex : Dict String Int
    , indexToName : Dict Int String
    }


type alias Item =
    List String


type Column
    = Index Int
    | Field String



-- PRIMITIVES


{-| Make a decoder that always succeeds.

```elm
> Decode.run (succeed 100) "a,b,c\n1,2,3"
Ok [100]
```

This is useful paired with `(|=)` or `andThen`.

-}
succeed : a -> Decoder a
succeed a =
    Decoder (\_ _ _ -> Ok a)


{-| Makea a decoder that always fails.

```elm
> Decode.run (fail "ouch!") "a,b,c\n1,2,3"
Error "ouch! in record[0]"
```
This is useful paired with `andThen`.

-}
fail : String -> Decoder a
fail message =
    Decoder (\_ rowIndex _ -> Err (Fail rowIndex message))


{-| Decode value for given column name.

```elm
> Decode.run (field "b") "a,b,c\n1,2,3"
Ok ["2"]
```

**Note**: It succeeds only if the value is non-empty string. If you want to allow empty string, use `string`.

-}
field : String -> Decoder String
field key =
    Decoder
        (\header rowIndex item ->
            header.nameToIndex
                |> Dict.get key
                |> Maybe.map
                    (\index ->
                        if index == -1 then
                            Err (AmbiguousField rowIndex key)
                        else
                            getByIndex rowIndex (ColumnNotFoundInBody rowIndex index (Just key)) item index
                    )
                |> Maybe.withDefault (Err (ColumnNotFoundInHeader rowIndex key))
        )


{-| Decode value for given column index. If column names are available, use `field` instead.

```elm
> Decode.run (index 1) "a,b,c\n1,2,3"
Ok ["2"]
```

**Note**: It succeeds only if the value is non-empty string. If you want to allow empty string, use `string`.

-}
index : Int -> Decoder String
index colIndex =
    Decoder
        (\_ rowIndex item ->
            getByIndex rowIndex (ColumnNotFoundInBody rowIndex colIndex Nothing) item colIndex
        )


{-| Sometimes you don't fully know the name of columns, but you need to get key-value pairs.

```
key1,key2,unknown1,unknown2
1,2,a,b
```

In this case, you can do like this:

```elm
> Decode.run (fieldsAfter "key2") source
Ok [ Dict.fromList [ ("unknown1", "a"), ("unknown2", "b") ] ]
```

See also `fieldsBetween`.

-}
fieldsAfter : String -> Decoder (Dict String String)
fieldsAfter key =
    Decoder
        (\header rowIndex item ->
            case Dict.get key header.nameToIndex of
                Just -1 ->
                    Err (AmbiguousField rowIndex key)

                Just index ->
                    makeDictInRange (\i -> i > index) header rowIndex item

                Nothing ->
                    Err (ColumnNotFoundInHeader rowIndex key)
        )


{-| Sometimes you don't fully know the name of columns, but you need to get key-value pairs.

```
key1,key2,unknown1,unknown2,key3,key4
1,2,a,b,3,4
```

In this case, you can do like this:

```elm
> Decode.run (fieldsBetween "key2" "key3") source
Ok [ Dict.fromList [ ("unknown1", "a"), ("unknown2", "b") ] ]
```

See also `fieldsAfter`.

-}
fieldsBetween : String -> String -> Decoder (Dict String String)
fieldsBetween key1 key2 =
    Decoder
        (\header rowIndex item ->
            case ( Dict.get key1 header.nameToIndex, Dict.get key2 header.nameToIndex ) of
                ( Just -1, Just _ ) ->
                    Err (AmbiguousField rowIndex key1)

                ( Just _, Just -1 ) ->
                    Err (AmbiguousField rowIndex key2)

                ( Just index1, Just index2 ) ->
                    makeDictInRange (\i -> i > index1 && i < index2) header rowIndex item

                ( Nothing, _ ) ->
                    Err (ColumnNotFoundInHeader rowIndex key1)

                _ ->
                    Err (ColumnNotFoundInHeader rowIndex key2)
        )


makeDictInRange : (Int -> Bool) -> Header -> Int -> Item -> Result Error (Dict String String)
makeDictInRange isInRange header rowIndex item =
    item
        |> List.indexedMap
            (\i value ->
                if isInRange i then
                    header.indexToName
                        |> Dict.get i
                        |> Maybe.map (\key -> ( key, value ))
                else
                    Nothing
            )
        |> List.filterMap identity
        |> makeDictInRangeHelp rowIndex Dict.empty


makeDictInRangeHelp : Int -> Dict String String -> List ( String, String ) -> Result Error (Dict String String)
makeDictInRangeHelp rowIndex dict list =
    case list of
        [] ->
            Ok dict

        ( key, value ) :: rest ->
            if Dict.member key dict then
                Err (AmbiguousField rowIndex key)
            else
                makeDictInRangeHelp rowIndex (Dict.insert key value dict) rest


getByIndex : Int -> Error -> Item -> Int -> Result Error String
getByIndex rowIndex err item i =
    case (List.drop i >> List.head) item of
        Just s ->
            if String.trim s == "" then
                Err (EmptyValue rowIndex s)
            else
                Ok s

        Nothing ->
            Err err



-- CONVERSION


{-| Convert String value into Int value.

```elm
> Decode.run (int (field "b")) "a,b,c\n1,2,3"
Ok [2]
```

-}
int : Decoder String -> Decoder Int
int (Decoder f) =
    Decoder
        (\header rowIndex item ->
            f header rowIndex item
                |> Result.andThen
                    (\s ->
                        String.toInt s
                            |> Result.mapError (InvalidDataType rowIndex)
                    )
        )


{-| Convert String value into Float value.

```elm
> Decode.run (float (field "b")) "a,b,c\n1.5,2.5,3.5"
Ok [2.5]
```

-}
float : Decoder String -> Decoder Float
float (Decoder f) =
    Decoder
        (\header rowIndex item ->
            f header rowIndex item
                |> Result.andThen
                    (\s ->
                        String.toFloat s
                            |> Result.mapError (InvalidDataType rowIndex)
                    )
        )


{-| Allow empty string. Without this, empty value causes failure.

```elm
> Decode.run (field "b") "a,b,c\n1,,3"
Err "unexpected empty value in record[0]"

> Decode.run (string (field "b")) "a,b,c\n1,,3"
Ok [""]
```

-}
string : Decoder String -> Decoder String
string (Decoder f) =
    Decoder
        (\header rowIndex item ->
            case f header rowIndex item of
                Err (EmptyValue _ s) ->
                    Ok s

                x ->
                    x
        )


{-| If value exists, it will be wrapped with `Just`. If not, `Nothing` will be returned.

There are several situations where `Nothing` can be returned.

```elm
> Decode.run (optional (field "b")) "a,b,c\n1,2,3"
Ok [ Just "2" ]

-- column does not exist in *header*
> Decode.run (optional (field "b")) "a,b,c\n1,,3"
Ok [ Nothing ]

-- column does not exist in *body*
> Decode.run (optional (field "b")) "a,b,c\n1"
Ok [ Nothing ]

-- *value* is empty
> Decode.run (optional (field "b")) "a,b,c\n1,,3"
Ok [ Nothing ]
```

**Note**: Be careful if you use both `optional` and `string`.
Empty value will be regarded as existing string and never be `Nothing`.

```elm
> Decode.run (optional (string (field "b"))) "a,b,c\n1,,3"
Ok [ Just "" ]
```

-}
optional : Decoder a -> Decoder (Maybe a)
optional (Decoder f) =
    Decoder
        (\header rowIndex item ->
            case f header rowIndex item of
                Ok a ->
                    Ok (Just a)

                Err (ColumnNotFoundInHeader _ _) ->
                    Ok Nothing

                Err (ColumnNotFoundInBody _ _ _) ->
                    Ok Nothing

                Err (EmptyValue _ _) ->
                    Ok Nothing

                Err e ->
                    Err e
        )


{-| Make pipeline to get multiple values from a record.

For example, here is a user table:

```
id,name,age,mail
1,John Smith,20,john@example.com
2,Jane Smith,19,
```

You can make decoder for it like this:

```elm
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
```

-}
(|=) : Decoder (a -> b) -> Decoder a -> Decoder b
(|=) (Decoder transform) (Decoder f) =
    Decoder
        (\header rowIndex item ->
            Result.map2 (\t a -> t a)
                (transform header rowIndex item)
                (f header rowIndex item)
        )
infixl 5 |=


{-| Convert the result to another type.

```
> Decode.run (map toUpper (field "b")) "a,b,c\nfoo,bar,baz"
Ok ["BAR"]
```

-}
map : (a -> b) -> Decoder a -> Decoder b
map transform (Decoder f) =
    Decoder
        (\header rowIndex item ->
            f header rowIndex item |> Result.map transform
        )


{-| Enable to make a decoder that partially depends on its value.

For example, here is a list of users:

```
type,userId,name
guest,,
admin,1,Tom
```

The fields "userId" and "name" is needed only when the field "type" is "admin". So you write like this:

```
type User = Guest | Admin String String

userDecoder : Decoder User
userDecoder =
    field "type"
        |> andThen
            (\t ->
                if t == "guest" then
                    succeed Guest
                else if t == "admin"
                    succeed Admin
                        |= field "userId"
                        |= field "name"
                else
                    fail ("unknown type: " ++ t)
            )
```

-}
andThen : (a -> Decoder b) -> Decoder a -> Decoder b
andThen toDecoder (Decoder f) =
    Decoder
        (\header rowIndex item ->
            f header rowIndex item
                |> Result.andThen
                    (\a ->
                        decodeItem (toDecoder a) header rowIndex item
                    )
        )


decodeItem : Decoder a -> Header -> Int -> Item -> Result Error a
decodeItem (Decoder f) header rowIndex item =
    f header rowIndex item


decodeItems : Decoder a -> Header -> List Item -> Result Error (List a)
decodeItems decoder header items =
    decodeItemsHelp decoder header items 0 []


decodeItemsHelp : Decoder a -> Header -> List Item -> Int -> List a -> Result Error (List a)
decodeItemsHelp decoder header items rowIndex list =
    case items of
        [] ->
            Ok (List.reverse list)

        x :: xs ->
            case decodeItem decoder header rowIndex x of
                Ok a ->
                    decodeItemsHelp decoder header xs (rowIndex + 1) (a :: list)

                Err e ->
                    Err e


{-| Run the decoder. The result is either list of items or error message.

```elm
> Decode.run (field "b") "a,b,c\n1,2,3"
Ok ["2"]
```

This parses the basic comma-separated CSV with header. For more advanced case, see `runWithOptions`.

-}
run : Decoder a -> String -> Result String (List a)
run =
    runWithOptions defaultOptions


{-| The default options are defined as below.

```
defaultOptions =
    { separator = ","
    , noHeader = False
    }
```

-}
defaultOptions : Options
defaultOptions =
    { separator = ","
    , noHeader = False
    }


{-| Run decoder with options.

```elm
> Decode.runWithOptions { separator = ";", noHeader = True } (index 1) "a;b;c"
Ok ["b"]
```

- `separator : String` is used to separate each line into string list. Default is `,`.
- `noHeader : Bool` is used to indicate that csv has no header. Default is `False`.

-}
runWithOptions : Options -> Decoder a -> String -> Result String (List a)
runWithOptions options decoder source =
    let
        realSource =
            if options.noHeader then
                "dummy\n" ++ source
            else
                source

        csv =
            Csv.parseWith options.separator realSource

        header =
            if options.noHeader then
                Header Dict.empty Dict.empty
            else
                Header
                    (makeNameToIndexDict csv.headers)
                    (csv.headers
                        |> List.indexedMap (,)
                        |> Dict.fromList
                    )

        items =
            csv.records
    in
        decodeItems decoder header items
            |> Result.mapError formatError


makeNameToIndexDict : List String -> Dict String Int
makeNameToIndexDict names =
    makeNameToIndexDictHelp 0 names Dict.empty


makeNameToIndexDictHelp : Int -> List String -> Dict String Int -> Dict String Int
makeNameToIndexDictHelp index names dict =
    case names of
        [] ->
            dict

        name :: rest ->
            makeNameToIndexDictHelp
                (index + 1)
                rest
                (dict
                    |> Dict.update name
                        (\maybeValue ->
                            case maybeValue of
                                Just _ ->
                                    Just -1

                                Nothing ->
                                    Just index
                        )
                )


formatError : Error -> String
formatError e =
    case e of
        ColumnNotFoundInHeader rowIndex colName ->
            "column '"
                ++ colName
                ++ "' does not exist in header in record["
                ++ toString rowIndex
                ++ "]"

        ColumnNotFoundInBody rowIndex colIndex colName ->
            "column["
                ++ toString colIndex
                ++ "]"
                ++ (case colName of
                        Just name ->
                            " namely '" ++ name ++ "'"

                        Nothing ->
                            ""
                   )
                ++ " does not exist in record["
                ++ toString rowIndex
                ++ "]"

        EmptyValue rowIndex s ->
            "unexpected empty value in record["
                ++ toString rowIndex
                ++ "]"

        InvalidDataType rowIndex mes ->
            mes
                ++ " in record["
                ++ toString rowIndex
                ++ "]"

        AmbiguousField rowIndex key ->
            "ambiguous field '"
                ++ key
                ++ "' was accessed in record["
                ++ toString rowIndex
                ++ "]"

        Fail rowIndex s ->
            s
                ++ " in record["
                ++ toString rowIndex
                ++ "]"
