module CsvDecode
    exposing
        ( Decoder
        , Options
        , succeed
        , fail
        , field
        , index
        , int
        , float
        , optional
        , optionalString
        , (|=)
        , map
        , andThen
        , run
        , runWithOptions
        , defaultOptions
        )

{-| Decode CSV.

# Types
@docs Decoder, Options

# Primitives
@docs succeed, fail, field, index

# Conversion
@docs int, float, optional, optionalString

# Transform
@docs (|=), map, andThen

# Run
@docs run, runWithOptions, defaultOptions
-}

import Dict exposing (Dict)
import Csv


{-| -}
type Decoder a
    = Decoder (Header -> Int -> Item -> Result Error a)


type Error
    = ColumnNotFoundInHeader Int String
    | ColumnNotFoundInBody Int Int (Maybe String)
    | InvalidDataType Int String
      -- TODO: column index (and name) is available in most cases
    | AmbiguousField Int String
    | Fail Int String


{-| -}
type alias Options =
    { separator : String
    , noHeader : Bool
    }


type alias Header =
    Dict String Int


type alias Item =
    List String


type Column
    = Index Int
    | Field String


{-| -}
succeed : a -> Decoder a
succeed a =
    Decoder (\_ _ _ -> Ok a)


{-| -}
fail : String -> Decoder a
fail message =
    Decoder (\_ rowIndex _ -> Err (Fail rowIndex message))


{-| -}
field : String -> Decoder String
field key =
    Decoder
        (\header rowIndex item ->
            header
                |> Dict.get key
                |> Maybe.map (\index -> getByIndex (ColumnNotFoundInBody rowIndex index (Just key)) item index)
                |> Maybe.withDefault (Err (ColumnNotFoundInHeader rowIndex key))
        )


fieldsAfter : String -> Decoder (Dict String String)
fieldsAfter key =
    Decoder
        (\header rowIndex item ->
            Debug.crash "not implemented yet."
        )


fieldsBetween : String -> String -> Decoder (Dict String String)
fieldsBetween key1 key2 =
    Decoder
        (\header rowIndex item ->
            Debug.crash "not implemented yet."
        )


{-| -}
index : Int -> Decoder String
index colIndex =
    Decoder
        (\_ rowIndex item ->
            getByIndex (ColumnNotFoundInBody rowIndex colIndex Nothing) item colIndex
        )


getByIndex : Error -> Item -> Int -> Result Error String
getByIndex errMessage item i =
    item
        |> List.drop i
        |> List.head
        |> Result.fromMaybe errMessage


{-| -}
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


{-| -}
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


{-| -}
optional : Decoder a -> Decoder (Maybe a)
optional (Decoder f) =
    Decoder
        (\header rowIndex item ->
            f header rowIndex item
                |> Result.toMaybe
                |> Ok
        )


{-| -}
optionalString : Decoder String -> Decoder (Maybe String)
optionalString (Decoder f) =
    Decoder
        (\header rowIndex item ->
            case f header rowIndex item of
                Ok s ->
                    if String.trim s == "" then
                        Ok Nothing
                    else
                        Ok (Just s)

                Err e ->
                    Ok Nothing
        )


{-| -}
(|=) : Decoder (a -> b) -> Decoder a -> Decoder b
(|=) (Decoder transform) (Decoder f) =
    Decoder
        (\header rowIndex item ->
            Result.map2 (\t a -> t a)
                (transform header rowIndex item)
                (f header rowIndex item)
        )
infixl 5 |=


{-| -}
map : (a -> b) -> Decoder a -> Decoder b
map transform (Decoder f) =
    Decoder
        (\header rowIndex item ->
            f header rowIndex item |> Result.map transform
        )


{-| -}
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
    decodeItemsHelp decoder header items 1 []


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


{-| -}
run : Decoder a -> String -> Result String (List a)
run =
    runWithOptions defaultOptions


{-| -}
defaultOptions : Options
defaultOptions =
    { separator = ","
    , noHeader = False
    }


{-| -}
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
                Dict.empty
            else
                csv.headers
                    |> List.indexedMap (\i colName -> ( colName, i ))
                    |> Dict.fromList

        items =
            csv.records
    in
        decodeItems decoder header items
            |> Result.mapError formatError


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
