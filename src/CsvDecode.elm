module CsvDecode
    exposing
        ( Decoder
        , Options
        , Error
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
        , formatError
        )

{-| Decode CSV.

# Types
@docs Decoder, Options, Error

# Primitives
@docs succeed, fail, field, index

# Conversion
@docs int, float, optional, optionalString

# Transform
@docs (|=), map, andThen

# Run
@docs run, runWithOptions, defaultOptions, formatError
-}

import Dict exposing (Dict)
import Csv


{-| -}
type Decoder a
    = Decoder (Header -> Item -> Result Error a)


{-| -}
type Error
    = ColumnNotFoundInHeader String
    | ColumnNotFoundInBody Int Int (Maybe String)
    | InvalidDataType Int String
      -- TODO: column index (and name) is available in most cases
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
    Decoder (\_ _ -> Ok a)


{-| -}
fail : String -> Decoder a
fail message =
    Decoder (\_ _ -> Err (Fail -1 message))


{-| -}
field : String -> Decoder String
field key =
    Decoder
        (\header item ->
            header
                |> Dict.get key
                |> Maybe.map (\index -> getByIndex (ColumnNotFoundInBody -1 index (Just key)) item index)
                |> Maybe.withDefault (Err (ColumnNotFoundInHeader key))
        )


{-| -}
formatError : Error -> String
formatError e =
    case e of
        ColumnNotFoundInHeader colName ->
            "column['" ++ colName ++ "'] does not exist in header"

        ColumnNotFoundInBody rowIndex colIndex colName ->
            "column['"
                ++ toString colIndex
                ++ "']"
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

        Fail rowIndex s ->
            s
                ++ " in record["
                ++ toString rowIndex
                ++ "]"


{-| -}
index : Int -> Decoder String
index index =
    Decoder
        (\_ item ->
            getByIndex (ColumnNotFoundInBody -1 index Nothing) item index
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
        (\header item ->
            f header item
                |> Result.andThen
                    (\s ->
                        String.toInt s
                            |> Result.mapError (InvalidDataType -1)
                    )
        )


{-| -}
float : Decoder String -> Decoder Float
float (Decoder f) =
    Decoder
        (\header item ->
            f header item
                |> Result.andThen
                    (\s ->
                        String.toFloat s
                            |> Result.mapError (InvalidDataType -1)
                    )
        )


{-| -}
optional : Decoder a -> Decoder (Maybe a)
optional (Decoder f) =
    Decoder
        (\header item ->
            f header item
                |> Result.toMaybe
                |> Ok
        )


{-| -}
optionalString : Decoder String -> Decoder (Maybe String)
optionalString (Decoder f) =
    Decoder
        (\header item ->
            case f header item of
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
        (\header item ->
            Result.map2 (\t a -> t a)
                (transform header item)
                (f header item)
        )
infixl 5 |=


{-| -}
map : (a -> b) -> Decoder a -> Decoder b
map transform (Decoder f) =
    Decoder
        (\header item ->
            f header item |> Result.map transform
        )


{-| -}
andThen : (a -> Decoder b) -> Decoder a -> Decoder b
andThen toDecoder (Decoder f) =
    Decoder
        (\header item ->
            f header item |> Result.andThen (\a -> decodeItem (toDecoder a) header item)
        )


decodeItem : Decoder a -> Header -> Item -> Result Error a
decodeItem (Decoder f) header item =
    f header item


decodeItems : Decoder a -> Header -> List Item -> Result Error (List a)
decodeItems decoder header items =
    decodeItemsHelp decoder header items 1 []


decodeItemsHelp : Decoder a -> Header -> List Item -> Int -> List a -> Result Error (List a)
decodeItemsHelp decoder header items index list =
    case items of
        [] ->
            Ok (List.reverse list)

        x :: xs ->
            case decodeItem decoder header x of
                Ok a ->
                    decodeItemsHelp decoder header xs (index + 1) (a :: list)

                Err e ->
                    Err e


{-| -}
run : Decoder a -> String -> Result Error (List a)
run =
    runWithOptions defaultOptions


{-| -}
defaultOptions : Options
defaultOptions =
    { separator = ","
    , noHeader = False
    }


{-| -}
runWithOptions : Options -> Decoder a -> String -> Result Error (List a)
runWithOptions options decoder source =
    let
        csv =
            Csv.parseWith options.separator source

        header =
            csv.headers
                |> List.indexedMap (\i colName -> ( colName, i ))
                |> Dict.fromList

        items =
            csv.records
    in
        decodeItems decoder header items
