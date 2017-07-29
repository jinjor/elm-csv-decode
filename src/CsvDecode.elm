module CsvDecode exposing (..)

import Dict exposing (Dict)
import Csv


type alias Header =
    Dict String Int


type alias Item =
    List String


type Decoder a
    = Decoder (Header -> Item -> Result String a)


type Column
    = Index Int
    | Field String


field : String -> Decoder String
field key =
    Decoder
        (\header item ->
            header
                |> Dict.get key
                |> Maybe.map (\index -> getByIndex ("column['" ++ key ++ "'] does not exist in item") item index)
                |> Maybe.withDefault (Err ("column['" ++ key ++ "'] does not exist in header"))
        )


index : Int -> Decoder String
index index =
    Decoder
        (\_ item ->
            getByIndex ("column[" ++ toString index ++ "] does not exist in item") item index
        )


getByIndex : String -> Item -> Int -> Result String String
getByIndex errMessage item i =
    item
        |> List.drop i
        |> List.head
        |> Result.fromMaybe errMessage


map : (a -> b) -> Decoder a -> Decoder b
map transform (Decoder f) =
    Decoder
        (\header item ->
            f header item |> Result.map transform
        )


andThen : (a -> Decoder b) -> Decoder a -> Decoder b
andThen toDecoder (Decoder f) =
    Decoder
        (\header item ->
            f header item |> Result.andThen (\a -> decodeItem (toDecoder a) header item)
        )


decodeItem : Decoder a -> Header -> Item -> Result String a
decodeItem (Decoder f) header item =
    f header item


decodeItems : Decoder a -> Header -> List Item -> Result String (List a)
decodeItems decoder header items =
    -- TODO: TCO
    case items of
        [] ->
            Ok []

        x :: xs ->
            decodeItem decoder header x
                |> Result.andThen
                    (\a ->
                        decodeItems decoder header xs
                            |> Result.map
                                (\tail ->
                                    a :: tail
                                )
                    )


int : Decoder String -> Decoder Int
int (Decoder f) =
    Decoder
        (\header item ->
            f header item |> Result.andThen String.toInt
        )


float : Decoder String -> Decoder Float
float (Decoder f) =
    Decoder
        (\header item ->
            f header item |> Result.andThen String.toFloat
        )


optional : Decoder a -> Decoder (Maybe a)
optional (Decoder f) =
    Decoder
        (\header item ->
            f header item
                |> Result.toMaybe
                |> Ok
        )


succeed : a -> Decoder a
succeed a =
    Decoder (\_ _ -> Ok a)


fail : String -> Decoder a
fail message =
    Decoder (\_ _ -> Err message)


(|=) : Decoder (a -> b) -> Decoder a -> Decoder b
(|=) (Decoder transform) (Decoder f) =
    Decoder
        (\header item ->
            Result.map2 (\t a -> t a)
                (transform header item)
                (f header item)
        )
infixl 5 |=


run : Decoder a -> String -> Result String (List a)
run decoder source =
    let
        csv =
            Csv.parse source

        header =
            csv.headers
                |> List.indexedMap (\i colName -> ( colName, i ))
                |> Dict.fromList

        items =
            csv.records
    in
        decodeItems decoder header items
