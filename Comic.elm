module Comic exposing (Comic, getLatest, get)

import Http
import Date exposing (Date)
import Date.Extra
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required, resolve)
import Json.Decode.Extra exposing (parseInt)
import RemoteData exposing (WebData)


type alias Comic =
    { id : Int
    , title : String
    , altTitle : String
    , img : String
    , publishedOn : Date
    , transcript : Maybe String
    }


comicDecoder : Decoder Comic
comicDecoder =
    decode toComicConstructor
        |> required "num" Decode.int
        |> required "title" Decode.string
        |> required "alt" Decode.string
        |> required "img" Decode.string
        |> required "transcript" possiblyEmptyString
        |> required "year" parseInt
        |> required "month" parseInt
        |> required "day" parseInt
        |> resolve


toComicConstructor :
    Int
    -> String
    -> String
    -> String
    -> Maybe String
    -> Int
    -> Int
    -> Int
    -> Decoder Comic
toComicConstructor id title altTitle img transcript year month day =
    let
        date =
            Date.Extra.fromCalendarDate
                year
                (Date.Extra.numberToMonth month)
                day
    in
        Decode.succeed
            { id = id
            , title = title
            , altTitle = altTitle
            , img = img
            , publishedOn = date
            , transcript = transcript
            }


possiblyEmptyString : Decoder (Maybe String)
possiblyEmptyString =
    let
        emptyStringToNothing string =
            if String.isEmpty string then
                Nothing
            else
                Just string
    in
        Decode.map emptyStringToNothing Decode.string


urlBase : String
urlBase =
    "https://cors.io/?https://xkcd.com/"


urlFileName : String
urlFileName =
    "info.0.json"


getLatest : Cmd (WebData Comic)
getLatest =
    Http.get (urlBase ++ urlFileName) comicDecoder
        |> RemoteData.sendRequest


get : Int -> Cmd (WebData Comic)
get id =
    Http.get (urlBase ++ toString id ++ "/" ++ urlFileName) comicDecoder
        |> RemoteData.sendRequest
