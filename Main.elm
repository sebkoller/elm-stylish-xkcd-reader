module Main exposing (main)

import Html as H exposing (Html)
import Html.Events as HE
import Html.Attributes as HA
import Http
import Date exposing (Date)
import Date.Extra
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required, resolve)
import Json.Decode.Extra exposing (parseInt)
import RemoteData exposing (WebData)


type alias Model =
    { comic : WebData Comic
    , route : Route
    , latestId : Maybe Int
    }


initialModel : ( Model, Cmd Msg )
initialModel =
    ( Model RemoteData.Loading Latest Nothing, getLatestComic )


type Route
    = Latest
    | Random
    | ComicId Int


type Msg
    = ComicResponse (WebData Comic)
    | LoadComic Int
    | PreviousComic
    | NextComic


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ComicResponse response ->
            case response of
                RemoteData.Success comic ->
                    if model.route == Latest then
                        ( { model
                            | comic = response
                            , latestId = Just comic.id
                          }
                        , Cmd.none
                        )
                    else
                        ( { model | comic = response }, Cmd.none )

                _ ->
                    ( { model | comic = response }, Cmd.none )

        LoadComic id ->
            ( { model
                | comic = RemoteData.Loading
                , route = (ComicId id)
              }
            , getComic id
            )

        PreviousComic ->
            case model.route of
                Latest ->
                    case model.latestId of
                        Just latestId ->
                            update (LoadComic (latestId - 1)) model

                        Nothing ->
                            ( model, Cmd.none )

                ComicId id ->
                    update (LoadComic (id - 1)) model

                Random ->
                    ( model, Cmd.none )

        NextComic ->
            case model.route of
                ComicId id ->
                    if not (isLatest model) then
                        update (LoadComic (id + 1)) model
                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )


isLatest : Model -> Bool
isLatest model =
    case model.route of
        Latest ->
            True

        ComicId id ->
            model.latestId
                |> Maybe.map (\latestId -> latestId == id)
                |> Maybe.withDefault False

        Random ->
            False


isFirst : Model -> Bool
isFirst model =
    case model.route of
        ComicId id ->
            if id == 1 then
                True
            else
                False

        _ ->
            False


view : Model -> Html Msg
view model =
    case model.comic of
        RemoteData.Success comic ->
            H.div []
                [ viewComic comic
                , viewNavigation model
                ]

        RemoteData.Failure error ->
            H.text <| toString error

        RemoteData.Loading ->
            H.text "Loading comic..."

        RemoteData.NotAsked ->
            H.button []
                [ H.text "Fetch latest comic" ]


viewComic : Comic -> Html Msg
viewComic comic =
    H.div []
        [ H.h4 [] [ H.text comic.title ]
        , H.div [] [ H.img [ HA.src comic.img ] [] ]
        , H.text comic.altTitle
        ]


viewNavigation : Model -> Html Msg
viewNavigation model =
    H.div []
        [ viewIf (not (isFirst model)) <| H.button [ HE.onClick PreviousComic ] [ H.text "Previous" ]
        , viewIf (not (isLatest model)) <|
            H.button [ HE.onClick NextComic ] [ H.text "Next" ]
        ]


type alias Comic =
    { id : Int
    , title : String
    , altTitle : String
    , img : String
    , date : Date
    }


comicDecoder : Decoder Comic
comicDecoder =
    decode toComicConstructor
        |> required "num" Decode.int
        |> required "title" Decode.string
        |> required "alt" Decode.string
        |> required "img" Decode.string
        |> required "year" parseInt
        |> required "month" parseInt
        |> required "day" parseInt
        |> resolve


toComicConstructor : Int -> String -> String -> String -> Int -> Int -> Int -> Decoder Comic
toComicConstructor id title altTitle img year month day =
    let
        -- mont
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
            , date = date
            }


urlBase : String
urlBase =
    "https://cors-anywhere.herokuapp.com/https://xkcd.com/"


urlFileName : String
urlFileName =
    "info.0.json"


getLatestComic : Cmd Msg
getLatestComic =
    Http.get (urlBase ++ urlFileName) comicDecoder
        |> RemoteData.sendRequest
        |> Cmd.map ComicResponse


getComic : Int -> Cmd Msg
getComic id =
    Http.get (urlBase ++ toString id ++ "/" ++ urlFileName) comicDecoder
        |> RemoteData.sendRequest
        |> Cmd.map ComicResponse



-- handleRequestComplete : Result Http.Error Comic -> Msg
-- handleRequestComplete result =
--     case result of
--         Ok post ->
--             SetComic post
--         Err error ->
--             SetError


viewIf : Bool -> Html msg -> Html msg
viewIf condition content =
    if condition then
        content
    else
        H.text ""


main : Program Never Model Msg
main =
    H.program
        { init = initialModel
        , view = view
        , update = update
        , subscriptions = (always Sub.none)
        }
