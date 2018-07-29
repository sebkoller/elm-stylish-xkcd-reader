module Main exposing (main)

import Html as H exposing (Html)
import Html.Events as HE
import Html.Attributes as HA
import Http
import Date exposing (Date)
import Date.Extra
import Random
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
    | RandomComic


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

        RandomComic ->
            case model.latestId of
                Just lastId ->
                    ( model, Random.generate LoadComic (Random.int 1 lastId) )

                Nothing ->
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
                [ viewComicHeader comic
                , viewNavigation model
                , viewComicBody comic
                ]

        RemoteData.Failure error ->
            H.text <| toString error

        RemoteData.Loading ->
            H.text "Loading comic..."

        RemoteData.NotAsked ->
            H.button []
                [ H.text "Fetch latest comic" ]


viewComicHeader : Comic -> Html Msg
viewComicHeader comic =
    H.div []
        [ H.h4 [] [ H.text (toString comic.id ++ " - " ++ comic.title) ]
        ]


viewComicBody : Comic -> Html Msg
viewComicBody comic =
    H.div []
        [ H.div [] [ H.img [ HA.src comic.img ] [] ]
        , H.div []
            [ H.b [] [ H.text "Alt title: " ]
            , H.text comic.altTitle
            ]
        , H.div []
            [ H.b [] [ H.text "Published on: " ]
            , H.text (Date.Extra.toFormattedString "y-M-d" comic.publishedOn)
            ]
        , (case comic.transcript of
            Just transcript ->
                H.div []
                    [ H.b [] [ H.text "Transcript: " ]
                    , H.pre [] [ H.text transcript ]
                    ]

            Nothing ->
                H.text ""
          )
        ]


viewNavigation : Model -> Html Msg
viewNavigation model =
    H.div []
        [ viewIf (not (isFirst model)) <| H.button [ HE.onClick PreviousComic ] [ H.text "Previous" ]
        , H.button [ HE.onClick RandomComic ] [ H.text "Random" ]
        , viewIf (not (isLatest model)) <|
            H.button [ HE.onClick NextComic ] [ H.text "Next" ]
        ]


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


viewIf : Bool -> Html msg -> Html msg
viewIf condition content =
    if condition then
        content
    else
        H.text ""


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


main : Program Never Model Msg
main =
    H.program
        { init = initialModel
        , view = view
        , update = update
        , subscriptions = (always Sub.none)
        }
