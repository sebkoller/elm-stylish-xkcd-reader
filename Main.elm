module Main exposing (main)

import Html exposing (Html)
import Html.Attributes
import Element exposing (Element, el)
import Element.Region
import Element.Input
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Color
import Http
import Date exposing (Date)
import Date.Extra
import Random
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required, resolve)
import Json.Decode.Extra exposing (parseInt)
import RemoteData exposing (WebData)
import Navigation exposing (Location)
import UrlParser as Url exposing (Parser)
import Keyboard
import FontAwesome


type alias Model =
    { comic : WebData Comic
    , route : Route
    , latestId : Maybe Int
    }


init : Location -> ( Model, Cmd Msg )
init location =
    -- if String.isEmpty location.hash then
    --     ( Model RemoteData.Loading Latest Nothing, getLatestComic )
    -- if String.isEmpty location.hash then
    --     String.
    --     ( Model RemoteData.Loading (ComicId location.hash) Nothing, getLatestComic )
    -- set (fromLocation location)
    -- setRoute : Maybe route -> (Model,
    setRoute (fromLocation location)


setRoute : Maybe Route -> ( Model, Cmd Msg )
setRoute maybeRoute =
    let
        latest =
            ( Model RemoteData.Loading Latest Nothing, getLatestComic )
    in
        case maybeRoute of
            Nothing ->
                latest

            Just Latest ->
                latest

            Just Random ->
                latest

            Just (ComicId id) ->
                ( Model RemoteData.Loading (ComicId id) Nothing, getComic id )


fromLocation : Location -> Maybe Route
fromLocation location =
    if String.isEmpty location.hash then
        Just Latest
    else
        Url.parseHash route location


route : Parser (Route -> a) a
route =
    Url.oneOf
        [ Url.map Latest (Url.s "")
        , Url.map ComicId (Url.int)
        ]


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
    | KeyMsg Keyboard.KeyCode
    | UrlChange Location


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
            , Cmd.batch
                [ getComic id
                , Navigation.newUrl ("#" ++ toString id)
                ]
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

        KeyMsg code ->
            let
                _ =
                    Debug.log "code" code
            in
                case code of
                    37 ->
                        update PreviousComic model

                    39 ->
                        update NextComic model

                    28 ->
                        update RandomComic model

                    _ ->
                        ( model, Cmd.none )

        UrlChange location ->
            let
                _ =
                    Debug.log "loc" location.hash
            in
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


icon : FontAwesome.Icon -> Element msg
icon =
    Element.html << FontAwesome.icon


view : Model -> Html Msg
view model =
    Element.layout
        [ Background.color Color.white
        , Element.width Element.fill
        ]
    <|
        case model.comic of
            RemoteData.Success comic ->
                Element.el [ Element.centerX ]
                    (Element.column [ Element.spacing 14 ]
                        [ viewComicHeader comic
                        , viewNavigation model
                        , viewComicBody comic
                        , Element.html FontAwesome.useSvg
                        ]
                    )

            RemoteData.Failure error ->
                Element.text <| toString error

            RemoteData.Loading ->
                Element.text "Loading comic..."

            RemoteData.NotAsked ->
                Element.text "Something went wrong..."


viewComicHeader : Comic -> Element Msg
viewComicHeader comic =
    Element.el
        [ Element.Region.heading 1
        , Element.centerX
        , Font.size 30
        , Font.bold
        ]
        (Element.text ("#" ++ toString comic.id ++ " - " ++ comic.title))


viewComicBody : Comic -> Element Msg
viewComicBody comic =
    Element.column
        [ Element.Region.mainContent
        , Element.spacing 10
        ]
        [ Element.image [ Element.centerX ]
            { src = comic.img, description = "comic image" }
        , comicAttribute "Alt title: " comic.altTitle
        , comicAttribute
            "Published on: "
            (Date.Extra.toFormattedString "y-M-d" comic.publishedOn)

        -- , (case comic.transcript of
        --     Just transcript ->
        --         Element.paragraph []
        --             [ el [ Font.bold ] (Element.text "Transcript: ")
        --             , Element.html (Html.pre [] [ Html.text transcript ])
        --             ]
        --     Nothing ->
        --         Element.none
        --   )
        ]


viewNavigation : Model -> Element Msg
viewNavigation model =
    el [ Element.centerX ]
        (Element.row [ Element.spacing 5, Element.Region.navigation ]
            [ navButton
                (isFirst model)
                [ icon FontAwesome.longArrowAltLeft
                , Element.text " Previous"
                ]
                PreviousComic
            , navButton False [ icon FontAwesome.random, Element.text " Random" ] RandomComic
            , navButton
                (isLatest model)
                [ Element.text "Next "
                , icon FontAwesome.longArrowAltRight
                ]
                NextComic
            ]
        )


navButton : Bool -> List (Element msg) -> msg -> Element msg
navButton disabled content msg =
    let
        disabledStyles =
            if disabled then
                [ Border.color (Color.rgba 0 0 0 0.37)
                , Element.mouseOver [ Background.color (Color.white) ]
                , Element.focused [ Background.color (Color.white) ]
                , Font.color (Color.rgba 0 0 0 0.37)
                , htmlAttribute "cursor" "default"
                ]
            else
                []
    in
        Element.Input.button
            ([ --Background.color Color.gray
               Border.color Color.darkBlue
             , Border.width 2
             , Element.paddingXY 14 9
             , Border.rounded 2
             , Font.size 14
             , Element.mouseOver [ Background.color (Color.rgba 158 158 158 0.2) ]
             , Element.mouseDown [ Background.color (Color.rgba 158 158 158 0.4) ]
             , Element.focused [ Background.color (Color.rgba 0 0 0 0.12) ]
             , htmlAttribute "text-transform" "uppercase"
             , htmlAttribute "user-select" "none"
             , htmlAttribute "-moz-user-select" "none"
             ]
                ++ disabledStyles
            )
            { label = Element.row [] content, onPress = Just msg }


htmlAttribute : String -> String -> Element.Attribute msg
htmlAttribute key value =
    Element.htmlAttribute (Html.Attributes.style [ ( key, value ) ])


comicAttribute : String -> String -> Element msg
comicAttribute label text =
    Element.paragraph [ Font.size 16 ]
        [ el [ Font.bold ] (Element.text label)
        , Element.text text
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
    "https://cors.io/?https://xkcd.com/"


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


viewIf : Bool -> Element msg -> Element msg
viewIf condition content =
    if condition then
        content
    else
        Element.text ""


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


subscriptions : Model -> Sub Msg
subscriptions _ =
    Keyboard.downs KeyMsg


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
