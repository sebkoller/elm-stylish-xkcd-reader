module Update exposing (Msg(..), update)

import Model exposing (Model)
import RemoteData exposing (WebData)
import Comic exposing (Comic)
import Element
import Navigation exposing (Location)
import Keyboard
import Route exposing (Route)
import Random
import Util
import Window


type Msg
    = ComicResponse (WebData Comic)
    | LatestComicId (WebData Comic)
    | LoadComic Int
    | PreviousComic
    | NextComic
    | RandomComic
    | KeyMsg Keyboard.KeyCode
    | UrlChange Location
    | WindowResize Window.Size


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ComicResponse response ->
            case response of
                RemoteData.Success comic ->
                    if model.route == Route.Latest then
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

        LatestComicId response ->
            case response of
                RemoteData.Success comic ->
                    ( { model | latestId = Just comic.id }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        LoadComic id ->
            ( { model
                | comic = RemoteData.Loading
                , route = (Route.ComicId id)
              }
            , Cmd.batch
                [ Comic.get id |> Cmd.map ComicResponse
                , Navigation.newUrl ("#" ++ toString id)
                ]
            )

        PreviousComic ->
            case model.route of
                Route.Latest ->
                    case model.latestId of
                        Just latestId ->
                            update (LoadComic (latestId - 1)) model

                        Nothing ->
                            ( model, Cmd.none )

                Route.ComicId id ->
                    update (LoadComic (id - 1)) model

        NextComic ->
            case model.route of
                Route.ComicId id ->
                    if not (Util.isLatest model) then
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
            case code of
                37 ->
                    -- left arrow
                    update PreviousComic model

                72 ->
                    -- h
                    update PreviousComic model

                39 ->
                    -- right arrow
                    update NextComic model

                76 ->
                    -- l
                    update NextComic model

                82 ->
                    -- r
                    update RandomComic model

                _ ->
                    ( model, Cmd.none )

        UrlChange location ->
            -- TODO do something
            ( model, Cmd.none )

        WindowResize size ->
            ( { model | device = Element.classifyDevice size }, Cmd.none )
