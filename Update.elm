module Update exposing (update)

import Navigation
import Types exposing (Model, Msg(..))
import RemoteData exposing (WebData)
import Comic exposing (Comic)
import Keyboard.Combo
import Route exposing (Route)
import Random
import Util


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ComicResponse response ->
            case response of
                RemoteData.Success comic ->
                    if model.route == Route.Latest then
                        ( { model
                            | comic = response
                            , lastId = Just comic.id
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
                    ( { model | lastId = Just comic.id }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        LoadComic id ->
            ( { model
                | comic = RemoteData.Loading
                , route = (Route.ComicId id)
              }
            , Cmd.batch
                [ Comic.fetch id |> Cmd.map ComicResponse
                , Navigation.newUrl ("#" ++ toString id)
                ]
            )

        PreviousComic ->
            case model.route of
                Route.Latest ->
                    case model.lastId of
                        Just lastId ->
                            update (LoadComic (lastId - 1)) model

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

        FirstComic ->
            update (LoadComic 1) model

        LastComic ->
            case model.lastId of
                Just lastId ->
                    update (LoadComic lastId) model

                Nothing ->
                    ( model, Cmd.none )

        RandomComic ->
            case model.lastId of
                Just lastId ->
                    ( model, Random.generate LoadComic (Random.int 1 lastId) )

                Nothing ->
                    ( model, Cmd.none )

        KeyComboMsg msg ->
            let
                ( updatedKeys, comboCmd ) =
                    Keyboard.Combo.update msg model.activeKeyCombos
            in
                ( { model | activeKeyCombos = updatedKeys }, comboCmd )

        UrlChange location ->
            -- TODO do something
            ( model, Cmd.none )
