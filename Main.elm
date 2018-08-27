module Main exposing (main)

import Navigation exposing (Location)
import Keyboard
import View
import Update exposing (Msg(..))
import Model exposing (Model)
import Route exposing (Route)
import Comic


init : Location -> ( Model, Cmd Msg )
init location =
    setRoute (Route.fromLocation location)


setRoute : Route -> ( Model, Cmd Msg )
setRoute route =
    let
        comicCmd =
            case route of
                Route.Latest ->
                    Comic.fetchLatest |> Cmd.map ComicResponse

                Route.ComicId id ->
                    Cmd.batch
                        -- Fetch latest comic, in order to store latest id
                        [ Comic.fetchLatest |> Cmd.map LatestComicId
                        , Comic.fetch id |> Cmd.map ComicResponse
                        ]
    in
        ( Model.initial route
        , comicCmd
        )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Keyboard.downs KeyMsg


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = init
        , view = View.view
        , update = Update.update
        , subscriptions = subscriptions
        }
