module Main exposing (main)

import Navigation exposing (Location)
import Keyboard
import Window
import View
import Update exposing (Msg(..))
import Model exposing (Model)
import Route exposing (Route)
import Comic
import Task


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
        , Cmd.batch
            [ comicCmd
            , Task.perform WindowResize Window.size
            ]
        )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Keyboard.downs KeyMsg
        , Window.resizes WindowResize
        ]


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = init
        , view = View.view
        , update = Update.update
        , subscriptions = subscriptions
        }
