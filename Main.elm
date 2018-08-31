module Main exposing (main)

import Navigation exposing (Location)
import RemoteData
import Keyboard.Combo
import View
import Update
import Types exposing (Model, Msg(..))
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
                    Comic.fetchLatest |> Cmd.map LatestComicResponse

                Route.Comic id ->
                    Cmd.batch
                        -- Fetch latest comic, in order to store latest id
                        [ Comic.fetchLatest |> Cmd.map LatestComicId
                        , Comic.fetch id |> Cmd.map ComicResponse
                        ]
    in
        ( initialModel route
        , comicCmd
        )


initialModel : Route -> Model
initialModel route =
    { comic = RemoteData.Loading
    , route = route
    , lastId = Nothing
    , activeKeyCombos = (Keyboard.Combo.init keyCombos KeyComboMsg)
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Keyboard.Combo.subscriptions model.activeKeyCombos


keyCombos : List (Keyboard.Combo.KeyCombo Msg)
keyCombos =
    [ Keyboard.Combo.combo1 (Keyboard.Combo.left) PreviousComic
    , Keyboard.Combo.combo1 (Keyboard.Combo.h) PreviousComic
    , Keyboard.Combo.combo1 (Keyboard.Combo.right) NextComic
    , Keyboard.Combo.combo1 (Keyboard.Combo.l) NextComic
    , Keyboard.Combo.combo2 ( Keyboard.Combo.shift, Keyboard.Combo.g ) LastComic
    , Keyboard.Combo.combo1 (Keyboard.Combo.g) FirstComic
    , Keyboard.Combo.combo1 (Keyboard.Combo.r) RandomComic
    ]


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = init
        , view = View.view
        , update = Update.update
        , subscriptions = subscriptions
        }
