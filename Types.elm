module Types exposing (Model, Msg(..))

import Navigation exposing (Location)
import RemoteData exposing (WebData)
import Comic exposing (Comic)
import Route exposing (Route)
import Keyboard.Combo


type alias Model =
    { comic : WebData Comic
    , route : Route
    , lastId : Maybe Int
    , activeKeyCombos : Keyboard.Combo.Model Msg
    }


type Msg
    = ComicResponse (WebData Comic)
    | LatestComicId (WebData Comic)
    | LoadComic Int
    | PreviousComic
    | NextComic
    | RandomComic
    | FirstComic
    | LastComic
    | KeyComboMsg Keyboard.Combo.Msg
    | UrlChange Location
