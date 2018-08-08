module Model exposing (Model, initial)

import RemoteData exposing (WebData)
import Element exposing (Device)
import Comic exposing (Comic)
import Route exposing (Route)


type alias Model =
    { comic : WebData Comic
    , route : Route
    , latestId : Maybe Int
    , device : Device
    }


initial : Route -> Model
initial route =
    { comic = RemoteData.Loading
    , route = route
    , latestId = Nothing
    , device = Element.classifyDevice { width = 0, height = 0 }
    }
