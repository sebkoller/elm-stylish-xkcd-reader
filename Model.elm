module Model exposing (Model, initial)

import RemoteData exposing (WebData)
import Comic exposing (Comic)
import Route exposing (Route)


type alias Model =
    { comic : WebData Comic
    , route : Route
    , lastId : Maybe Int
    }


initial : Route -> Model
initial route =
    { comic = RemoteData.Loading
    , route = route
    , lastId = Nothing
    }
