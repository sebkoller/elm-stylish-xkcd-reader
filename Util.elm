module Util exposing (isLast, isFirst)

import Types exposing (Model)
import Route


isLast : Model -> Bool
isLast model =
    case model.route of
        Route.Latest ->
            True

        Route.Comic id ->
            model.lastId
                |> Maybe.map (\lastId -> lastId == id)
                |> Maybe.withDefault True


isFirst : Model -> Bool
isFirst model =
    case model.route of
        Route.Comic id ->
            if id == 1 then
                True
            else
                False

        _ ->
            False
