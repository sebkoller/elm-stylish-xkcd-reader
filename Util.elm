module Util exposing (isLatest, isFirst)

import Types exposing (Model)
import Route


isLatest : Model -> Bool
isLatest model =
    case model.route of
        Route.Latest ->
            True

        Route.ComicId id ->
            model.lastId
                |> Maybe.map (\lastId -> lastId == id)
                |> Maybe.withDefault True


isFirst : Model -> Bool
isFirst model =
    case model.route of
        Route.ComicId id ->
            if id == 1 then
                True
            else
                False

        _ ->
            False
