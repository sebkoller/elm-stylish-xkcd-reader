module Route exposing (Route(..), fromLocation)

import Navigation exposing (Location)
import UrlParser as Url exposing (Parser)


type Route
    = Latest
    | ComicId Int


fromLocation : Location -> Route
fromLocation location =
    -- if String.isEmpty location.hash then
    --     Just Latest
    -- else
    Url.parseHash matcher location
        |> Maybe.withDefault Latest


matcher : Parser (Route -> a) a
matcher =
    Url.oneOf
        [ Url.map Latest Url.top
        , Url.map ComicId Url.int
        ]
