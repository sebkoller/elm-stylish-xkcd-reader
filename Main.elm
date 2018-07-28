module Main exposing (main)

import Html exposing (Html)
import Html.Events exposing (onClick)
import Http
import HttpBuilder exposing (withExpect, send)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required)


type Model
    = ShowPost Post
    | ShowError String
    | Loading
    | Blank


initialModel : ( Model, Cmd Msg )
initialModel =
    ( Blank, Cmd.none )


type alias Post =
    { title : String
    }


type Msg
    = SetPost Post
    | SetError
    | LoadLatestPost


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetPost post ->
            ( ShowPost post, Cmd.none )

        SetError ->
            ( ShowError "Failed to fetch latest post", Cmd.none )

        LoadLatestPost ->
            ( Loading, getLatestPost )


view : Model -> Html Msg
view model =
    case model of
        ShowPost post ->
            Html.text post.title

        ShowError error ->
            Html.text error

        Loading ->
            Html.text "Loading post..."

        Blank ->
            Html.button [ onClick LoadLatestPost ]
                [ Html.text "Fetch latest post" ]


postDecoder : Decoder Post
postDecoder =
    decode Post
        |> required "title" Decode.string


getLatestPost : Cmd Msg
getLatestPost =
    HttpBuilder.get "https://cors-anywhere.herokuapp.com/https://xkcd.com/info.0.json"
        |> withExpect (Http.expectJson postDecoder)
        |> send handleRequestComplete


handleRequestComplete : Result Http.Error Post -> Msg
handleRequestComplete result =
    case result of
        Ok post ->
            SetPost post

        Err error ->
            SetError


main : Program Never Model Msg
main =
    Html.program
        { init = initialModel
        , view = view
        , update = update
        , subscriptions = (always Sub.none)
        }
