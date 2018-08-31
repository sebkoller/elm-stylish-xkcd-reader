module View exposing (view)

import Http
import Html exposing (Html)
import Html.Attributes
import Element exposing (Element, el, row, column, paragraph)
import Element.Region
import Element.Input
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Color
import Date.Extra
import RemoteData
import FontAwesome
import Types exposing (Model, Msg(..))
import Comic exposing (Comic)
import Route exposing (Route)
import Util
import Json.Encode as Json
import VirtualDom exposing (node, property)


icon : FontAwesome.Icon -> Element msg
icon =
    Element.html << FontAwesome.icon


view : Model -> Html Msg
view model =
    Element.layout
        [ Background.color Color.white
        ]
    <|
        row
            [ Element.paddingXY 0 10
            , Element.alignTop
            , Font.family
                [ Font.typeface "Roboto"
                , Font.sansSerif
                ]
            ]
            [ Element.html FontAwesome.useSvg
            , Element.html robotoFont
            , Element.el [ Element.centerX, Element.width Element.fill ]
                (column [ Element.spacing 14 ]
                    (case model.comic of
                        RemoteData.Success comic ->
                            [ viewComicHeader comic
                            , viewNavigation model
                            , viewComicBody comic
                            ]

                        RemoteData.Failure error ->
                            [ viewRouteHeader model.route
                            , viewNavigation model
                            , viewError error
                            ]

                        RemoteData.Loading ->
                            [ viewRouteHeader model.route
                            , viewNavigation model
                            , Element.el [ Element.centerX, Element.padding 50 ]
                                (Element.html
                                    (FontAwesome.iconWithOptions
                                        FontAwesome.spinner
                                        FontAwesome.Solid
                                        [ FontAwesome.Animation FontAwesome.Spin
                                        , FontAwesome.Size FontAwesome.Large
                                        ]
                                        []
                                    )
                                )
                            ]

                        RemoteData.NotAsked ->
                            [ Element.text "Something went wrong..." ]
                    )
                )
            ]


viewComicHeader : Comic -> Element msg
viewComicHeader comic =
    viewHeader ("#" ++ toString comic.id ++ " - " ++ comic.title)


viewHeader : String -> Element msg
viewHeader heading =
    Element.el
        [ Element.Region.heading 1
        , Element.centerX
        , Font.size 30
        , Font.bold
        ]
        (Element.text heading)


viewRouteHeader : Route -> Element msg
viewRouteHeader route =
    case route of
        Route.Latest ->
            viewHeader "#"

        Route.Comic id ->
            viewHeader ("#" ++ toString id)


viewComicBody : Comic -> Element Msg
viewComicBody comic =
    column
        [ Element.Region.mainContent
        , Element.spacing 10
        ]
        [ Element.decorativeImage [ Element.centerX ]
            { src = comic.img }
        , el [ Font.size 16, Element.centerX ]
            (column
                [ Element.width (Element.minimum 300 Element.fill)
                , Element.width (Element.maximum 750 Element.fill)
                ]
                [ comicAttribute "Alt title: " comic.altTitle
                , comicAttribute
                    "Published on: "
                    (Date.Extra.toFormattedString "y-M-d" comic.publishedOn)
                ]
            )
        ]


viewNavigation : Model -> Element Msg
viewNavigation model =
    let
        isLoadingLatest =
            model.route == Route.Latest && not (RemoteData.isSuccess model.comic)

        isFirst =
            Util.isFirst model

        isLast =
            Util.isLast model

        lastIdMissing =
            isNothing model.lastId
    in
        el [ Element.centerX ]
            (row [ Element.spacing 5, Element.Region.navigation ]
                [ navButton
                    (isFirst || isLoadingLatest)
                    [ icon FontAwesome.angleDoubleLeft ]
                    FirstComic
                , navButton
                    (isFirst || isLoadingLatest)
                    [ icon FontAwesome.angleLeft
                    , Element.text " Previous"
                    ]
                    PreviousComic
                , navButton
                    lastIdMissing
                    [ icon FontAwesome.random, Element.text " Random" ]
                    RandomComic
                , navButton
                    isLast
                    [ Element.text "Next "
                    , icon FontAwesome.angleRight
                    ]
                    NextComic
                , navButton
                    isLast
                    [ icon FontAwesome.angleDoubleRight ]
                    LastComic
                ]
            )


viewError : Http.Error -> Element msg
viewError error =
    let
        errorText =
            case error of
                Http.Timeout ->
                    "A timeout has occured!"

                Http.NetworkError ->
                    "Trouble reaching the API!"

                Http.BadPayload _ _ ->
                    "The API returned an unexpected payload!"

                Http.BadStatus _ ->
                    "The API returned an error!"

                Http.BadUrl _ ->
                    "The API url seems to be misformed!"
    in
        viewHeader errorText


robotoFont : Html msg
robotoFont =
    (node "link"
        [ property "href"
            (Json.string "https://fonts.googleapis.com/css?family=Roboto:400,500")
        ]
        []
    )


navButton : Bool -> List (Element msg) -> msg -> Element msg
navButton disabled content msg =
    let
        disabledStyles =
            if disabled then
                [ Border.color (Color.rgba 0 0 0 0.37)
                , Element.mouseOver [ Background.color (Color.white) ]
                , Element.mouseDown [ Background.color (Color.white) ]
                , Font.color (Color.rgba 0 0 0 0.37)
                , htmlAttribute "cursor" "default"
                ]
            else
                []

        maybeMsg =
            if disabled then
                Nothing
            else
                Just msg
    in
        Element.Input.button
            ([ Border.color Color.black
             , Border.width 2
             , Element.paddingXY 14 9
             , Border.rounded 2
             , Font.size 14
             , Font.medium
             , Element.mouseOver [ Background.color (Color.rgba 158 158 158 0.2) ]
             , Element.mouseDown [ Background.color (Color.rgba 158 158 158 0.4) ]
             , htmlAttribute "box-shadow" "none"
             , htmlAttribute "text-transform" "uppercase"
             , htmlAttribute "user-select" "none"
             , htmlAttribute "-moz-user-select" "none"
             ]
                ++ disabledStyles
            )
            { label = row [ Element.spacing 10 ] content, onPress = maybeMsg }


htmlAttribute : String -> String -> Element.Attribute msg
htmlAttribute key value =
    Element.htmlAttribute (Html.Attributes.style [ ( key, value ) ])


comicAttribute : String -> String -> Element msg
comicAttribute label text =
    paragraph [ Element.width Element.fill ]
        [ el [ Font.bold ] (Element.text label)
        , Element.text text
        ]


viewIf : Bool -> Element msg -> Element msg
viewIf condition content =
    if condition then
        content
    else
        Element.text ""


isNothing : Maybe a -> Bool
isNothing m =
    case m of
        Nothing ->
            True

        Just _ ->
            False
