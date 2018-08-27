module View exposing (view)

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
import Model exposing (Model)
import Comic exposing (Comic)
import Route exposing (Route)
import Update exposing (Msg(..))
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
                            [ Element.text <| toString error ]

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

        Route.ComicId id ->
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
    el [ Element.centerX ]
        (row [ Element.spacing 5, Element.Region.navigation ]
            [ navButton
                (Util.isFirst model)
                [ icon FontAwesome.angleDoubleLeft ]
                FirstComic
            , navButton
                (Util.isFirst model)
                [ icon FontAwesome.angleLeft
                , Element.text " Previous"
                ]
                PreviousComic
            , navButton False [ icon FontAwesome.random, Element.text " Random" ] RandomComic
            , navButton
                (Util.isLatest model)
                [ Element.text "Next "
                , icon FontAwesome.angleRight
                ]
                NextComic
            , navButton
                (Util.isLatest model)
                [ icon FontAwesome.angleDoubleRight ]
                LastComic
            ]
        )


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

                -- , Element.focused [ Background.color (Color.white) ]
                , Font.color (Color.rgba 0 0 0 0.37)
                , htmlAttribute "cursor" "default"
                ]
            else
                []
    in
        Element.Input.button
            ([ Border.color Color.darkBlue
             , Border.width 2
             , Element.paddingXY 14 9
             , Border.rounded 2
             , Font.size 14
             , Font.medium
             , Element.mouseOver [ Background.color (Color.rgba 158 158 158 0.2) ]
             , Element.mouseDown [ Background.color (Color.rgba 158 158 158 0.4) ]

             -- , Element.focused [ Background.color (Color.rgba 0 0 0 0.12) ]
             , htmlAttribute "box-shadow" "none"
             , htmlAttribute "text-transform" "uppercase"
             , htmlAttribute "user-select" "none"
             , htmlAttribute "-moz-user-select" "none"
             ]
                ++ disabledStyles
            )
            { label = row [ Element.spacing 10 ] content, onPress = Just msg }


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
