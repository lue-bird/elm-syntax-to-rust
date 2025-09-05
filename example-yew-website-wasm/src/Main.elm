module Main exposing (Event, State, initialState, update, view)

import Html
import Html.Attributes
import Html.Events
import Json.Decode
import Svg
import Svg.Attributes


type alias State =
    { count : Int
    , mouseX : Int
    , mouseY : Int
    , mouseTrail : List { x : Int, y : Int }
    }


type Event
    = CounterClicked
    | MouseMoved { x : Int, y : Int }


initialState : () -> State
initialState () =
    { count = 0
    , mouseX = 0
    , mouseY = 0
    , mouseTrail = []
    }


view : State -> Html.Html Event
view state =
    Html.div
        [ Html.Events.on "mousemove"
            (Json.Decode.map2 (\x y -> MouseMoved { x = x, y = y })
                (Json.Decode.field "clientX" Json.Decode.int)
                (Json.Decode.field "clientY" Json.Decode.int)
            )
        , Html.Attributes.style "width" "100vw"
        , Html.Attributes.style "height" "100vh"
        , Html.Attributes.style "box-sizing" "border-box"
        ]
        [ Html.div
            [ Html.Attributes.style "position" "absolute"
            ]
            [ Html.h1 []
                [ Html.text "Hello, wanderer!" ]
            , Html.p []
                [ Html.text
                    ("You're at x="
                        ++ (state.mouseX |> String.fromInt)
                        ++ " y="
                        ++ (state.mouseY |> String.fromInt)
                    )
                ]
            , Html.button
                [ Html.Events.onClick CounterClicked
                , Html.Attributes.style "padding" "20px"
                ]
                [ Html.text (String.fromInt state.count)
                ]
            , Html.br [] []
            , Html.br [] []
            , Html.a
                [ Html.Attributes.title "Evan Czaplicki, Public domain, via Wikimedia Commons"
                , Html.Attributes.href "https://commons.wikimedia.org/wiki/File:Elm_logo.svg"
                ]
                [ Html.img
                    [ Html.Attributes.src "https://upload.wikimedia.org/wikipedia/commons/thumb/f/f3/Elm_logo.svg/256px-Elm_logo.svg.png?20160911065740"
                    , Html.Attributes.alt "the elm logo"
                    , Html.Attributes.width 100
                    ]
                    [ Html.text "the elm logo" ]
                ]
            ]
        , Svg.svg
            [ Svg.Attributes.style "position: absolute"
            , Svg.Attributes.x "0"
            , Svg.Attributes.y "0"
            , Svg.Attributes.width "100vw"
            , Svg.Attributes.height "100vh"
            , -- omitting viewBox roughly means
              -- Svg.Attributes.viewBox "0 0 100% 100%"
              Svg.Attributes.pointerEvents "none"
            ]
            [ Svg.polyline
                [ Svg.Attributes.fill "none"
                , Svg.Attributes.stroke "green"
                , Svg.Attributes.points
                    (state.mouseTrail
                        |> List.map
                            (\point ->
                                (point.x |> String.fromInt)
                                    ++ ","
                                    ++ (point.y |> String.fromInt)
                            )
                        |> String.join " "
                    )
                ]
                []
            ]
        ]


update : Event -> State -> State
update event state =
    case event of
        CounterClicked ->
            { state | count = state.count + 1 }

        MouseMoved newMousePosition ->
            { state
                | mouseX = newMousePosition.x
                , mouseY = newMousePosition.y
                , mouseTrail =
                    if state.mouseX == 0 && state.mouseY == 0 then
                        state.mouseTrail

                    else
                        { x = state.mouseX, y = state.mouseY }
                            :: (state.mouseTrail |> List.take 999)
            }
