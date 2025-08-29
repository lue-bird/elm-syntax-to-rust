module Main exposing (ElementToRender(..), Interface(..), State, initWindow, stateToInterface)

import Color exposing (Color)


type State
    = StateRunning
        { y : Float
        , yVelocity : Float
        }
    | StateCloseWindow


windowHeight : Int
windowHeight =
    600


windowWidth : Int
windowWidth =
    800


initWindow :
    { windowWidth : Int
    , windowHeight : Int
    , windowTitle : String
    , state : State
    }
initWindow =
    { windowWidth = windowWidth
    , windowHeight = windowHeight
    , windowTitle = """rust 🤝 elm 🤝 raylib"""
    , state =
        StateRunning
            { y = 0.3 * ((windowHeight |> Basics.toFloat) - characterHeight)
            , yVelocity = 0
            }
    }


type Interface state
    = InterfaceRender
        { clearColor : Color
        , elements : List ElementToRender
        }
    | InterfaceQuit
    | InterfaceOnSimulationTick (() -> state)
    | InterfaceOnQuit (() -> state)


type ElementToRender
    = FilledRectangleToRender FilledRectangleToRender


type alias FilledRectangleToRender =
    RecordWithoutConstructorFunction
        { left : Float
        , top : Float
        , width : Float
        , height : Float
        , color : Color
        }


stateToInterface : State -> List (Interface State)
stateToInterface state =
    case state of
        StateCloseWindow ->
            [ InterfaceQuit ]

        StateRunning running ->
            [ InterfaceOnQuit (\() -> StateCloseWindow)
            , InterfaceRender
                { clearColor = Color.rgba 0.047 0 0 0
                , elements =
                    [ let
                        wiggleRoom : Float
                        wiggleRoom =
                            min 2 (abs running.yVelocity * 0.1)
                                * (sqrt (characterHeight * characterWidth) / 2)

                        squish : Float
                        squish =
                            Basics.clamp
                                0
                                1
                                ((running.y - ((windowHeight |> Basics.toFloat) - (characterHeight + wiggleRoom)))
                                    / wiggleRoom
                                )
                      in
                      FilledRectangleToRender
                        { left =
                            ((windowWidth |> Basics.toFloat) / 2)
                                - (characterWidth / 2)
                                - ((wiggleRoom * squish) / 2)
                        , top = running.y
                        , width = characterWidth + wiggleRoom * squish
                        , height = characterHeight - wiggleRoom * squish
                        , color = Color.rgb255 0 0 100
                        }
                    ]
                }
            , InterfaceOnSimulationTick
                (\() ->
                    let
                        updatedYVelocity : Float
                        updatedYVelocity =
                            if running.y >= (windowHeight |> Basics.toFloat) - characterHeight then
                                -running.yVelocity

                            else
                                running.yVelocity + 0.1
                    in
                    StateRunning
                        { y = running.y + updatedYVelocity
                        , yVelocity = updatedYVelocity
                        }
                )
            ]


characterHeight : Float
characterHeight =
    100


characterWidth : Float
characterWidth =
    100



--


type alias RecordWithoutConstructorFunction a =
    a
