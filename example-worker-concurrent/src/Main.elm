port module Main exposing (Event, State, main)

{-| Slightly arbitrary CLI program:

Ask the user for name and title
and if they take a while, print dots while still waiting for user input.

Showcases that reading user input doesn't block the "dot printing task"
→ subscriptions are run concurrently.

-}

import Json.Decode
import Json.Encode
import Time


port portStdOutWrite : Json.Encode.Value -> Cmd event_


port portProcessExit : Json.Encode.Value -> Cmd event_


port portStdInReadLine : (Json.Encode.Value -> event) -> Sub event


port portSleep1Second : (Json.Encode.Value -> event) -> Sub event


stdOutWrite : String -> Cmd event_
stdOutWrite output =
    portStdOutWrite (Json.Encode.string output)


processExit : Int -> Cmd event_
processExit code =
    portProcessExit (Json.Encode.int code)


stdInReadLine : (String -> Event) -> Sub Event
stdInReadLine onReadLine =
    portStdInReadLine
        (\value ->
            case value |> Json.Decode.decodeValue Json.Decode.string of
                Ok readLine ->
                    onReadLine readLine

                Err error ->
                    PortEventFailedToDecode
                        { name = "portStdInReadLine"
                        , error = error
                        }
        )


sleep1Second : (Time.Posix -> Event) -> Sub Event
sleep1Second on1SecondPassed =
    portSleep1Second
        (\value ->
            case value |> Json.Decode.decodeValue (Json.Decode.map Time.millisToPosix Json.Decode.int) of
                Ok currentTimePosix ->
                    on1SecondPassed currentTimePosix

                Err error ->
                    PortEventFailedToDecode
                        { name = "portSleep1Second"
                        , error = error
                        }
        )


type alias State =
    { name : Maybe String
    , title : Maybe String
    }


type Event
    = StdInLineReadName String
    | StdInLineReadTitle String
    | WaitingFor1Second Time.Posix
    | PortEventFailedToDecode { name : String, error : Json.Decode.Error }


type alias Flags =
    List String


main : Program Flags State Event
main =
    Platform.worker
        { init =
            \commandLineArguments ->
                case commandLineArguments of
                    [ name, title ] ->
                        ( { name = Just name, title = Just title }
                        , stdOutWrite
                            ("Welcome, "
                                ++ name
                                ++ ", the "
                                ++ title
                                ++ ". Nice to meet ya!"
                            )
                        )

                    [ name ] ->
                        ( { name = Just name, title = Nothing }
                        , stdOutWrite
                            ("Hi, "
                                ++ name
                                ++ ". Which title should I assign to you?  > "
                            )
                        )

                    _ ->
                        ( { name = Nothing, title = Nothing }
                        , stdOutWrite "What's your name?  > "
                        )
        , update =
            \event state ->
                case event of
                    StdInLineReadName name ->
                        ( { state | name = Just name }
                        , stdOutWrite
                            ("Hi, "
                                ++ name
                                ++ ". Which title should I assign to you?  > "
                            )
                        )

                    StdInLineReadTitle title ->
                        ( { state | title = Just title }
                        , stdOutWrite
                            ("Got it. "
                                ++ Maybe.withDefault "unnamed" state.name
                                ++ ", the "
                                ++ title
                                ++ ". Nice to meet ya!\n"
                            )
                        )

                    WaitingFor1Second _ ->
                        ( state
                        , stdOutWrite "."
                        )

                    PortEventFailedToDecode portError ->
                        ( state
                        , Cmd.batch
                            [ stdOutWrite
                                ("Failed to decode event of port "
                                    ++ portError.name
                                    ++ ": "
                                    ++ (portError.error |> Json.Decode.errorToString)
                                    ++ ".\n"
                                )
                            , processExit 1
                            ]
                        )
        , subscriptions =
            \state ->
                case ( state.name, state.title ) of
                    ( Just _, Just _ ) ->
                        Sub.none

                    ( Nothing, _ ) ->
                        Sub.batch
                            [ stdInReadLine StdInLineReadName
                            , sleep1Second WaitingFor1Second
                            ]

                    ( Just _, Nothing ) ->
                        Sub.batch
                            [ stdInReadLine StdInLineReadTitle
                            , sleep1Second WaitingFor1Second
                            ]
        }
