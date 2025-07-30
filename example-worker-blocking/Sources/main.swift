import Foundation

func performCmd<event>(_ cmd: Elm.PlatformCmd_Cmd<event>) {
    for cmdSingle in cmd {
        switch cmdSingle {
        case let .PlatformCmd_PortOutgoing(name: name, value: value):
            var maybeValueJsonDecodeError: Elm.JsonDecode_Error? = .none
            switch name {
            case "portStdOutWrite":
                switch Elm.JsonDecode_decodeValue(Elm.JsonDecode_string, value) {
                case let .Result_Ok(toWrite):
                    FileHandle.standardOutput.write(Data(toWrite.utf8))
                case let .Result_Err(jsonDecodeError):
                    maybeValueJsonDecodeError = .some(jsonDecodeError)
                }
            case "portProcessExit":
                switch Elm.JsonDecode_decodeValue(Elm.JsonDecode_int, value) {
                case let .Result_Ok(code):
                    exit(Int32(truncatingIfNeeded: Int(code)))
                case let .Result_Err(jsonDecodeError):
                    print(
                        "failed to decode value of port outgoing \(name): \(Elm.JsonDecode_errorToString(jsonDecodeError))"
                    )
                }
            case _:
                print("unknown port outgoing name \(name)")
            }
            if let valueJsonDecodeError = maybeValueJsonDecodeError {
                print(
                    "failed to decode value of port outgoing \(name): \(Elm.JsonDecode_errorToString(valueJsonDecodeError))"
                )
            }
        }
    }

}

var elmInitialized =
    Elm.Main_main.init_(Elm.Array_toList(Array(CommandLine.arguments.dropFirst())))
var elmCurrentState: Elm.Main_State = elmInitialized.first

performCmd(elmInitialized.second)

func onEvent(_ event: Elm.Main_Event, _ state: Elm.Main_State) -> Elm.Main_State {
    let updated = Elm.Main_main.update(event)(state)
    performCmd(updated.second)
    return updated.first
}

var elmCurrentSubscriptions: Elm.PlatformSub_Sub<Elm.Main_Event> =
    Elm.Main_main.subscriptions(elmCurrentState)
while !elmCurrentSubscriptions.isEmpty {
    for subSingle in elmCurrentSubscriptions {
        switch subSingle {
        case let .PlatformSub_PortIncoming(name: name, onValue: onValue):
            switch name {
            case "portStdInReadLine":
                let input: String? = readLine()
                elmCurrentState = onEvent(
                    onValue(Elm.JsonEncode_string(input ?? "")),
                    elmCurrentState
                )
            case _:
                print("unknown port incoming name \(name)")
            }
        }
    }
    elmCurrentSubscriptions = Elm.Main_main.subscriptions(elmCurrentState)
}
