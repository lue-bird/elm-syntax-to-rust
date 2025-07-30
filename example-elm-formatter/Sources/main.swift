import Foundation

do {
    let inputData = try FileHandle.standardInput.readToEnd() ?? Data()
    let input = String(data: inputData, encoding: .utf8) ?? ""
    FileHandle.standardOutput.write(
        Data(Elm.FormatSingleModule_formatSingleModule(input).utf8)
    )
} catch let error {
    print(error.localizedDescription)
}
