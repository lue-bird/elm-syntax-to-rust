module FormatSingleElmModule exposing (string)

import ElmSyntaxParserLenient
import ElmSyntaxPrint


string : String -> Result String String
string moduleSource =
    case ElmSyntaxParserLenient.run ElmSyntaxParserLenient.module_ moduleSource of
        Nothing ->
            Err "failed to parse. Please run the elm compiler for errors"

        Just moduleParsed ->
            Ok
                (ElmSyntaxPrint.module_ moduleParsed
                    |> ElmSyntaxPrint.toString
                )
