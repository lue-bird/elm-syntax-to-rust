// turn elm.rs into an escaped elm string
// and inserts it at ElmSyntaxToRust.defaultDeclarations
import * as fs from "node:fs"
import * as path from "node:path"

function indexAfterFirst(needle, lookFromIndex, full) {
    return full.indexOf(needle, lookFromIndex) + needle.length
}

const defaultDeclarationsRustFile =
    fs.readFileSync(
        path.join(import.meta.dirname, "elm.rs"),
        { encoding: "utf-8" }
    )
const elmSyntaxToRustElmPath = path.join(import.meta.dirname, "ElmSyntaxToRust.elm")
const elmSyntaxToRustElmFile =
    fs.readFileSync(elmSyntaxToRustElmPath, { encoding: "utf-8" })

const elmString =
    "\"\"\"\n"
    + defaultDeclarationsRustFile
        .slice(
            defaultDeclarationsRustFile.indexOf("pub type"),
            defaultDeclarationsRustFile.length
        )
        .replaceAll("\\", "\\\\")
        .trim()
    + "\n\"\"\""
const defaultDeclarationsDeclarationStartIndex =
    elmSyntaxToRustElmFile.indexOf(`"""\npub type`)
const defaultDeclarationsDeclarationToReplace =
    elmSyntaxToRustElmFile.slice(
        defaultDeclarationsDeclarationStartIndex,
        indexAfterFirst(`\n"""`, defaultDeclarationsDeclarationStartIndex, elmSyntaxToRustElmFile)
    )
const elmSyntaxToRustElmFileWithUpdatedDefaultDeclarations =
    elmSyntaxToRustElmFile.replace(defaultDeclarationsDeclarationToReplace, elmString)
fs.writeFileSync(
    elmSyntaxToRustElmPath,
    elmSyntaxToRustElmFileWithUpdatedDefaultDeclarations,
    { encoding: "utf-8" }
)
