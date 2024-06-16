/// Minimum bindings for Node.js functionality.
module Node.Min

open Fable.Core
open Fable.Core.JsInterop

module NodeTypes =
    [<AllowNullLiteral>]
    type Error =
        abstract cause: string option with get, set
        abstract message: string option with get, set

    [<AllowNullLiteral>]
    type Buffer =
        abstract toString: unit -> string

module Path =
    open System

    type IExports =
        abstract basename: path: string -> string
        abstract dirname: path: string -> string
        abstract join: [<ParamArray>] paths: string [] -> string
        abstract resolve: [<ParamArray>] paths: string [] -> string
        abstract sep: string

[<Import("*", "path")>]
let path: Path.IExports = jsNative

module Fs =
    open NodeTypes

    [<AllowNullLiteral>]
    type Stats =
        abstract isFile: unit -> bool
        abstract isDirectory: unit -> bool

    type IExports =
        abstract existsSync: path: string -> bool
        abstract readdir: path: string * ?callback: (Error option -> ResizeArray<string> -> unit) -> unit
        abstract readFile: filename: string * options: obj * callback: (Error option -> Buffer -> unit) -> unit
        abstract writeFile: filename: string * data: obj * ?callback: (Error option -> unit) -> unit
        abstract statSync: path: string -> Stats

[<Import("*", "fs")>]
let fs: Fs.IExports = jsNative

module Module =
    let fileURLToPath: string -> string = importMember "url"

    let inline __filename<'T> = fileURLToPath (emitJsExpr () "import.meta.url")
    let inline __dirname<'T> = path.dirname (__filename)

module Process =
    [<AllowNullLiteral>]
    type IExports =
        abstract argv: ResizeArray<string> with get, set

[<Import("*", "process")>]
let process': Process.IExports = jsNative

// NOTE: minimum implementation for Intl.DateTimeFormat.formatToParts.
[<RequireQualifiedAccess>]
module Intl =
    [<AllowNullLiteral>]
    type DateTimeFormatOptions =
        abstract weekday: string with get, set
        abstract year: string with get, set
        abstract month: string with get, set
        abstract day: string with get, set
        abstract hour: string with get, set
        abstract minute: string with get, set
        abstract second: string with get, set
        abstract hourCycle: string with get, set
        abstract timeZone: string with get, set
        abstract timeZoneName: string with get, set

    [<AllowNullLiteral>]
    type DateTimeFormatPart =
        abstract ``type``: string with get, set
        abstract value: string with get, set

    [<AllowNullLiteral>]
    type DateTimeFormat =
        [<Emit "new Intl.$0($1, $2)">]
        abstract Create: lang: string -> options: DateTimeFormatOptions -> DateTimeFormat

        abstract formatToParts: date: System.DateTime -> DateTimeFormatPart array

    [<Global>]
    let DateTimeFormat: DateTimeFormat = jsNative
