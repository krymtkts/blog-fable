/// Additional Node.js functionality.
module rec Node

open Fable.Core
open Fable.Core.JsInterop
open Node

type Node.Fs.IExports with
    [<Emit("$0.copyFileSync($1...)")>]
    member __.copyFileSync(src: string, dest: string, ?mode: int) = jsNative

[<RequireQualifiedAccess>]
module Directory =

    let moveUp (path: string) =
        path.Split(char Node.Api.path.sep)
        |> Array.skip 1
        |> String.concat Node.Api.path.sep

    let join2 (pathA: string) (pathB: string) = path.join (pathA, pathB)
    let join3 (pathA: string) (pathB: string) (pathC: string) = path.join (pathA, pathB, pathC)

    let exists (dir: string) =
        Promise.create (fun resolve reject -> fs.exists ((U2.Case1 dir), (fun res -> resolve res)))

    let create (dir: string) =
        let options = createObj [ "recursive" ==> true ]

        Promise.create (fun resolve reject ->
            fs?mkdir (dir,
                      options,
                      (fun (err: Node.Base.ErrnoException option) ->
                          match err with
                          | Some err -> reject (err :?> System.Exception)
                          | None -> resolve ())))

    let ensure (dir: string) =
        promise {
            match! exists dir with
            | true -> return ()
            | false -> return! create dir
        }

    let dirname (dir: string) = path.dirname (dir)

    let leaf (dir: string) = path.basename (dir)

    let getFiles (isRecursive: bool) (dir: string) =
        Promise.create (fun resolve reject ->
            fs.readdir (
                U2.Case1 dir,
                (fun (err: Node.Base.ErrnoException option) (files: ResizeArray<string>) ->
                    match err with
                    | Some err -> reject (err :?> System.Exception)
                    | None ->
                        files.ToArray()
                        |> Array.map (fun file -> file, File.statsSync (Directory.join2 dir file))
                        |> Array.map (fun (filePath, fileInfo) ->
                            if fileInfo.isDirectory () then
                                // If recursive then get the files from the others sub dirs
                                if isRecursive then
                                    promise {
                                        let! files = getFiles true (Directory.join2 dir filePath)
                                        return files |> List.map (Directory.join2 filePath)
                                    }
                                else
                                    // Else, we return an empty list and this will have the effect
                                    // of removing the directory from the final list
                                    promise { return [] }
                            else
                                promise { return [ filePath ] })
                        |> Promise.all
                        |> Promise.map (fun directories ->
                            if Array.isEmpty directories then
                                []
                            else
                                Array.reduce (fun a b -> a @ b) directories)
                        |> Promise.map (fun files -> resolve files)
                        |> ignore)
            ))

    let rmdir (dir: string) =
        let options = createObj [ "recursive" ==> true ]

        promise {
            let! dirExist = Directory.exists dir

            if dirExist then
                do!
                    Promise.create (fun resolve reject ->
                        fs?rm (dir,
                               options,
                               (fun (err: Base.ErrnoException option) ->
                                   match err with
                                   | Some err -> reject (err :?> System.Exception)
                                   | None -> resolve ())))
        }

[<RequireQualifiedAccess>]
module File =
    let changeExtension (extention: string) (path: string) =
        let extensionPos = path.LastIndexOf('.')
        path.Substring(0, extensionPos + 1) + extention

    let read (path: string) =
        Promise.create (fun resolve reject ->
            fs.readFile (
                path,
                (fun err buffer ->
                    match err with
                    | Some err -> reject (err :?> System.Exception)
                    | None -> resolve (buffer.ToString()))
            ))

    let readSync (path: string) = fs.readFileSync(path).ToString()

    let write (path: string) (content: string) =
        promise {
            do! path |> Directory.dirname |> Directory.ensure

            return!
                Promise.create (fun resolve reject ->
                    fs.writeFile (
                        path,
                        content,
                        (fun res ->
                            match res with
                            | Some res -> reject (res :?> System.Exception)
                            | None -> resolve ())
                    ))
        }

    let copy (source: string) (destination: string) =
        promise {
            do!
                destination
                |> Directory.dirname
                |> Directory.ensure

            return!
                Promise.create (fun resolve reject ->
                    fs?copyFile
                    $ (source,
                       destination,
                       fun res ->
                           match res with
                           | Some res -> reject res
                           | None -> resolve ()))
        }

    let exist (path: string) =
        Promise.create (fun resolve reject -> fs.exists (U2.Case1 path, (fun res -> resolve res)))

    let existSync (path: string) = fs.existsSync (U2.Case1 path)

    let absolutePath (dir: string) = path.resolve (dir)

    let stats (path: string) =
        Promise.create (fun resolve reject ->
            fs.stat (
                U2.Case1 path,
                (fun (err: Node.Base.ErrnoException option) (stats: Node.Fs.Stats) ->
                    match err with
                    | Some err ->
                        reject (err :?> System.Exception)
                        null
                    | None ->
                        resolve stats
                        null)
            ))

    let statsSync (path: string) : Node.Fs.Stats = fs.statSync (U2.Case1 path)

module Module =

    let fileURLToPath: string -> string = importMember "url"

    let inline __filename<'T> = fileURLToPath (emitJsExpr () "import.meta.url")
    let inline __dirname<'T> = path.dirname (__filename)

module Process =
    [<Import("*", "process")>]
    let process': Process.Process = jsNative

    let argv = process'.argv


// NOTE: minimum implementation for Intl.DateTimeFormat.formatToParts.
[<RequireQualifiedAccess>]
module Intl =
    [<Global>]
    let DateTimeFormat: DateTimeFormat = jsNative

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
