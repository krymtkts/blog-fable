/// Additional Node.js functionality.
module rec Node.Extra

open Fable.Core.JsInterop
open Node.Min

[<RequireQualifiedAccess>]
module Directory =
    open NodeTypes

    let join2 (pathA: string) (pathB: string) = path.join (pathA, pathB)
    let join3 (pathA: string) (pathB: string) (pathC: string) = path.join (pathA, pathB, pathC)
    let exists = fs.existsSync

    let create (dir: string) =
        let options = createObj [ "recursive" ==> true ]

        Promise.create (fun resolve reject ->
            fs?mkdir (
                dir,
                options,
                (fun (err: Error option) ->
                    match err with
                    | Some err -> reject (err :?> System.Exception)
                    | None -> resolve ())
            ))

    let ensure (dir: string) =
        promise {
            match exists dir with
            | true -> return ()
            | false -> return! create dir
        }

    let dirname (dir: string) = path.dirname dir

    let leaf (dir: string) = path.basename dir

    let getFiles (isRecursive: bool) (dir: string) =
        Promise.create (fun resolve reject ->
            fs.readdir (
                dir,
                (fun (err: Error option) (files: ResizeArray<string>) ->
                    match err with
                    | Some err -> reject (err :?> System.Exception)
                    | None ->
                        files.ToArray()
                        |> Array.map (fun file -> file, File.statsSync (join2 dir file))
                        |> Array.map (fun (filePath, fileInfo) ->
                            if fileInfo.isDirectory () then
                                // If recursive then get the files from the others sub dirs
                                if isRecursive then
                                    promise {
                                        let! files = getFiles true (join2 dir filePath)
                                        return files |> List.map (join2 filePath)
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

[<RequireQualifiedAccess>]
module File =
    let read (path: string) =
        Promise.create (fun resolve reject ->
            fs.readFile (
                path,
                null,
                (fun err buffer ->
                    match err with
                    | Some err -> reject (err :?> System.Exception)
                    | None -> resolve (buffer.toString ()))
            ))

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
            do! destination |> Directory.dirname |> Directory.ensure

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

    let absolutePath (dir: string) = path.resolve dir
    let statsSync (path: string) : Fs.Stats = fs.statSync path

module Process =
    let argv = process'.argv

module DateTime =
    open System
    open System.Text.RegularExpressions

    let options (timeZone: string) =
        jsOptions<Intl.DateTimeFormatOptions> (fun o ->
            o.weekday <- "short"
            o.year <- "numeric"
            o.month <- "short"
            o.day <- "2-digit"
            o.hour <- "numeric"
            o.minute <- "numeric"
            o.second <- "numeric"
            o.hourCycle <- "h23"
            o.timeZone <- timeZone
            o.timeZoneName <- "short")

    let datetimeFormat timeZone =
        Intl.DateTimeFormat.Create "en-US" <| options timeZone

    let toRFC822DateTimeString (timeZone: string) (d: DateTime) =
        let formatter = datetimeFormat timeZone
        let parts = formatter.formatToParts (d)
        let p: string[] = parts |> Array.map _.value
        let d = $"%s{p.[0]}%s{p.[1]}%s{p.[4]} %s{p.[2]} %s{p.[6]}"
        let t = p.[8..12] |> String.concat ""

        let z =
            match p.[14] with
            | "UTC" -> "+0000"
            | z ->
                let item = Regex.Matches(z, @"GMT([+-])(\d+)")
                let group = item.Item 0
                let op = (group.Groups.Item 1).Value
                let offset = int (group.Groups.Item 2).Value

                $"%s{op}%02d{offset}00"

        $"%s{d} %s{t} %s{z}"

    let parseToRFC822DateTimeString (timeZone: string) (str: string) =
        DateTime.Parse(str) |> toRFC822DateTimeString timeZone
