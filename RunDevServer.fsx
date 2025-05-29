#r "nuget: Fake.DotNet.Cli"
#r "nuget: Fake.JavaScript.Npm"
#r "nuget: Suave, >= 2.7.0-beta1"

#load "./test/DevServer.fs"

open System
open Fake.IO
open Fake.IO.Globbing.Operators
open Suave

open DevServer

let root =
    match fsi.CommandLineArgs with
    | [| _; root |] -> root
    | _ -> ""

try
    use _ =
        !!"src/**/*.fs"
        ++ "contents/**/*.md"
        ++ "contents/**/*.yml"
        ++ "contents/**/*.yaml"
        ++ "sass/**/*.scss"
        |> ChangeWatcher.run handleWatcherEvents

    let index: string = $"http://localhost:%d{port}%s{root}/index.html"
    printfn $"Open %s{index} ..."
    openIndex index

    printfn "Starting dev server..."
    let home = IO.Path.Join [| __SOURCE_DIRECTORY__; "docs" |]
    printfn $"watch '%s{home}'"

    startWebServer (cfg home) (app root)

finally
    ()
