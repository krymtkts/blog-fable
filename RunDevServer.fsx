#load "DevServer.fsx"

open Fake.IO
open Fake.IO.Globbing.Operators
open Suave

open DevServer

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
    startWebServer cfg app

finally
    ()
