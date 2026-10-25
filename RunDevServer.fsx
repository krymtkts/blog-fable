// NOTE: requires all dependencies references.
#r "nuget: Fake.Core.Process"
#r "nuget: Fake.DotNet.Cli"
#r "nuget: Fake.JavaScript.Npm"
#r "nuget: Serilog"
#r "nuget: Serilog.Sinks.Console"
#r "nuget: Suave"
// NOTE: Override vulnerable transitive dependencies of Fake.DotNet.Cli.
#r "nuget: System.Drawing.Common"
#r "nuget: System.Security.Cryptography.Pkcs"
#r "nuget: NuGet.Protocol"
// NOTE: loading the dev server from test project.
#load "./test/DevServer.fs"

open System
open Fake.IO
open Fake.IO.Globbing.Operators
open Suave

open DevServer
open System.Threading

let root =
    match fsi.CommandLineArgs with
    | [| _; root |] -> root
    | _ -> ""

let ct = new CancellationTokenSource()

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
    startWebServer (suaveConfig home ct.Token) (webpart root)

finally
    use _ = ct
    ct.Cancel()
