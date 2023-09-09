#r "nuget: Fake.Core.Trace"
#r "nuget: Fake.DotNet.Cli"
#r "nuget: Fake.IO.FileSystem"
#r "nuget: Suave"

open Fake.Core
open Fake.DotNet
open Fake.IO
open Fake.IO.Globbing.Operators
open Suave
open Suave.Files
open Suave.Filters
open Suave.Operators
open Suave.Sockets
open Suave.Sockets.Control
open Suave.Utils
open Suave.WebSocket
open System
open System.Net


let port =
    let rec findPort port =
        let portIsTaken =
            System
                .Net
                .NetworkInformation
                .IPGlobalProperties
                .GetIPGlobalProperties()
                .GetActiveTcpListeners()
            |> Seq.exists (fun x -> x.Port = int (port))

        if portIsTaken then
            findPort (port + 1us)
        else
            port

    findPort 8080us

let handleWatcherEvents, socketHandler =
    let refreshEvent = new Event<_>()

    let handleWatcherEvents (events: FileChange seq) =
        for e in events do
            let fi = FileInfo.ofPath e.FullPath

            Trace.traceImportant $"%s{fi.FullName} was changed."

        let cmd = "fable src"
        let args = "--runScript dev" // NOTE: run script with development mode.
        let result = DotNet.exec (fun x -> { x with DotNetCliPath = "dotnet" }) cmd args

        if result.OK then
            refreshEvent.Trigger()
        else
            printfn $"`dotnet %s{cmd} %s{args}` failed"

    let socketHandler (webSocket: WebSocket) =
        fun _ ->
            socket {
                while true do
                    do!
                        Async.AwaitEvent(refreshEvent.Publish)
                        |> SocketOp.ofAsync

                    let seg = ASCII.bytes "refreshed" |> ByteSegment
                    do! webSocket.send Text seg true
            }

    handleWatcherEvents, socketHandler

let home =
    IO.Path.Join [| __SOURCE_DIRECTORY__
                    "docs" |]

printfn $"watch '%s{home}'"

let cfg =
    { defaultConfig with
        homeFolder = Some(home)
        compressedFilesFolder = Some(home)
        bindings = [ HttpBinding.create HTTP IPAddress.Loopback port ]
        listenTimeout = TimeSpan.FromMilliseconds 3000. }

let app: WebPart =
    let logger = Logging.Log.create "dev-server"

    choose [ log logger logFormat >=> never
             path "/websocket" >=> handShake socketHandler

             GET
             >=> path "/blog-fable"
             >=> browseFileHome "blog-fable/index.html"
             GET
             >=> path "/blog-fable/"
             >=> browseFileHome "blog-fable/index.html"

             GET
             >=> Writers.setHeader "Cache-Control" "no-cache, no-store, must-revalidate"
             >=> Writers.setHeader "Pragma" "no-cache"
             >=> Writers.setHeader "Expires" "0"
             >=> browseHome

             Writers.setStatus HTTP_404
             >=> browseFileHome "/blog-fable/404.html" ]

let openIndex url =
    let p = new Diagnostics.ProcessStartInfo(url)

    p.UseShellExecute <- true
    Diagnostics.Process.Start(p) |> ignore

try
    use _ =
        !! "src/**/*.fs" ++ "contents/**/*.md"
        |> ChangeWatcher.run handleWatcherEvents

    let index: string = $"http://localhost:%d{port}/blog-fable/index.html"
    printfn $"Open %s{index} ..."
    openIndex index

    printfn "Starting dev server..."
    startWebServer cfg app

finally
    ()
