#r "nuget: Fake.Core.Trace"
#r "nuget: Fake.DotNet.Cli"
#r "nuget: Fake.JavaScript.Npm"
#r "nuget: Fake.IO.FileSystem"
#r "nuget: Suave"

open Fake.Core
open Fake.DotNet
open Fake.IO
open Fake.IO.Globbing.Operators
open Fake.JavaScript
open Suave
open Suave.Filters
open Suave.Operators
open Suave.Sockets
open Suave.Sockets.Control
open Suave.Utils
open Suave.WebSocket
open System
open System.Net
open System.Threading

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

type BuildEvent =
    | BuildFable
    | BuildMd
    | BuildStyle
    | Noop

let handleWatcherEvents, socketHandler =
    let refreshEvent = new Event<_>()

    let buildFable () =
        let cmd = "fable src"
        let args = "--runScript dev" // NOTE: run script with development mode.
        let result = DotNet.exec (fun x -> { x with DotNetCliPath = "dotnet" }) cmd args

        match result.OK with
        | true -> Ok true
        | false ->
            printfn $"`dotnet %s{cmd} %s{args}` failed"
            Error false

    let buildMd () =
        Npm.run "build-md" id
        Ok true

    let buildStyle () =
        Npm.run "build-css" id
        Ok true

    let handleWatcherEvents (events: FileChange seq) =
        let es =
            events
            |> Seq.map (fun e ->
                let fi = FileInfo.ofPath e.FullPath

                Trace.traceImportant $"%s{fi.FullName} was changed."

                match fi.FullName with
                | x when x.EndsWith(".fs") -> BuildFable
                | x when x.EndsWith(".md") -> BuildMd
                | x when x.EndsWith(".scss") -> BuildStyle
                | _ -> Noop)
            |> Set.ofSeq

        let fableOrMd =
            match [ BuildFable; BuildMd ] |> List.map es.Contains with
            | [ true; true ] -> buildFable ()
            | [ _; true ] -> buildMd ()
            | [ true; _ ] -> buildFable ()
            | _ -> Ok false

        let style =
            match es |> Set.contains BuildStyle with
            | true -> buildStyle ()
            | _ -> Ok false

        match fableOrMd, style with
        | Ok true, _
        | _, Ok true ->
            refreshEvent.Trigger()
            printfn "refresh event is triggered."
        | _ -> printfn "refresh event not triggered."

    let socketHandler (ws: WebSocket) _ =
        let rec refreshLoop () =
            async {
                do! refreshEvent.Publish |> Async.AwaitEvent

                printfn "refresh client."
                let seg = ASCII.bytes "refreshed" |> ByteSegment
                let! _ = ws.send Text seg true

                return! refreshLoop ()
            }

        let rec mainLoop (cts: CancellationTokenSource) =
            socket {
                let! msg = ws.read ()

                match msg with
                | (Close, _, _) ->
                    use _ = cts
                    cts.Cancel()

                    let emptyResponse = [||] |> ByteSegment
                    do! ws.send Close emptyResponse true
                    printfn "WebSocket connection closed gracefully."
                | _ -> return! mainLoop cts
            }

        let cts = new CancellationTokenSource()
        Async.Start(refreshLoop (), cts.Token)
        mainLoop cts


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


let root =
    match fsi.CommandLineArgs with
    | [| _; root |] -> root
    | _ -> ""

let app: WebPart =
    let logger = Logging.Log.create "dev-server"

    choose [

             path "/websocket" >=> handShake socketHandler

             GET
             >=> Writers.setHeader "Cache-Control" "no-cache, no-store, must-revalidate"
             >=> Writers.setHeader "Pragma" "no-cache"
             >=> Writers.setHeader "Expires" "0"
             >=> choose [ path $"{root}/"
                          >=> Files.browseFileHome "blog-fable/index.html"
                          path $"{root}"
                          >=> Redirection.redirect $"/blog-fable/"

                          Files.browseHome ]
             >=> log logger logFormat

             Writers.setStatus HTTP_404
             >=> logWithLevel Logging.Error logger logFormat
             >=> Files.browseFileHome $"{root}/404.html" ]

let openIndex url =
    let p = new Diagnostics.ProcessStartInfo(url)

    p.UseShellExecute <- true
    Diagnostics.Process.Start(p) |> ignore

try
    use _ =
        !! "src/**/*.fs"
        ++ "contents/**/*.md"
        ++ "sass/**/*.scss"
        |> ChangeWatcher.run handleWatcherEvents

    let index: string = $"http://localhost:%d{port}%s{root}/index.html"
    printfn $"Open %s{index} ..."
    openIndex index

    printfn "Starting dev server..."
    startWebServer cfg app

finally
    ()
