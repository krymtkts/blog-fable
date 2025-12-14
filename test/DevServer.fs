module DevServer

open Fake.Core
open Fake.DotNet
open Fake.IO
open Fake.JavaScript
open Suave
open Suave.Filters
open Suave.Operators
open Suave.Sockets
open Suave.Sockets.Control
open Suave.Utils
open System
open System.Net
open System.Net.Sockets
open System.Threading
open System.Threading.Tasks

let port =
    let rec findPort port =
        let portIsTaken =
            NetworkInformation.IPGlobalProperties.GetIPGlobalProperties().GetActiveTcpListeners()
            |> Seq.exists (fun x -> x.Port = int port)

        if portIsTaken then findPort (port + 1us) else port

    findPort 8080us

[<RequireQualifiedAccess>]
[<Struct>]
type BuildEvent =
    | BuildFable
    | BuildMd
    | BuildStyle
    | Noop

let (handleWatcherEvents: FileChange seq -> unit), sseHandler =
    let refreshEvent = new Event<unit>()

    let buildFable () =
        let cmd = "fable src"
        let args = "--runScript dev" // NOTE: run script with development mode.
        let result = DotNet.exec (fun x -> { x with DotNetCliPath = "dotnet" }) cmd args

        match result.OK with
        | true -> Result.Ok true
        | false ->
            printfn $"`dotnet %s{cmd} %s{args}` failed"
            Result.Error false

    let buildMd () =
        Npm.run "build-md dev" id
        Ok true

    let buildStyle () =
        Npm.run "build-css" id
        Ok true

    let handleWatcherEvents (events: FileChange seq) =
        let es =
            events
            |> Seq.map (fun e ->
                let fi = FileInfo.ofPath e.FullPath

                Trace.traceImportant $"%s{fi.FullName} was changed. ext: %s{fi.Extension}"

                match fi.Extension with
                | ".fs" -> BuildEvent.BuildFable
                | ".md"
                | ".yml"
                | ".yaml" -> BuildEvent.BuildMd
                | ".scss" -> BuildEvent.BuildStyle
                | _ -> BuildEvent.Noop)
            |> Set.ofSeq

        let fableOrMd =
            match [ BuildEvent.BuildFable; BuildEvent.BuildMd ] |> List.map es.Contains with
            | [ true; true ] -> buildFable ()
            | [ _; true ] -> buildMd ()
            | [ true; _ ] -> buildFable ()
            | _ -> Ok false

        let style =
            match es |> Set.contains BuildEvent.BuildStyle with
            | true -> buildStyle ()
            | _ -> Ok false

        match fableOrMd, style with
        | Ok true, _
        | _, Ok true ->
            refreshEvent.Trigger()
            printfn "refresh event is triggered."
        | _ -> printfn "refresh event not triggered."

    let sseHandler (conn: Connection) : Task<unit> =
        task {
            try
                // NOTE: Tell the browser how long to wait before retrying the connection.
                do! EventSource.retry conn 1000u

                // NOTE: Initial event so the client can confirm it is connected.
                do! EventSource.eventType conn "connected"
                do! EventSource.data conn "ok"
                do! EventSource.dispatch conn

                while true do
                    do!
                        Async.StartAsTask(
                            refreshEvent.Publish |> Async.AwaitEvent,
                            cancellationToken = CancellationToken.None
                        )

                    do! EventSource.eventType conn "refresh"
                    do! EventSource.data conn "refreshed"
                    do! EventSource.dispatch conn
            with
            | :? OperationCanceledException -> ()
            | :? SocketException -> ()
        }

    handleWatcherEvents, sseHandler

let suaveConfig (home: string) (ct: CancellationToken) =
    let home = IO.Path.GetFullPath home
    printfn $"watch '%s{home}'"

    let extendedMimeTypesMap (ext: string) =
        // NOTE: Add custom mime types for pagefind to prevent 404 error.
        match ext with
        | ".pagefind"
        | ".pf_fragment"
        | ".pf_index"
        | ".pf_meta" -> Writers.createMimeType "application/octet-stream" false
        | ext -> Writers.defaultMimeTypesMap ext

    { defaultConfig with
        homeFolder = Some(home)
        compressedFilesFolder = Some(home)
        bindings = [ HttpBinding.create HTTP IPAddress.Loopback port ]
        listenTimeout = TimeSpan.FromMilliseconds 3000.
        mimeTypesMap = extendedMimeTypesMap
        cancellationToken = ct }


let webpart (root: string) : WebPart =
    // TODO: Logging module is missing in Suave 3.2.
    // let logger = Suave.Logging.Log.create "dev-server"

    choose [

        path "/sse" >=> EventSource.handShake sseHandler

        GET
        >=> Writers.setHeader "Cache-Control" "no-cache, no-store, must-revalidate"
        >=> Writers.setHeader "Pragma" "no-cache"
        >=> Writers.setHeader "Expires" "0"
        >=> choose [

            path $"{root}/" >=> Files.browseFileHome "blog-fable/index.html"
            path $"{root}" >=> Redirection.redirect $"/blog-fable/"

            Files.browseHome

        ]
        // >=> log logger logFormat

        Writers.setStatus HTTP_404
        // >=> logWithLevel Logging.Error logger logFormat
        >=> Files.browseFileHome $"{root}/404.html"
    ]

let openIndex url =
    let p = new Diagnostics.ProcessStartInfo(url)

    p.UseShellExecute <- true
    Diagnostics.Process.Start(p) |> ignore
