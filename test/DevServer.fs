module DevServer

open Fake.Core
open Fake.DotNet
open Fake.IO
open Fake.JavaScript
open Suave
open Suave.Filters
open Suave.Operators
open Suave.Sockets
open System
open System.Diagnostics
open System.Net
open System.Net.Sockets
open System.Threading
open System.Threading.Tasks


module Logging =
    open Serilog

    let logger =
        LoggerConfiguration()
            .MinimumLevel.Debug()
            .WriteTo.Console(
                outputTemplate = "[{Timestamp:yyyy-MM-dd HH:mm:ss.fff} {Level:u3}] {Message:lj}{NewLine}{Exception}"
            )
            .CreateLogger()

    let log (logger: ILogger) : WebPart =
        fun ctx ->
            async {
                logger.Information(
                    "HTTP {Method} {Path} -> {Status} {Reason}",
                    ctx.request.``method``.ToString(),
                    ctx.request.rawPath,
                    ctx.response.status.code,
                    ctx.response.status.reason
                )

                return Some ctx
            }

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

[<RequireQualifiedAccess>]
[<Struct>]
type BuildResult =
    | Ok
    | Error of string
    | Noop

let (handleWatcherEvents: FileChange seq -> unit), sseHandler =
    let refreshEvent = new Event<unit>()

    let buildFable () =
        let cmd = "fable src"
        let args = "--runScript dev" // NOTE: run script with development mode.
        let result = DotNet.exec (fun x -> { x with DotNetCliPath = "dotnet" }) cmd args

        match result.OK with
        | true -> BuildResult.Ok
        | false ->
            printfn $"`dotnet %s{cmd} %s{args}` failed"
            result.Messages |> String.concat "\n" |> BuildResult.Error

    let buildMd () =
        try
            Npm.run "build-md" id
            BuildResult.Ok
        with ex ->
            printfn $"`[error] npm run build-md` failed: %s{ex.Message}"
            BuildResult.Error ex.Message

    let buildStyle () =
        try
            Npm.run "build-css" id
            BuildResult.Ok
        with ex ->
            printfn $"`[error] npm run build-css` failed: %s{ex.Message}"
            BuildResult.Error ex.Message

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
            | _ -> BuildResult.Noop

        let style =
            match es |> Set.contains BuildEvent.BuildStyle with
            | true -> buildStyle ()
            | _ -> BuildResult.Noop

        match fableOrMd, style with
        | BuildResult.Ok, _
        | _, BuildResult.Ok ->
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
            | :? OperationCanceledException
            | :? SocketException -> ()
        }

    handleWatcherEvents, sseHandler

let suaveConfig (home: string) (ct: CancellationToken) =
    let home =
        let home = IO.Path.GetFullPath home
        printfn $"watch '%s{home}'"
        home |> Some

    let extendedMimeTypesMap (ext: string) =
        // NOTE: Add custom mime types for pagefind to prevent 404 error.
        match ext with
        | ".pagefind"
        | ".pf_fragment"
        | ".pf_index"
        | ".pf_meta" -> Writers.createMimeType "application/octet-stream" false
        | ext -> Writers.defaultMimeTypesMap ext

    { defaultConfig with
        homeFolder = home
        compressedFilesFolder = home
        bindings = [ HttpBinding.create HTTP IPAddress.Loopback port ]
        listenTimeout = TimeSpan.FromMilliseconds 3000.
        mimeTypesMap = extendedMimeTypesMap
        cancellationToken = ct }

let webpart (root: string) : WebPart =
    let root = root.Trim '/'

    choose [

        path "/sse" >=> EventSource.handShake sseHandler >=> Logging.log Logging.logger

        GET
        >=> Writers.setHeader "Cache-Control" "no-cache, no-store, must-revalidate"
        >=> Writers.setHeader "Pragma" "no-cache"
        >=> Writers.setHeader "Expires" "0"
        >=> choose [

            path $"/{root}/" >=> Files.browseFileHome $"{root}/index.html"
            path $"/{root}" >=> Redirection.redirect $"/{root}/"

            Files.browseHome

        ]
        >=> Logging.log Logging.logger

        Writers.setStatus HTTP_404
        >=> choose [
            Files.browseFileHome $"{root}/404.html"
            RequestErrors.NOT_FOUND "404 - Not Found" // NOTE: Fallback 404 page.
        ]
        >=> Logging.log Logging.logger

    ]

let openIndex url =
    let p = new ProcessStartInfo(url)

    p.UseShellExecute <- true
    Process.Start(p) |> ignore
