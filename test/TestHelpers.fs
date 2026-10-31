module TestHelpers

open System
open Expecto
open Microsoft.Playwright
open DevServer
open Suave
open System.Threading
open System.Threading.Tasks
open System.Diagnostics

(*
NOTE: This tests requires the Playwright CLI to be installed.
ex) PS> ./test/bin/Debug/*/playwright.ps1 install
*)

type DevServer private (cancellationTokenSource: CancellationTokenSource, serverTask: Task) =
    static let timeout = TimeSpan.FromSeconds 10.
    static let root = "/blog-fable"

    member __.Port = port
    member __.Root = root

    interface IAsyncDisposable with
        member __.DisposeAsync() =
            task {
                printfn "Stopping dev server asynchronously..."

                try
                    try
                        let shutdown =
                            task {
                                do! cancellationTokenSource.CancelAsync()
                                // NOTE: Cancellation callbacks completing does not mean Suave has released its listener.
                                try
                                    do! serverTask
                                with :? OperationCanceledException when cancellationTokenSource.IsCancellationRequested ->
                                    ()
                            }

                        do! shutdown.WaitAsync(timeout)
                    with :? TimeoutException as ex ->
                        raise (TimeoutException("Test server did not stop within 10 seconds.", ex))
                finally
                    cancellationTokenSource.Dispose()
            }
            |> ValueTask

    static member StartAsync() : Task<DevServer> =
        task {
            let home = IO.Path.Join [| __SOURCE_DIRECTORY__; ".."; "docs" |]
            let cancellationTokenSource = new CancellationTokenSource()

            let listening, serverTask =
                try
                    startWebServerAsync (suaveConfig home cancellationTokenSource.Token) (webpart root)
                with _ ->
                    cancellationTokenSource.Dispose()
                    reraise ()

            let server = new DevServer(cancellationTokenSource, serverTask)

            try
                let listeningTask =
                    Async.StartAsTask(listening, cancellationToken = cancellationTokenSource.Token)

                let! completed = Task.WhenAny(listeningTask :> Task, serverTask).WaitAsync(timeout)

                if completed = serverTask then
                    do! serverTask
                    failwith "Test server stopped before it started listening."

                let! _ = listeningTask
                printfn $"Dev server started at http://localhost:%d{port}%s{root}"
                return server
            with ex ->
                let failure =
                    match ex with
                    | :? TimeoutException ->
                        TimeoutException("Test server did not start listening within 10 seconds.", ex) :> exn
                    | _ -> ex

                try
                    do! (server :> IAsyncDisposable).DisposeAsync().AsTask()
                with cleanupError ->
                    raise (AggregateException("Test server startup and cleanup failed.", [| failure; cleanupError |]))

                return raise failure
        }

type IPlaywright with
    member __.NewChromiumPage() : Task<IPage> =
        task {
            let! browser = __.Chromium.LaunchAsync()
            return! browser.NewPageAsync()
        }

type PlaywrightAsyncDisposable(playwright: IPlaywright) =
    interface IAsyncDisposable with
        member _.DisposeAsync() =
            playwright.Dispose()
            printfn "Playwright disposed."
            () |> ValueTask

    member _.Instance = playwright

type IPage with
    member __.GotoAndCheck(url: string) =
        task {
            let opt =
                let opt = PageGotoOptions()
                opt.WaitUntil <- WaitUntilState.DOMContentLoaded
                opt

            let! response = __.GotoAsync(url, opt)

            match response with
            | null -> return Result.Error $"Failed to load page: %s{url}"
            | r when not r.Ok -> return Result.Error $"Failed to load page: %s{url}"
            | r -> return Result.Ok r
        }

let repositoryRoot =
    IO.Path.GetFullPath(IO.Path.Combine(__SOURCE_DIRECTORY__, ".."))

let runProcess (fileName: string) (arguments: string list) =
    task {
        let startInfo = ProcessStartInfo(fileName)
        startInfo.WorkingDirectory <- repositoryRoot
        startInfo.UseShellExecute <- false
        startInfo.RedirectStandardOutput <- true
        startInfo.RedirectStandardError <- true

        for argument in arguments do
            startInfo.ArgumentList.Add argument

        use child = new Process()
        child.StartInfo <- startInfo

        if child.Start() |> not then
            failtestf "Failed to start %s" fileName

        let outputTask = child.StandardOutput.ReadToEndAsync()
        let errorTask = child.StandardError.ReadToEndAsync()
        do! child.WaitForExitAsync()
        let! output = outputTask
        let! error = errorTask

        if child.ExitCode <> 0 then
            failtestf "%s exited with code %d.\nstdout:\n%s\nstderr:\n%s" fileName child.ExitCode output error
    }
