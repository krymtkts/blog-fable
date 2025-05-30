module Tests

open System
open Expecto
open Expecto.Flip

open Microsoft.Playwright

open DevServer
open Suave
open System.Threading

(*
This tests requires the Playwright CLI to be installed.
ex) PS> ./test/bin/Debug/*/playwright.ps1 install
*)

type DevServer() =
    let home = IO.Path.Join [| __SOURCE_DIRECTORY__; ".."; "docs" |]
    let port = port
    let root = "/blog-fable"
    let webServer = startWebServerAsync (suaveConfig home) (webpart root) |> snd
    let cancellationTokenSource = new CancellationTokenSource()

    do
        Async.Start(webServer, cancellationTokenSource.Token)
        printfn $"Dev server started at http://localhost:%d{port}%s{root}"

    member __.Port = port
    member __.Root = root

    interface IDisposable with
        member __.Dispose() =
            printfn "Stopping dev server..."
            cancellationTokenSource.Cancel()

type IPlaywright with
    member __.NewChromiumPage() =
        task {
            let! browser = __.Chromium.LaunchAsync()
            return! browser.NewPageAsync()
        }

type IPage with
    member __.GotoAndCheck(url: string) =
        task {
            let! response = __.GotoAsync(url)

            match response with
            | null -> return Error "Failed to load page: %s{url}"
            | r when not r.Ok -> return Error "Failed to load page: %s{url}"
            | r -> return Ok r
        }

let snapshotDir = IO.Path.Combine(__SOURCE_DIRECTORY__, "snapshots")

let ensureSnapshotDir () =
    if snapshotDir |> IO.Directory.Exists |> not then
        snapshotDir |> IO.Directory.CreateDirectory |> ignore

let getSnapshotPath (path: string) =
    if "http" |> path.StartsWith then
        failwith "Path should not start with 'http'. Use relative paths instead."

    let fileName = path.Replace("/", "_")
    IO.Path.Combine(snapshotDir, fileName + ".snapshot")

let saveSnapshot (path: string) (content: string) =
    IO.File.WriteAllTextAsync(path, content, Text.Encoding.UTF8)

let loadSnapshot (path: string) =
    task {
        if path |> IO.File.Exists then
            let! content = IO.File.ReadAllTextAsync(path, Text.Encoding.UTF8)
            return content |> Some
        else
            return None
    }


[<Tests>]
let tests =
    testList "snapshot testing" [

        testAsync "comparison" {

            let paths =
                [

                  "/index.html"

                  ]

            use server = new DevServer()
            let baseUrl: string = $"http://localhost:%d{server.Port}%s{server.Root}"
            ensureSnapshotDir ()

            return!
                // TODO: i want to use testTask here, but i don't know how to convert it.
                task {
                    use! playwright = Playwright.CreateAsync()
                    let! page = playwright.NewChromiumPage()

                    for path in paths do
                        let url = baseUrl + path
                        let snapshotPath = getSnapshotPath path

                        printfn "Loading %s..." url

                        let! response = url |> page.GotoAndCheck

                        match response with
                        | Error msg -> failwithf "%s" msg
                        | Ok _ -> ()

                        let locator = "html" |> page.Locator
                        let! content = locator.AriaSnapshotAsync()
                        let! expectedContent = snapshotPath |> loadSnapshot

                        match expectedContent with
                        | Some expectedContent -> content |> Expect.equal $"Content mismatch for {url}" expectedContent
                        | None ->
                            do! saveSnapshot snapshotPath content
                            printfn $"Saved new snapshot for %s{url} to %s{snapshotPath}"

                }
                |> Async.AwaitTask
        }

    ]
