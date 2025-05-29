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


[<Tests>]
let tests =
    testList "samples" [

        test "PlayWright" {

            use server = new DevServer()

            task {
                use! playwright = Playwright.CreateAsync()
                let! page = playwright.NewChromiumPage()

                let index: string = $"http://localhost:%d{server.Port}%s{server.Root}/index.html"
                let! response = page.GotoAndCheck(index)

                match response with
                | Error msg -> failwithf "%s" msg
                | Ok _ -> ()

                let! title = page.TitleAsync()

                printfn "title is '%s'" title

            }
            |> Async.AwaitTask
            |> Async.RunSynchronously
        }

    ]
