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

[<Tests>]
let tests =
    testList "samples" [

        test "PlayWright" {

            printfn "Starting dev server..."

            let home = IO.Path.Join [| __SOURCE_DIRECTORY__; ".."; "docs" |]
            let root = "/blog-fable"
            let _, webServer = startWebServerAsync (cfg home) (app root)
            let cancellationTokenSource = new CancellationTokenSource()
            Async.Start(webServer, cancellationTokenSource.Token) |> ignore

            let index: string = $"http://localhost:%d{port}%s{root}/index.html"

            task {
                use! playwright = Playwright.CreateAsync()
                let! browser = playwright.Chromium.LaunchAsync()
                let! page = browser.NewPageAsync()

                let! response = page.GotoAsync(index)

                match response with
                | null -> failwithf "Failed to load page: %s" index
                | r when not r.Ok -> failwithf "Failed to load page: %s" index
                | _ -> ()

                let! title = page.TitleAsync()

                printfn "title is '%s'" title

            }
            |> Async.AwaitTask
            |> Async.RunSynchronously
        }

    ]
