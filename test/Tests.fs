module Tests

open System
open Expecto
open Expecto.Flip

open Microsoft.Playwright

open DevServer
open Suave
open System.Threading
open System.Threading.Tasks

(*
This tests requires the Playwright CLI to be installed.
ex) PS> ./test/bin/Debug/*/playwright.ps1 install
*)

type DevServer() =
    let home = IO.Path.Join [| __SOURCE_DIRECTORY__; ".."; "docs" |]
    let port = port
    let root = "/blog-fable"
    let cancellationTokenSource = new CancellationTokenSource()

    do
        startWebServerAsync (suaveConfig home cancellationTokenSource.Token) (webpart root)
        |> ignore

        printfn $"Dev server started at http://localhost:%d{port}%s{root}"

    member __.Port = port
    member __.Root = root

    interface IDisposable with
        member __.Dispose() =
            printfn "Stopping dev server..."
            cancellationTokenSource.Cancel()

    interface IAsyncDisposable with
        member __.DisposeAsync() =
            task {
                printfn "Stopping dev server asynchronously..."
                do! cancellationTokenSource.CancelAsync()
            }
            |> ValueTask

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

let overwriteSnapshotsEnabled () =
    Environment.GetEnvironmentVariable "BLOG_FABLE_UPDATE_SNAPSHOTS"
    |> String.IsNullOrEmpty
    |> not

[<Tests>]
let tests =
    testList "snapshot testing" [

        testTask "comparison" {

            let paths =
                [

                  "/index.html"
                  "/archives.html"
                  "/pages/about.html"
                  "/pages/sampla-page-without-front-matter.html"
                  "/pages/sampla-page.html"
                  "/posts/2022-12-31-flatten-posts-in-nested-directory.html"
                  "/posts/2023-01-01-sample-post-without-front-matter.html"
                  "/posts/2023-02-01-about-markdown-parser.html"
                  "/posts/2023-03-01-sample-post.html"
                  "/posts/2023-04-01-default-color-scheme.html"
                  "/posts/2023-09-10-blog-fable.html"
                  "/tags.html"
                  "/tags/fsharp.html"
                  "/tags/image.html"
                  "/tags/markdown.html"
                  "/tags/sample.html"
                  "/tags/t.html"
                  "/tags/tag.html"
                  "/tags/this-is-extreme-long-tag-name.html"
                  "/tags/yaml.html"
                  "/booklogs.html"
                  "/booklogs/2022.html"
                  "/booklogs/2023.html"
                  "/booklogs/2024.html"
                  "/booklogs/a-book.html"
                  "/booklogs/b-book.html"
                  "/booklogs/c-book.html"
                  "/booklogs/d-book.html"
                  "/404.html"
                  "/xxx.html" // Test for 404 page

                  ]

            use server = new DevServer()
            let baseUrl: string = $"http://localhost:%d{server.Port}%s{server.Root}"
            ensureSnapshotDir ()

            let! (playwright: IPlaywright) = Playwright.CreateAsync()
            use _ = PlaywrightAsyncDisposable playwright
            let! (page: IPage) = playwright.NewChromiumPage()
            let overwriteSnapshots = overwriteSnapshotsEnabled ()

            for path in paths do
                let url = baseUrl + path
                let snapshotPath = getSnapshotPath path

                printfn "Loading %s..." url

                let! response = url |> page.GotoAndCheck

                match response with
                | Result.Error msg -> failwith $"%s{msg}"
                | Result.Ok _ -> ()

                let! actual = "html" |> page.Locator |> _.AriaSnapshotAsync()
                let! expectedContent = snapshotPath |> loadSnapshot

                if overwriteSnapshots then
                    do! saveSnapshot snapshotPath actual
                    printfn $"Overwrote snapshot for %s{url} to %s{snapshotPath}"

                expectedContent
                |> Expect.wantSome $"Snapshot not found for %s{url}. Expected at %s{snapshotPath}"
                |> fun expected -> Expect.equal $"Content mismatch for %s{url}" expected actual

        }

    ]
