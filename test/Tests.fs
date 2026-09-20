module Tests

open System
open Expecto
open Expecto.Flip

open Microsoft.Playwright

open DevServer
open Suave
open System.Threading
open System.Threading.Tasks
open System.Net.Http
open System.Diagnostics

(*
NOTE: This tests requires the Playwright CLI to be installed.
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

[<Tests>]
let tests =
    testSequenced
    <| testList "snapshot testing" [

        testTask "comparison" {

            let paths =
                [
                    ""
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
            let failures = ResizeArray<string>()

            for path in paths do
                let url = baseUrl + path
                let snapshotPath = getSnapshotPath path

                printfn "Loading %s..." url

                let! response = url |> page.GotoAndCheck

                match response with
                | Result.Error msg -> failwith $"%s{msg}"
                | _ -> ()

                let! actual = "html" |> page.Locator |> _.AriaSnapshotAsync()
                let! expectedContent = snapshotPath |> loadSnapshot

                if overwriteSnapshots then
                    do! saveSnapshot snapshotPath actual
                    printfn $"Overwrote snapshot for %s{url} to %s{snapshotPath}"

                try
                    expectedContent
                    |> Expect.wantSome $"Snapshot not found for %s{url}. Expected at %s{snapshotPath}"
                    |> fun expected -> Expect.equal $"Content mismatch for %s{url}" expected actual
                with ex ->
                    failures.Add $"Snapshot test failed for %s{url}: %s{ex.Message}"

            if failures.Count > 0 then
                failtestf "Snapshot test failed for the following URLs:\n%s" (String.concat "\n" failures)

        }

        testTask "Markdown exports serve published content only" {
            use server = new DevServer()
            let client = new HttpClient()
            let baseUrl: string = $"http://localhost:%d{server.Port}%s{server.Root}"

            let pages =
                [
                    ("/posts/2023-03-01-sample-post.html.md",
                     "# Sample post - subtitle",
                     "Posts have front matter.",
                     "- URL: <https://krymtkts.github.io/blog-fable/posts/2023-03-01-sample-post.html.md>")
                    ("/pages/sampla-page-without-front-matter.html.md",
                     "# sampla-page-without-front-matter",
                     "The page can omit front matter",
                     "- URL: <https://krymtkts.github.io/blog-fable/pages/sampla-page-without-front-matter.html.md>")
                ]

            for path, expectedTitle, expectedBody, expectedUrl in pages do
                let! response: HttpResponseMessage = client.GetAsync(baseUrl + path)
                let! content: string = response.Content.ReadAsStringAsync()

                if response.IsSuccessStatusCode |> not then
                    failtestf "Failed to load Markdown export %s: %O" path response.StatusCode

                if content.StartsWith expectedTitle |> not then
                    failtestf "Markdown export has unexpected title for %s: %s" path content

                if content.Contains expectedBody |> not then
                    failtestf "Markdown export does not contain source content for %s: %s" path content

                if content.Contains expectedUrl |> not then
                    failtestf "Markdown export does not contain an autolink URL for %s: %s" path content

                if content.StartsWith "<!DOCTYPE html>" then
                    failtestf "Markdown export should not be an HTML document: %s" path

                if content.StartsWith "---" then
                    failtestf "Markdown export should not start with front matter: %s" path

            let! futureResponse: HttpResponseMessage =
                client.GetAsync(baseUrl + "/posts/2077-01-01-future-post.html.md")

            let! futureContent: string = futureResponse.Content.ReadAsStringAsync()

            if futureContent.Contains "future-post" then
                failtest "Future post Markdown export should not be published"

            if futureContent.StartsWith "<!DOCTYPE html>" |> not then
                failtest "Future post Markdown export should fall back to the 404 page"

            client.Dispose()
        }

        testTask "Booklog Markdown exports preserve reading records" {
            use server = new DevServer()
            let client = new HttpClient()
            let baseUrl: string = $"http://localhost:%d{server.Port}%s{server.Root}"
            let path = "/booklogs/a-book.html.md"

            let! response: HttpResponseMessage = client.GetAsync(baseUrl + path)
            let! content: string = response.Content.ReadAsStringAsync()

            if response.IsSuccessStatusCode |> not then
                failtestf "Failed to load booklog Markdown export %s: %O" path response.StatusCode

            for expected in
                [
                    "# Booklog - A book"
                    "Author: Jane Doe"
                    "## 2023-01-01"
                    "- Read count: n+1"
                    "- Pages: 1 ~ 9 (pages read: 9)"
                    "start day of Jan."
                ] do
                if content.Contains expected |> not then
                    failtestf "Booklog Markdown export does not contain %s: %s" expected content

            if content.StartsWith "<!DOCTYPE html>" then
                failtest "Booklog Markdown export should not be an HTML document"

            client.Dispose()
        }

        testTask "HTML detail pages advertise LLM resources" {
            use server = new DevServer()
            let client = new HttpClient()
            let baseUrl: string = $"http://localhost:%d{server.Port}%s{server.Root}"
            let siteUrl = "https://krymtkts.github.io/blog-fable"

            let detailPaths =
                [
                    "/posts/2023-03-01-sample-post.html"
                    "/pages/sampla-page.html"
                    "/booklogs/a-book.html"
                ]

            for path in detailPaths do
                let! response: HttpResponseMessage = client.GetAsync(baseUrl + path)
                let! content: string = response.Content.ReadAsStringAsync()

                if response.IsSuccessStatusCode |> not then
                    failtestf "Failed to load HTML detail page %s: %O" path response.StatusCode

                if
                    content.Contains "rel=\"alternate\"" |> not
                    || content.Contains "type=\"text/markdown\"" |> not
                    || content.Contains($"href=\"%s{siteUrl}%s{path}.md\"") |> not
                then
                    failtestf "HTML detail page does not advertise its Markdown export: %s" path

                if
                    content.Contains "rel=\"describedby\"" |> not
                    || content.Contains($"href=\"%s{siteUrl}/llms.txt\"") |> not
                then
                    failtestf "HTML detail page does not advertise llms.txt: %s" path

            for path in [ "/index.html"; "/archives.html"; "/booklogs.html"; "/404.html" ] do
                let! response: HttpResponseMessage = client.GetAsync(baseUrl + path)
                let! content: string = response.Content.ReadAsStringAsync()

                if response.IsSuccessStatusCode |> not then
                    failtestf "Failed to load non-detail page %s: %O" path response.StatusCode

                if content.Contains "rel=\"alternate\"" || content.Contains "rel=\"describedby\"" then
                    failtestf "Non-detail page should not advertise page-specific LLM resources: %s" path

            client.Dispose()
        }

        testTask "llms.txt lists published Markdown exports without auto-generated descriptions" {
            use server = new DevServer()
            let client = new HttpClient()
            let baseUrl: string = $"http://localhost:%d{server.Port}%s{server.Root}"

            let! response: HttpResponseMessage = client.GetAsync(baseUrl + "/llms.txt")
            let! content: string = response.Content.ReadAsStringAsync()

            if response.IsSuccessStatusCode |> not then
                failtestf "Failed to load llms.txt: %O" response.StatusCode

            for expected in [ "# Blog Fable"; "## Posts"; "## Pages"; "## Booklogs" ] do
                if content.Contains expected |> not then
                    failtestf "llms.txt does not contain %s: %s" expected content

            for section in [ "Posts"; "Pages"; "Booklogs" ] do
                if content.Contains($"## %s{section}\n\n-") |> not then
                    failtestf "llms.txt should separate the %s heading from its links: %s" section content

            let hasGeneratedDescription (line: string) =
                line.StartsWith "- ["
                && (line.Contains "/posts/" || line.Contains "/pages/")
                && line.Contains "): "

            if content.Split('\n') |> Array.exists hasGeneratedDescription then
                failtest "llms.txt should not generate descriptions for posts or pages"

            if content.Contains "): Jane Doe" |> not then
                failtest "llms.txt should preserve explicit booklog descriptions"

            let assertLinksInOrder (section: string) (urls: string list) =
                let positions = urls |> List.map (fun url -> content.IndexOf url)

                if positions |> List.exists (fun position -> position < 0) then
                    failtestf "llms.txt does not contain all expected %s links: %s" section (String.concat ", " urls)

                if
                    positions
                    |> List.pairwise
                    |> List.exists (fun (previous, current) -> previous >= current)
                then
                    failtestf
                        "llms.txt does not list %s links in the expected order: %s"
                        section
                        (String.concat ", " urls)

            assertLinksInOrder "Posts" [
                "2023-09-10-blog-fable.html.md"
                "2023-04-01-default-color-scheme.html.md"
                "2023-03-01-sample-post.html.md"
                "2023-02-01-about-markdown-parser.html.md"
                "2023-01-01-sample-post-without-front-matter.html.md"
                "2022-12-31-flatten-posts-in-nested-directory.html.md"
            ]

            assertLinksInOrder "Booklogs" [ "c-book.html.md"; "b-book.html.md"; "a-book.html.md"; "d-book.html.md" ]

            let outputRoot =
                System.IO.Path.Combine(__SOURCE_DIRECTORY__, "..", "docs", "blog-fable")
                |> System.IO.Path.GetFullPath

            let urls =
                [ "posts"; "pages"; "booklogs" ]
                |> List.collect (fun root ->
                    System.IO.Directory.GetFiles(System.IO.Path.Combine(outputRoot, root), "*.html.md")
                    |> Array.map (fun path ->
                        let relative = System.IO.Path.GetRelativePath(outputRoot, path).Replace("\\", "/")

                        $"https://krymtkts.github.io/blog-fable/%s{relative}")
                    |> Array.toList)

            if urls.IsEmpty then
                failtest "No Markdown exports were found"

            for url in urls do
                if content.Contains url |> not then
                    failtestf "llms.txt does not link to %s: %s" url content

            if content.Contains "2077-01-01-future-post.html.md" then
                failtest "llms.txt should not link to the future post"

            if content.Contains "## Archives" then
                failtest "llms.txt should not contain an Archives section"

            client.Dispose()
        }

        testTask "Pagefind filters classify archive and booklog results" {
            use server = new DevServer()
            let baseUrl: string = $"http://localhost:%d{server.Port}%s{server.Root}"

            let! (playwright: IPlaywright) = Playwright.CreateAsync()
            use _ = PlaywrightAsyncDisposable playwright
            let! (page: IPage) = playwright.NewChromiumPage()

            let! response = $"%s{baseUrl}/index.html" |> page.GotoAndCheck

            match response with
            | Result.Error msg -> failwith $"%s{msg}"
            | _ -> ()

            let filtersTask: Task<string> =
                page.EvaluateAsync<string>
                    """
                    async () => {
                        const pagefind = await import("/blog-fable/pagefind/pagefind.js");
                        return JSON.stringify(await pagefind.filters());
                    }
                """

            let! filters = filtersTask

            for expected: string in [ "\"section\""; "\"archive\""; "\"booklog\""; "\"tag\""; "\"sample\"" ] do
                if (filters: string).Contains expected |> not then
                    failtestf "Pagefind filters did not contain %s: %s" expected filters

            let booklogUrlsTask: Task<string array> =
                page.EvaluateAsync<string array>
                    """
                    async () => {
                        const pagefind = await import("/blog-fable/pagefind/pagefind.js");
                        const search = await pagefind.search(null, {
                            filters: { section: "booklog" }
                        });
                        const data = await Promise.all(search.results.map(result => result.data()));
                        return data.map(item => item.url);
                    }
                """

            let! booklogUrls = booklogUrlsTask

            if
                (booklogUrls: string array).Length = 0
                || booklogUrls
                   |> Array.exists (fun (url: string) -> url.Contains("/booklogs/") |> not)
            then
                failtestf "Booklog filter returned unexpected URLs: %s" (String.concat ", " booklogUrls)

            let taggedArchiveUrlsTask: Task<string array> =
                page.EvaluateAsync<string array>
                    """
                    async () => {
                        const pagefind = await import("/blog-fable/pagefind/pagefind.js");
                        const search = await pagefind.search(null, {
                            filters: { section: "archive", tag: "sample" }
                        });
                        const data = await Promise.all(search.results.map(result => result.data()));
                        return data.map(item => item.url);
                    }
                """

            let! taggedArchiveUrls = taggedArchiveUrlsTask

            if
                (taggedArchiveUrls: string array).Length = 0
                || taggedArchiveUrls
                   |> Array.exists (fun (url: string) -> url.Contains("/booklogs/"))
            then
                failtestf "Tagged archive filter returned unexpected URLs: %s" (String.concat ", " taggedArchiveUrls)

            let! dropdownCount = page.Locator("pagefind-filter-dropdown").CountAsync()

            if dropdownCount <> 2 then
                failtestf "Expected two Pagefind filter dropdowns, but found %d" dropdownCount

            let modalTrigger = page.Locator("pagefind-modal-trigger .pf-trigger-btn")
            do! modalTrigger.WaitForAsync()
            do! modalTrigger.ClickAsync()

            let! modalScrollbarColor =
                page
                    .Locator(".pf-modal-body")
                    .EvaluateAsync<string>("element => getComputedStyle(element).scrollbarColor")

            if modalScrollbarColor = "auto" then
                failtest "Pagefind modal scrollbar did not use the site theme"

            let tagDropdown = page.Locator("pagefind-filter-dropdown[filter='tag']")
            do! tagDropdown.WaitForAsync()

            let sectionDropdown = page.Locator("pagefind-filter-dropdown[filter='section']")
            let sectionTrigger = sectionDropdown.Locator(".pf-dropdown-trigger")
            do! sectionTrigger.WaitForAsync()
            do! sectionTrigger.ClickAsync()

            let! dropdownScrollbarColor =
                sectionDropdown
                    .Locator(".pf-dropdown-options")
                    .EvaluateAsync<string>("element => getComputedStyle(element).scrollbarColor")

            if dropdownScrollbarColor = "auto" then
                failtest "Pagefind filter scrollbar did not use the site theme"

            let! dropdownOverflow =
                sectionDropdown
                    .Locator(".pf-dropdown-menu")
                    .EvaluateAsync<string>("element => getComputedStyle(element).overflowY")

            if dropdownOverflow <> "visible" then
                failtestf "Pagefind filter menu should not be a second scroll container: %s" dropdownOverflow

            let booklogOption = sectionDropdown.Locator("[role='option'][data-value='booklog']")
            do! booklogOption.WaitForAsync()
            do! booklogOption.ClickAsync()

            let! sectionTriggerLabel = sectionTrigger.GetAttributeAsync("aria-label")

            match sectionTriggerLabel |> Option.ofObj with
            | Some label when label = "Section, 1 filter selected" -> ()
            | Some label -> failtestf "Unexpected selected section label: %s" label
            | None -> failtest "The selected section label was null"

            let hiddenOptions = LocatorWaitForOptions()
            hiddenOptions.State <- WaitForSelectorState.Hidden
            do! tagDropdown.WaitForAsync(hiddenOptions)
        }

        testTask "Pagefind tag filters use AND semantics" {
            use server = new DevServer()
            let baseUrl: string = $"http://localhost:%d{server.Port}%s{server.Root}"

            let! (playwright: IPlaywright) = Playwright.CreateAsync()
            use _ = PlaywrightAsyncDisposable playwright
            let! (page: IPage) = playwright.NewChromiumPage()

            let! response = $"%s{baseUrl}/index.html" |> page.GotoAndCheck

            match response with
            | Result.Error msg -> failwith $"%s{msg}"
            | _ -> ()

            let modalTrigger = page.Locator("pagefind-modal-trigger .pf-trigger-btn")
            do! modalTrigger.WaitForAsync()
            do! modalTrigger.ClickAsync()

            let tagDropdown = page.Locator("pagefind-filter-dropdown[filter='tag']")
            do! tagDropdown.WaitForAsync()

            let tagOptions = tagDropdown.Locator(".pf-dropdown-options")
            let attachedOptions = LocatorWaitForOptions()
            attachedOptions.State <- WaitForSelectorState.Attached
            do! tagOptions.WaitForAsync(attachedOptions)

            let! multiselectable = tagOptions.GetAttributeAsync("aria-multiselectable")

            match multiselectable |> Option.ofObj with
            | Some value when value = "true" -> ()
            | Some value -> failtestf "Tag filter should allow multiple selections: %s" value
            | None -> failtest "Tag filter did not expose aria-multiselectable"

            let tagTrigger = tagDropdown.Locator(".pf-dropdown-trigger")
            do! tagTrigger.ClickAsync()

            let sampleOption = tagDropdown.Locator("[role='option'][data-value='sample']")
            do! sampleOption.WaitForAsync()
            do! sampleOption.ClickAsync()

            let yamlOption = tagDropdown.Locator("[role='option'][data-value='yaml']")
            do! yamlOption.WaitForAsync()
            do! yamlOption.ClickAsync()

            let! selectedValues =
                page.EvaluateAsync<string array>
                    """
                    () => Array.from(document.querySelectorAll(
                        "pagefind-filter-dropdown[filter='tag'] [role='option'][aria-selected='true']"
                    ))
                        .map(element => element.dataset.value)
                        .sort()
                """

            let expectedSelectedValues = [| "sample"; "yaml" |]

            if selectedValues <> expectedSelectedValues then
                failtestf "Tag filter did not keep both selections: %s" (String.concat ", " selectedValues)

            let! tagTriggerLabel = tagTrigger.GetAttributeAsync("aria-label")

            match tagTriggerLabel |> Option.ofObj with
            | Some label when label = "Tag, 2 filters selected" -> ()
            | Some label -> failtestf "Unexpected selected tag label: %s" label
            | None -> failtest "The selected tag label was null"

            let tagUrlsTask: Task<string array> =
                page.EvaluateAsync<string array>
                    """
                    async () => {
                        const pagefind = await import("/blog-fable/pagefind/pagefind.js");
                        const search = await pagefind.search(null, {
                            filters: { tag: ["sample", "yaml"] }
                        });
                        const data = await Promise.all(search.results.map(result => result.data()));
                        return data
                            .map(item => new URL(item.url, location.href).pathname)
                            .sort();
                    }
                """

            let! tagUrls = tagUrlsTask

            let expectedTagUrls =
                [|
                    "/blog-fable/pages/sampla-page.html"
                    "/blog-fable/posts/2023-03-01-sample-post.html"
                |]

            if tagUrls <> expectedTagUrls then
                failtestf "Tag AND filter returned unexpected URLs: %s" (String.concat ", " tagUrls)
        }

        testTask "Disabled LLM output removes Markdown exports and discovery links" {
            let outputRoot = IO.Path.Combine(repositoryRoot, "docs", "blog-fable")
            let markdownRoot = IO.Path.Combine(outputRoot, "posts")
            let pagesRoot = IO.Path.Combine(outputRoot, "pages")
            let booklogsRoot = IO.Path.Combine(outputRoot, "booklogs")
            let llmsPath = IO.Path.Combine(outputRoot, "llms.txt")
            let mutable failure = None

            try
                do! runProcess "node" [ "src/App.fs.js"; "--no-llms" ]

                if IO.File.Exists llmsPath then
                    failtest "llms.txt should not be generated when LLM output is disabled"

                let markdownFiles =
                    [ markdownRoot; pagesRoot; booklogsRoot ]
                    |> List.collect (fun root ->
                        if IO.Directory.Exists root then
                            IO.Directory.GetFiles(root, "*.html.md", IO.SearchOption.AllDirectories)
                            |> Array.toList
                        else
                            [])

                if markdownFiles.IsEmpty |> not then
                    failtestf
                        "Markdown exports should not be generated when LLM output is disabled: %s"
                        (String.concat ", " markdownFiles)

                for relativePath in
                    [
                        "posts/2023-03-01-sample-post.html"
                        "pages/sampla-page.html"
                        "booklogs/a-book.html"
                    ] do
                    let path = IO.Path.Combine(outputRoot, relativePath)
                    let content = IO.File.ReadAllText path

                    if content.Contains "rel=\"alternate\"" || content.Contains "rel=\"describedby\"" then
                        failtestf "LLM discovery links should be omitted from %s" relativePath
            with ex ->
                failure <- Some ex

            do! runProcess "node" [ "src/App.fs.js" ]

            if IO.File.Exists llmsPath |> not then
                failtest "llms.txt was not restored after the disabled-output test"

            match failure with
            | Some ex -> return raise ex
            | None -> return ()
        }

    ]
