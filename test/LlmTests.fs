module LlmTests

open System
open Expecto
open System.Net.Http
open TestHelpers

let private publishedMarkdown =
    testCaseTask "Markdown exports serve published content only"
    <| fun () ->
        task {
            use! (server: DevServer) = DevServer.StartAsync()
            use client = new HttpClient()
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
                use! response: HttpResponseMessage = client.GetAsync(baseUrl + path)
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

            use! futureResponse: HttpResponseMessage =
                client.GetAsync(baseUrl + "/posts/2077-01-01-future-post.html.md")

            let! futureContent: string = futureResponse.Content.ReadAsStringAsync()

            if futureContent.Contains "future-post" then
                failtest "Future post Markdown export should not be published"

            if futureContent.StartsWith "<!DOCTYPE html>" |> not then
                failtest "Future post Markdown export should fall back to the 404 page"
        }

let private booklogMarkdown =
    testCaseTask "Booklog Markdown exports preserve reading records"
    <| fun () ->
        task {
            use! (server: DevServer) = DevServer.StartAsync()
            use client = new HttpClient()
            let baseUrl: string = $"http://localhost:%d{server.Port}%s{server.Root}"
            let path = "/booklogs/a-book.html.md"

            use! response: HttpResponseMessage = client.GetAsync(baseUrl + path)
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
        }

let private discoveryLinks =
    testCaseTask "HTML detail pages advertise LLM resources"
    <| fun () ->
        task {
            use! (server: DevServer) = DevServer.StartAsync()
            use client = new HttpClient()
            let baseUrl: string = $"http://localhost:%d{server.Port}%s{server.Root}"
            let siteUrl = "https://krymtkts.github.io/blog-fable"

            let detailPaths =
                [
                    "/posts/2023-03-01-sample-post.html"
                    "/pages/sampla-page.html"
                    "/booklogs/a-book.html"
                ]

            for path in detailPaths do
                use! response: HttpResponseMessage = client.GetAsync(baseUrl + path)
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
                use! response: HttpResponseMessage = client.GetAsync(baseUrl + path)
                let! content: string = response.Content.ReadAsStringAsync()

                if response.IsSuccessStatusCode |> not then
                    failtestf "Failed to load non-detail page %s: %O" path response.StatusCode

                if content.Contains "rel=\"alternate\"" || content.Contains "rel=\"describedby\"" then
                    failtestf "Non-detail page should not advertise page-specific LLM resources: %s" path
        }

let private llmsIndex =
    testCaseTask "llms.txt lists published Markdown exports without auto-generated descriptions"
    <| fun () ->
        task {
            use! (server: DevServer) = DevServer.StartAsync()
            use client = new HttpClient()
            let baseUrl: string = $"http://localhost:%d{server.Port}%s{server.Root}"

            use! response: HttpResponseMessage = client.GetAsync(baseUrl + "/llms.txt")
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
        }

let private disabledOutput =
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

let tests =
    testList "LLM output" [
        publishedMarkdown
        booklogMarkdown
        discoveryLinks
        llmsIndex
        disabledOutput
    ]
