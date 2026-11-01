module LinkTests

open System
open System.IO
open System.Threading.Tasks
open Expecto
open Microsoft.Playwright
open TestHelpers

let private internalLinks =
    testTask "Generated internal links resolve to pages and fragment targets" {
        use! (server: DevServer) = DevServer.StartAsync()
        use! (browser: TestBrowser) = TestBrowser.StartAsync()
        let page = browser.Page
        let baseUrl = $"http://localhost:%d{server.Port}%s{server.Root}"

        let outputRoot =
            Path.GetFullPath(Path.Combine(repositoryRoot, "docs", "blog-fable"))

        let origin = (Uri baseUrl).GetLeftPart(UriPartial.Authority)
        let root = server.Root + "/"
        let failures = ResizeArray<string>()

        let fragments =
            Collections.Generic.Dictionary<string, ResizeArray<string * string>>()

        let paths =
            Directory.GetFiles(outputRoot, "*.html", SearchOption.AllDirectories)
            |> Array.map (fun path -> Path.GetRelativePath(outputRoot, path).Replace('\\', '/'))
            |> Array.sort

        Expect.isNonEmpty paths "Generated HTML pages should exist"

        let mutable checkedLinks = 0

        for path in paths do
            let sourceUrl = $"%s{baseUrl}/%s{path}"
            let! response = page.GotoAndCheck sourceUrl

            match response with
            | Error message -> failtest message
            | Ok _ -> ()

            let! anchors = page.Locator("a[href]").AllAsync()

            let! (hrefs: (string | null) array) =
                anchors
                |> Seq.map (fun (anchor: ILocator) -> anchor.GetAttributeAsync("href"))
                |> Task.WhenAll

            for href in hrefs |> Array.choose Option.ofObj do
                match Uri.TryCreate(Uri sourceUrl, href) with
                | true, NonNull target when
                    target.GetLeftPart(UriPartial.Authority) = origin
                    && (target.AbsolutePath = server.Root
                        || target.AbsolutePath.StartsWith(root, StringComparison.Ordinal))
                    ->
                    checkedLinks <- checkedLinks + 1

                    let relative =
                        if target.AbsolutePath = server.Root then
                            ""
                        else
                            Uri.UnescapeDataString(target.AbsolutePath.Substring(root.Length))
                        |> fun relative -> if relative = "" then "index.html" else relative

                    let filePath =
                        Path.GetFullPath(Path.Combine(outputRoot, relative.Replace('/', Path.DirectorySeparatorChar)))

                    if
                        filePath.StartsWith(
                            outputRoot + string Path.DirectorySeparatorChar,
                            StringComparison.OrdinalIgnoreCase
                        )
                        && File.Exists filePath
                    then
                        if target.Fragment.Length > 1 then
                            let fragment = Uri.UnescapeDataString(target.Fragment.Substring 1)

                            match fragments.TryGetValue relative with
                            | true, targets -> targets.Add(path, fragment)
                            | false, _ -> fragments.Add(relative, ResizeArray [ (path, fragment) ])
                    else
                        failures.Add $"%s{path}: %s{href} does not resolve to a generated file"
                | _ -> ()

        Expect.isGreaterThan checkedLinks 0 "Generated pages should contain internal links"
        Expect.isGreaterThan fragments.Count 0 "Generated pages should contain links to fragments"

        for KeyValue(path, targets) in fragments do
            let! response = page.GotoAndCheck $"%s{baseUrl}/%s{path}"

            match response with
            | Error message -> failtest message
            | Ok _ -> ()

            let! elements = page.Locator("[id], a[name]").AllAsync()

            let! (ids: (string | null) array) =
                elements
                |> Seq.map (fun (element: ILocator) -> element.GetAttributeAsync("id"))
                |> Task.WhenAll

            let! (names: (string | null) array) =
                elements
                |> Seq.map (fun (element: ILocator) -> element.GetAttributeAsync("name"))
                |> Task.WhenAll

            let availableTargets = Seq.append ids names |> Seq.choose Option.ofObj |> Set.ofSeq

            for source, fragment in targets |> Seq.distinct do
                if not (Set.contains fragment availableTargets) then
                    failures.Add $"%s{source}: %s{path}#%s{fragment} has no matching fragment target"

        if failures.Count > 0 then
            failtestf "Broken internal links:\n%s" (String.concat "\n" failures)
    }

let tests = testList "Links" [ internalLinks ]
