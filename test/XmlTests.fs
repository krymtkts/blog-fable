module XmlTests

open System
open System.Globalization
open System.Net.Http
open System.Xml.Linq
open Expecto
open TestHelpers

let private elementValue (ns: XNamespace) field (element: XElement) =
    element.Element(ns + field)
    |> Option.ofObj
    |> fun child -> Expect.wantSome child $"Missing XML element: %O{ns + field}"
    |> _.Value

let private siteUrl = "https://krymtkts.github.io/blog-fable"

let private publishedPosts =
    [
        "/posts/2023-09-10-blog-fable.html"
        "/posts/2023-04-01-default-color-scheme.html"
        "/posts/2023-03-01-sample-post.html"
        "/posts/2023-02-01-about-markdown-parser.html"
        "/posts/2023-01-01-sample-post-without-front-matter.html"
        "/posts/2022-12-31-flatten-posts-in-nested-directory.html"
    ]
    |> List.map ((+) siteUrl)

let private rss =
    testCaseTask "RSS lists published posts in reverse chronological order"
    <| fun () ->
        task {
            use! (server: DevServer) = DevServer.StartAsync()
            use client = new HttpClient()

            use! response = client.GetAsync($"%s{server.BaseUrl}/feed.xml")

            Expect.isTrue response.IsSuccessStatusCode "RSS should be served successfully"
            let! content = response.Content.ReadAsStringAsync()
            let document = XDocument.Parse content

            let root =
                document.Root
                |> Option.ofObj
                |> fun root -> Expect.wantSome root "RSS root is missing"

            let name = XName.Get
            Expect.equal root.Name (name "rss") "RSS root element"
            let version = root.Attribute(name "version") |> Option.ofObj |> Option.map _.Value
            Expect.equal version (Some "2.0") "RSS version"

            let channel =
                root.Element(name "channel")
                |> Option.ofObj
                |> fun channel -> Expect.wantSome channel "RSS channel is missing"

            let value = elementValue XNamespace.None

            Expect.equal (value "title" channel) "Blog Fable" "RSS channel title"
            Expect.equal (value "link" channel) siteUrl "RSS channel URL"

            let items = channel.Elements(name "item") |> Seq.toList
            let links = items |> List.map (value "link")
            Expect.equal links publishedPosts "RSS should contain only published posts, newest first"

            for item in items do
                Expect.equal (value "guid" item) (value "link" item) "RSS GUID should match its canonical URL"
                Expect.isNonEmpty (value "title" item) "RSS item title"
                Expect.isNonEmpty (value "description" item) "RSS item content"

                Expect.isTrue
                    (DateTimeOffset.TryParse(value "pubDate" item, CultureInfo.InvariantCulture, DateTimeStyles.None)
                     |> fst)
                    "RSS publication date"

            let sample =
                items
                |> List.find (fun item -> value "link" item = siteUrl + "/posts/2023-03-01-sample-post.html")

            Expect.equal (value "title" sample) "Sample post - subtitle" "RSS should preserve subtitles"
        }

let private sitemap =
    testCaseTask "Sitemap includes published pages and excludes unpublished resources"
    <| fun () ->
        task {
            use! (server: DevServer) = DevServer.StartAsync()
            use client = new HttpClient()

            use! response = client.GetAsync($"%s{server.BaseUrl}/sitemap.xml")

            Expect.isTrue response.IsSuccessStatusCode "Sitemap should be served successfully"
            let! content = response.Content.ReadAsStringAsync()
            let document = XDocument.Parse content

            let root =
                document.Root
                |> Option.ofObj
                |> fun root -> Expect.wantSome root "Sitemap root is missing"

            let ns = XNamespace.Get "http://www.sitemaps.org/schemas/sitemap/0.9"
            Expect.equal root.Name (ns + "urlset") "Sitemap root and namespace"

            let value = elementValue ns

            let entries = root.Elements(ns + "url") |> Seq.toList
            let links = entries |> List.map (value "loc")
            Expect.isNonEmpty links "Sitemap should contain URLs"
            Expect.equal (List.distinct links) links "Sitemap should not duplicate URLs"

            let otherPages =
                [
                    "/index.html"
                    "/archives.html"
                    "/tags.html"
                    "/booklogs.html"
                    "/booklogs/2022.html"
                    "/booklogs/2023.html"
                    "/booklogs/2024.html"
                    "/booklogs/a-book.html"
                    "/booklogs/b-book.html"
                    "/booklogs/c-book.html"
                    "/booklogs/d-book.html"
                    "/pages/about.html"
                    "/pages/sampla-page.html"
                    "/pages/sampla-page-without-front-matter.html"
                ]
                |> List.map ((+) siteUrl)

            for url in publishedPosts @ otherPages do
                Expect.contains links url "Published page should appear in sitemap"

            for entry in entries do
                let url = value "loc" entry

                Expect.isTrue
                    (url.StartsWith(siteUrl + "/", StringComparison.Ordinal))
                    "Sitemap URL should use the public site root"

                Expect.isFalse (url.Contains "2077-01-01-future-post") "Future post should not appear in sitemap"

                Expect.isFalse
                    (url.EndsWith ".md" || url.EndsWith "/404.html" || url.EndsWith "/llms.txt")
                    "Sitemap should list public HTML pages only"

                let relative =
                    (Uri url).AbsolutePath.Substring(server.Root.Length + 1)
                    |> Uri.UnescapeDataString

                let filePath =
                    IO.Path.Combine(server.OutputRoot, relative.Replace('/', IO.Path.DirectorySeparatorChar))

                Expect.isTrue (IO.File.Exists filePath) $"Sitemap URL should resolve to a generated file: %s{url}"

                Expect.isTrue
                    (DateOnly.TryParseExact(
                        value "lastmod" entry,
                        "yyyy-MM-dd",
                        CultureInfo.InvariantCulture,
                        DateTimeStyles.None
                     )
                     |> fst)
                    "Sitemap last modification date"

            let sample =
                entries
                |> List.find (fun entry -> value "loc" entry = siteUrl + "/posts/2023-03-01-sample-post.html")

            Expect.equal (value "lastmod" sample) "2023-03-02" "Sitemap should use the explicit modification date"
        }

let tests = testList "XML output" [ rss; sitemap ]
