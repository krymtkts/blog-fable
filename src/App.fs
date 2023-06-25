module App

open StaticWebGenerator

type Mode =
    | Development
    | Production

let private render stage =
    promise {
        let title = "Blog Title"
        let copyright = "2023 krymtkts"

        let navi =
            [ Title
                  { text = title
                    path = "/blog-fable/index.html"
                    sitemap = Yes "1.0" }
              Link
                  { text = "Archives"
                    path = "/blog-fable/archives.html"
                    sitemap = Yes "0.9" }
              Link
                  { text = "Tags"
                    path = "/blog-fable/tags.html"
                    sitemap = Yes "0.9" }
              Link
                  { text = "About Me"
                    path = "/blog-fable/pages/about.html"
                    sitemap = No }
              Link
                  { text = "RSS"
                    path = "/blog-fable/atom.xml"
                    sitemap = No } ]

        let navbar, navSitemap = generateNavbar navi

        let devInjection, devScript =
            match stage with
            | Development ->
                Some("/blog-fable/live-reload.js"), [ ("js/live-reload.js", "docs/blog-fable/live-reload.js") ]
            | Production -> None, []

        let site: FixedSiteContent =
            { navbar = navbar
              title = title
              copyright = copyright
              favicon = "/blog-fable/img/favicon.ico"
              devInjection = devInjection }

        let renderPostAndPages = renderMarkdowns site "/blog-fable/tags"
        let! metaPosts = renderPostAndPages "contents/posts" "docs/blog-fable/posts"
        let! metaPages = renderPostAndPages "contents/pages" "docs/blog-fable/pages"

        do! renderIndex site "/blog-fable/tags" metaPosts "docs/blog-fable/index.html"

        let archiveDefs =
            [ Posts
                  { title = "Posts"
                    metas = metaPosts
                    root = "/blog-fable/posts"
                    priority = "0.8" }
              Pages
                  { title = "Pages"
                    metas = metaPages
                    root = "/blog-fable/pages"
                    priority = "0.8" } ]

        let! archiveLocs = renderArchives site archiveDefs "docs/blog-fable/archives.html"

        let tagDef =
            { title = "Tags"
              metas = Seq.concat [ metaPosts; metaPages ]
              root = "/blog-fable/tags"
              postRoot = "/blog-fable/posts"
              priority = "0.9" }

        let! tagLocs = renderTags site tagDef "docs/blog-fable/tags.html"
        do! render404 site "docs/blog-fable/404.html"

        do!
            renderSitemap
                "https://krymtkts.github.io"
                "docs/blog-fable/sitemap.xml"
                (Seq.concat [ navSitemap
                              tagLocs
                              archiveLocs ])

        do!
            copyResources
            <| [ ("contents/img/favicon.ico", "docs/blog-fable/img/favicon.ico") ]
               @ devScript

        printfn "Render complete!"
    }
    |> ignore

let dev =
    match List.ofSeq argv with
    | [ _; _; mode ] when mode = "dev" -> Development
    | _ -> Production

render dev
