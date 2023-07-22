module App

open StaticWebGenerator

type Mode =
    | Development
    | Production

let private render stage =
    promise {
        let siteName = "Blog Title"
        let pathRoot = "/blog-fable"

        let navs =
            [ Title
                  { text = siteName
                    path = "/index.html"
                    sitemap = Yes "1.0" }
              Link
                  { text = "Archives"
                    path = "/archives.html"
                    sitemap = Yes "0.9" }
              Link
                  { text = "Tags"
                    path = "/tags.html"
                    sitemap = Yes "0.9" }
              Link
                  { text = "About Me"
                    path = "/pages/about.html"
                    sitemap = No }
              Link
                  { text = "RSS"
                    path = "/feed.xml"
                    sitemap = No } ]

        let navbar, navSitemap = generateNavbar pathRoot navs

        let devInjection, devScript =
            match stage with
            | Development -> Some("/js/live-reload.js"), [ ("js/live-reload.js", "docs/blog-fable/js/live-reload.js") ]
            | Production -> None, []

        let site: FixedSiteContent =
            { lang = "ja"
              navbar = navbar
              name = siteName
              title = siteName
              description = "Blog Description"
              url = "https://krymtkts.github.io"
              pathRoot = pathRoot
              copyright = "2023 krymtkts"
              favicon = "/img/favicon.ico"
              devInjection = devInjection }

        let renderPostAndPages = renderMarkdowns site "/tags"
        let! metaPosts = renderPostAndPages "contents/posts" "docs/blog-fable/posts"
        let! metaPages = renderPostAndPages "contents/pages" "docs/blog-fable/pages"

        do! renderIndex site "/tags" metaPosts "docs/blog-fable/index.html"

        let archiveDefs =
            [ Posts
                  { title = "Posts"
                    metas = metaPosts
                    root = "/posts"
                    priority = "0.8" }
              Pages
                  { title = "Pages"
                    metas = metaPages
                    root = "/pages"
                    priority = "0.8" } ]

        let! archiveLocs = renderArchives site archiveDefs "docs/blog-fable/archives.html"

        let tagDef =
            { title = "Tags"
              metas = Seq.concat [ metaPosts; metaPages ]
              tagRoot = "/tags"
              postRoot = "/posts"
              priority = "0.9" }

        let! tagLocs = renderTags site tagDef "docs/blog-fable/tags.html"
        do! render404 site "docs/blog-fable/404.html"

        do!
            renderSitemap
                site.url
                "docs/blog-fable/sitemap.xml"
                (Seq.concat [ navSitemap
                              tagLocs
                              archiveLocs ])

        do!
            renderFeed
                { title = siteName
                  description = site.description
                  link = $"{site.url}/{site.pathRoot}"
                  feed = "/feed.xml"
                  generator = "blog-fable"
                  postRoot = "/posts"
                  posts = metaPosts }
                "docs/blog-fable/feed.xml"

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
