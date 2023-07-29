module App

open StaticWebGenerator

type Mode =
    | Development
    | Production

type RnderOptions =
    { stage: Mode

      siteName: string
      description: string
      siteUrl: string
      pathRoot: string
      lang: string
      copyright: string
      favicon: string

      src: string
      dst: string

      postsRoot: string
      postsTitle: string
      pagesRoot: string
      paesTitle: string
      tagsRoot: string
      tagsTitle: string
      archivesRoot: string
      archivesTitle: string

      additionalNavs: Nav list

      feed: string

     }

let private render (opts: RnderOptions) =
    promise {
        let index = "/index.html"
        let feed = $"/{opts.feed}.xml"

        let navs =
            [ Title
                  { text = opts.siteName
                    path = index
                    sitemap = Yes "1.0" }
              Link
                  { text = opts.archivesTitle
                    path = $"{opts.archivesRoot}.html"
                    sitemap = Yes "0.9" }
              Link
                  { text = opts.tagsTitle
                    path = $"{opts.tagsRoot}.html"
                    sitemap = Yes "0.9" } ]
            @ opts.additionalNavs
              @ [ Link
                      { text = "RSS"
                        path = feed
                        sitemap = No } ]

        let navbar, navSitemap = generateNavbar opts.pathRoot navs

        let devInjection, devScript =
            match opts.stage with
            | Development ->
                Some("/js/live-reload.js"), [ ("js/live-reload.js", $"{opts.dst}{opts.pathRoot}/js/live-reload.js") ]
            | Production -> None, []

        let site: FixedSiteContent =
            { lang = opts.lang
              navbar = navbar
              name = opts.siteName
              title = opts.siteName
              description = opts.description
              url = opts.siteUrl
              pathRoot = opts.pathRoot
              copyright = opts.copyright
              favicon = opts.favicon
              devInjection = devInjection }

        let renderPostAndPages = renderMarkdowns site opts.tagsRoot
        let! metaPosts = renderPostAndPages $"{opts.src}{opts.postsRoot}" $"{opts.dst}{opts.pathRoot}{opts.postsRoot}"
        let! metaPages = renderPostAndPages $"{opts.src}{opts.pagesRoot}" $"{opts.dst}{opts.pathRoot}{opts.pagesRoot}"

        do! renderIndex site opts.tagsRoot metaPosts $"{opts.dst}{opts.pathRoot}{index}"

        let archiveDefs =
            [ Posts
                  { title = opts.postsTitle
                    metas = metaPosts
                    root = opts.postsRoot
                    priority = "0.8" }
              Pages
                  { title = opts.paesTitle
                    metas = metaPages
                    root = opts.pagesRoot
                    priority = "0.8" } ]

        let! archiveLocs = renderArchives site archiveDefs $"{opts.dst}{site.pathRoot}{opts.archivesRoot}.html"

        let tagDef =
            { title = opts.tagsTitle
              metas = Seq.concat [ metaPosts; metaPages ]
              tagRoot = opts.tagsRoot
              postRoot = opts.postsRoot
              priority = "0.9" }

        let! tagLocs = renderTags site tagDef $"{opts.dst}{site.pathRoot}{opts.tagsRoot}.html"
        do! render404 site $"{opts.dst}{opts.pathRoot}/404.html"

        do!
            renderSitemap
                site.url
                $"{opts.dst}{opts.pathRoot}/sitemap.xml"
                (Seq.concat [ navSitemap
                              tagLocs
                              archiveLocs ])


        do!
            renderFeed
                { title = opts.siteName
                  description = opts.description
                  link = $"{opts.siteUrl}{opts.pathRoot}"
                  feed = feed
                  postRoot = opts.postsRoot
                  posts = metaPosts }
                $"{opts.dst}{opts.pathRoot}{feed}"

        do!
            copyResources
            <| [ ($"{opts.src}{opts.favicon}", $"{opts.dst}{opts.pathRoot}{opts.favicon}") ]
               @ devScript

        printfn "Render complete!"
    }
    |> ignore

let dev =
    match List.ofSeq argv with
    | [ _; _; mode ] when mode = "dev" -> Development
    | _ -> Production

render
    { stage = dev
      lang = "ja"
      siteName = "Blog Title"
      description = "Blog Description"
      siteUrl = "https://krymtkts.github.io"
      pathRoot = "/blog-fable"
      copyright = "2023 krymtkts"
      favicon = "/img/favicon.ico"

      src = "contents"
      dst = "docs"

      postsRoot = "/posts"
      postsTitle = "Posts"
      pagesRoot = "/pages"
      paesTitle = "Pages"
      tagsRoot = "/tags"
      tagsTitle = "Tags"
      archivesRoot = "/archives"
      archivesTitle = "Archives"

      feed = "feed"

      additionalNavs =
          [ Link
                { text = "About Me"
                  path = "/pages/about.html"
                  sitemap = No } ]

    }
