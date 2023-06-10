module App

open StaticWebGenerator


let private render () =
    promise {
        let title = "Blog Title"
        let copyright = "2023 krymtkts"

        let navi =
            [ Title
                  { text = title
                    path = "/blog-fable/index.html" }
              Link
                  { text = "Archives"
                    path = "/blog-fable/archives.html" }
              Link
                  { text = "Tags"
                    path = "/blog-fable/tags.html" }
              Link
                  { text = "About Me"
                    path = "/blog-fable/pages/about.html" }
              Link
                  { text = "RSS"
                    path = "/blog-fable/atom.xml" } ]

        let navbar = generateNavbar navi

        let site =
            { navbar = navbar
              title = title
              copyright = copyright }

        let renderPostAndPages = renderMarkdowns site "/blog-fable/tags"
        let! metaPosts = renderPostAndPages "contents/posts" "docs/blog-fable/posts"
        let! metaPages = renderPostAndPages "contents/pages" "docs/blog-fable/pages"

        do! renderIndex site "/blog-fable/tags" metaPosts "docs/blog-fable/index.html"

        let arcives =
            [ { title = "Posts"
                metas = metaPosts
                root = "/blog-fable/posts" }
              { title = "Pages"
                metas = metaPages
                root = "/blog-fable/pages" } ]

        do! renderArchives site arcives "docs/blog-fable/archives.html"
        let meta = Seq.concat [ metaPosts; metaPages ]
        do! renderTags site "/blog-fable/tags" meta "docs/blog-fable/tags.html"
        do! render404 site "docs/blog-fable/404.html"

        do! copyResources [ ("contents/img/favicon.ico", "docs/blog-fable/img/favicon.ico") ]

        printfn "Render complete!"
    }
    |> ignore

render ()
