module App

open StaticWebGenerator


let private render () =
    promise {
        let title = "Blog Title"
        let copyright = "2023 krymtkts"
        let navbar = generateNavbar title

        let renderPostAndPages = renderMarkdowns navbar title copyright
        let! metaPosts = renderPostAndPages "contents/posts" "docs/blog-fable/posts"
        let! metaPages = renderPostAndPages "contents/pages" "docs/blog-fable/pages"

        do! renderIndex navbar title copyright metaPosts "docs/blog-fable/index.html"
        do! renderArchives navbar title copyright metaPosts metaPages "docs/blog-fable/archives.html"
        let meta = Seq.concat [ metaPosts; metaPages ]
        do! renderTags navbar title copyright meta "docs/blog-fable/tags.html"
        do! render404 navbar title copyright "docs/blog-fable/404.html"

        do! copyResources [ ("contents/img/favicon.ico", "docs/blog-fable/img/favicon.ico") ]

        printfn "Render complete!"
    }
    |> ignore

render ()
