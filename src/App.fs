module App

open StaticWebGenerator


let private render () =
    promise {
        let title = "Blog Title"
        let copyright = "2023 krymtkts"
        let navbar = generateNavbar title

        let renderPostAndPages = renderMarkdowns navbar title copyright
        let! metaPosts = renderPostAndPages "contents/posts" "docs/posts"
        let! metaPages = renderPostAndPages "contents/pages" "docs/pages"

        do! renderIndex navbar title copyright metaPosts
        do! renderArchives navbar title copyright metaPosts metaPages "docs/archives.html"
        let meta = Seq.concat [ metaPosts; metaPages ]
        do! renderTags navbar title copyright meta "docs/tags.html"
        do! render404 navbar title copyright "docs/404.html"

        do! copyResources [ ("contents/img/favicon.ico", "docs/img/favicon.ico") ]

        printfn "Render complete!"
    }
    |> ignore

render ()
