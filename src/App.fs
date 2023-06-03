module App

open StaticWebGenerator


let private render () =
    promise {
        let title = "Blog Title"
        let navbar = generateNavbar title

        let! metaPosts = renderMarkdowns navbar title "contents/posts" "docs/posts"
        let! metaPages = renderMarkdowns navbar title "contents/pages" "docs/pages"

        do! renderIndex navbar title metaPosts
        do! renderArchives navbar title metaPosts metaPages "docs/archives.html"
        let meta = Seq.concat [ metaPosts; metaPages ]
        do! renderTags navbar title meta "docs/tags.html"
        do! render404 navbar title "docs/404.html"

        do! copyResources [ ("contents/img/favicon.ico", "docs/img/favicon.ico") ]

        printfn "Render complete!"
    }
    |> ignore

render ()
