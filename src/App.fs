module App

open StaticWebGenerator

let private readAndWrite navbar title source dist =
    promise {
        printfn "Rendering %s..." source
        let! m = IO.readFile source

        let frontMatter, content =
            m
            |> Parser.parseMarkdownAsReactEl "content"
            |> fun (fm, c) ->
                let title =
                    match fm with
                    | Some fm -> sprintf "%s - %s" title fm.title
                    | None -> title

                fm, frame navbar title c |> Parser.parseReactStatic


        printfn "Writing %s..." dist

        do! IO.writeFile dist content
        return frontMatter
    }

let private renderMarkdowns navbar title sourceDir distDir =
    promise {
        let! files = getMarkdownFiles sourceDir
        let rw = readAndWrite navbar title

        return!
            files
            |> List.map (fun source ->
                let dist = getDistPath source distDir

                promise {
                    let! fm = rw source dist

                    return
                        { frontMatter = fm
                          layout = discriminateLayout source
                          source = source
                          dist = dist }
                })
            |> Promise.all
    }

let private renderIndex navbar title metaPosts =
    let latest =
        metaPosts
        |> Seq.map (fun m -> m.source)
        |> getLatestPost

    promise {
        let rw = readAndWrite navbar title
        let dist = IO.resolve "docs/index.html"

        do! rw latest dist |> Promise.map ignore
    }

let renderArchives navbar title metaPosts metaPages dist =
    promise {
        printfn "Rendering archives..."
        let! archives = generateArchives metaPosts metaPages

        let content =
            archives
            |> frame navbar (sprintf "%s - Archives" title)
            |> Parser.parseReactStatic

        printfn "Writing archives %s..." dist

        do! IO.writeFile dist content
    }

let renderTags navbar title meta dist =
    let tagsContent, tagPageContents = generateTagsContent meta
    let frame = frame navbar

    promise {
        printfn "Rendering tags..."
        let title = (sprintf "%s - Tags" title)

        let content =
            tagsContent
            |> frame title
            |> Parser.parseReactStatic

        printfn "Writing tags %s..." dist

        do! IO.writeFile dist content

        return!
            tagPageContents
            |> List.map (fun (tag, tagPageContent) ->
                let dist =
                    IO.resolve (
                        sprintf "%s/%s.html"
                        <| dist.Replace(".html", "")
                        <| tag
                    )

                printfn "Writing tag %s..." dist

                let content =
                    tagPageContent
                    |> frame (sprintf "%s - %s" title tag)
                    |> Parser.parseReactStatic

                IO.writeFile dist content |> Promise.map ignore)
            |> Promise.all
            |> Promise.map ignore
    }

let render404 navbar title dist =
    promise {
        printfn "Rendering 404..."

        let content =
            generate404
            |> frame navbar (sprintf "%s - 404" title)
            |> Parser.parseReactStatic

        printfn "Writing 404 %s..." dist

        do! IO.writeFile dist content
    }

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

        do! IO.copy "contents/img/favicon.ico" "docs/img/favicon.ico"

        printfn "Render complete!"
    }
    |> ignore

render ()
