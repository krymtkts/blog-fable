module App

open StaticWebGenerator

let private readAndWrite navbar source dist =
    promise {
        printfn "Rendering %s..." source
        let! m = IO.readFile source

        let frontMatter, content =
            m
            |> Parser.parseMarkdownAsReactEl "content"
            |> fun (fm, c) ->
                fm,
                frame navbar "Blog - Fable" c
                |> Parser.parseReactStatic


        printfn "Writing %s..." dist

        do! IO.writeFile dist content
        return frontMatter
    }

let renderTags navbar (meta: Meta seq) dist =
    let tagsContent, tagPageContents = generateTagsContent meta

    promise {
        printfn "Rendering tags..."

        let content =
            tagsContent
            |> frame navbar "Blog - Fable"
            |> Parser.parseReactStatic

        printfn "Writing tags %s..." dist

        do! IO.writeFile dist content

        return!
            tagPageContents
            |> List.map (fun (tag, tagPageContent) ->
                let dist = IO.resolve (sprintf "docs/tags/%s.html" tag)

                printfn "Writing tag %s..." dist

                let content =
                    tagPageContent
                    |> frame navbar "Blog - Fable"
                    |> Parser.parseReactStatic

                IO.writeFile dist content |> Promise.map ignore)
            |> Promise.all
            |> Promise.map ignore
    }

let renderArchives navbar (metaPosts: Meta seq) (metaPages: Meta seq) dist =
    promise {
        printfn "Rendering archives..."
        let! archives = generateArchives metaPosts metaPages

        let content =
            archives
            |> frame navbar "Blog - Fable"
            |> Parser.parseReactStatic

        printfn "Writing archives %s..." dist

        do! IO.writeFile dist content
    }

let private renderPosts sourceDir distDir navbar =
    promise {
        let! files = getMarkdownFiles sourceDir
        let rw = readAndWrite navbar

        do!
            getLatestPost files
            |> fun source ->
                let dist = IO.resolve "docs/index.html"

                rw source dist |> Promise.map ignore

        return!
            files
            |> List.map (fun source ->
                let dist = getDistPath source distDir

                promise {
                    let! fm = rw source dist

                    return
                        { frontMatter = fm
                          source = source
                          dist = dist }
                })
            |> Promise.all
    }

let private renderMarkdowns sourceDir distDir navbar =
    promise {
        let! files = getMarkdownFiles sourceDir
        let rw = readAndWrite navbar

        return!
            files
            |> List.map (fun source ->
                let dist = getDistPath source distDir

                promise {
                    let! fm = rw source dist

                    return
                        { frontMatter = fm
                          source = source
                          dist = dist }
                })
            |> Promise.all
    }

let private render () =
    promise {
        let navbar = generateNavbar

        let! metaPosts = renderPosts "contents/posts" "docs/posts" navbar
        let! metaPages = renderMarkdowns "contents/pages" "docs/pages" navbar
        let meta = Seq.concat [ metaPosts; metaPages ]

        do! renderArchives navbar metaPosts metaPages "docs/archives.html"
        do! renderTags navbar meta "docs/tags.html"
        do! IO.copy "contents/fable.ico" "docs/fable.ico"

        printfn "Render complete!"
    }
    |> ignore

render ()
