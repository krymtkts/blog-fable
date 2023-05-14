module App

open StaticWebGenerator



let private readAndWrite navbar source dist =
    promise {
        printfn "Rendering %s..." source
        let! m = IO.readFile source

        let content =
            m
            |> Parser.parseMarkdownAsReactEl "content"
            |> frame navbar "Blog - Fable"
            |> Parser.parseReactStatic

        printfn "Writing %s..." dist

        do! IO.writeFile dist content
    }

let renderArchives navbar sourceDir dist =
    promise {
        printfn "Rendering archives..."
        let! archives = generateArchives sourceDir

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
                promise { do! rw source dist }

        files
        |> List.map (fun source ->
            let dist = getDistPath source distDir
            promise { do! rw source dist })
        |> Promise.all
        |> ignore
    }

let private renderMarkdowns sourceDir distDir navbar =
    promise {
        let! files = getMarkdownFiles sourceDir
        let rw = readAndWrite navbar

        files
        |> List.map (fun source ->
            let dist = getDistPath source distDir
            promise { do! rw source dist })
        |> Promise.all
        |> ignore
    }

let private render () =
    promise {
        let navbar = generateNavbar

        do! renderArchives navbar "contents" "docs/archives.html"
        do! renderPosts "contents/posts" "docs/posts" navbar
        do! renderMarkdowns "contents/pages" "docs/pages" navbar
        do! IO.copy "contents/fable.ico" "docs/fable.ico"

        printfn "Render complete!"
    }
    |> ignore

render ()
