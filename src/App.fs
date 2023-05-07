module App

open StaticWebGenerator

let private getMarkdownFiles dir =
    promise {
        let! paths = IO.getFiles dir

        let files =
            paths
            |> List.filter (fun p -> p.EndsWith ".md")
            |> List.map (sprintf "%s/%s" dir)
            |> List.map IO.resolve

        return files
    }

let getLeafPath (source: string) =
    source.Replace("\\", "/").Split("/") |> Seq.last

let getDistPath (source: string) (dir: string) =
    getLeafPath source
    |> fun x -> x.Replace(".md", ".html")
    |> sprintf "%s/%s" dir
    |> IO.resolve

let private readAndWrite source dist =
    promise {
        printfn "Rendering %s..." source
        let! m = IO.readFile source

        let content =
            m
            |> Parser.parseMarkdownAsReactEl "content"
            |> frame "Fable"
            |> Parser.parseReactStatic

        printfn "Writing %s..." dist

        do! IO.writeFile dist content
    }

let private renderPosts sourceDir distDir =
    promise {
        let! files = getMarkdownFiles sourceDir

        do!
            files
            |> List.sortBy getLeafPath
            |> Seq.last
            |> fun source ->
                let dist = IO.resolve "docs/index.html"
                promise { do! readAndWrite source dist }

        files
        |> List.map (fun source ->
            let dist = getDistPath source distDir
            promise { do! readAndWrite source dist })
        |> Promise.all
        |> ignore
    }

let private renderMarkdowns sourceDir distDir =
    promise {
        let! files = getMarkdownFiles sourceDir

        files
        |> List.map (fun source ->
            let dist = getDistPath source distDir
            promise { do! readAndWrite source dist })
        |> Promise.all
        |> ignore
    }

let private render () =
    promise {
        do! renderMarkdowns "contents/pages" "docs/pages"
        do! renderPosts "contents/posts" "docs/posts"
        do! IO.copy "contents/fable.ico" "docs/fable.ico"

        printfn "Render complete!"
    }
    |> ignore

render ()
