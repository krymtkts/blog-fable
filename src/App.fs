module App

open StaticWebGenerator

let private docsPath = IO.resolve "docs"


let private renderMarkdowns sourceDir distDir =
    promise {
        let! paths = IO.getFiles sourceDir

        paths
        |> List.filter (fun p -> p.EndsWith ".md")
        |> List.map (fun p ->
            let source = IO.resolve <| sprintf "%s/%s" sourceDir p
            printfn "Rendering %s..." source

            promise {
                let! m = IO.readFile source

                let content =
                    m
                    |> Parser.parseMarkdownAsReactEl "content"
                    |> frame "Fable"
                    |> Parser.parseReactStatic

                let filePath =
                    p.Replace("\\", "/").Split("/") // TODO: remove only date path.
                    |> Seq.last
                    |> fun x -> x.Replace(".md", ".html")

                let dist = IO.resolve <| sprintf "%s/%s" distDir filePath

                printfn "Writing %s..." dist

                do! IO.writeFile dist content
            })
        |> Promise.all
        |> Promise.map (fun _ -> ())
        |> ignore
    }

let private render () =
    promise {
        do! renderMarkdowns "contents/pages" "docs/pages"
        do! renderMarkdowns "contents/posts" "docs/posts"
        do! IO.copy "contents/fable.ico" "docs/fable.ico"

        printfn "Render complete!"
    }
    |> ignore

render ()
