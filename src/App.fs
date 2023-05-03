module App

open StaticWebGenerator

let private markdownPath = IO.resolve "README.md"
let private indexPath = IO.resolve "docs/index.html"

let private render () =
    promise {
        let! el =
            IO.readFile markdownPath
            |> Promise.map (fun m -> parseMarkdownAsReactEl "content" m)

        let content = frame "Fable" el |> parseReactStatic
        do! IO.writeFile indexPath content
        do! IO.copy "static/fable.ico" "docs/fable.ico"
    }
    |> ignore

    printfn "Render complete!"

render ()
