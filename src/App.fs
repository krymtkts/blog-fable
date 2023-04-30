module App

open StaticWebGenerator

let private markdownPath = IO.resolve "../README.md"
let private indexPath = IO.resolve "../docs/index.html"

let private render () =
    let content =
        IO.readFile markdownPath
        |> parseMarkdownAsReactEl "content"

    frame "Fable" content
    |> parseReactStatic
    |> IO.writeFile indexPath

    printfn "Render complete!"

IO.copy "static/fable.ico" "docs/fable.ico"
render ()
