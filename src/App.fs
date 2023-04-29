module App

open StaticWebGenerator

// let markdownPath = IO.resolve "${entryDir}/../README.md"
// let dataPath = IO.resolve "${entryDir}/../data/people.json"
let private indexPath = IO.resolve "../public/index.html"
// let indexPath =
//     "C:/Users/takatoshi/dev/github.com/krymtkts/blog-fable/public/index.html"

let private render () =
    // let content =
    //     IO.readFile markdownPath
    //     |> parseMarkdownAsReactEl "content"

    // let data = createTable ()

    """
    <!doctype html>
    <html>
    <head>
      <title>Fable</title>
      <meta http-equiv='Content-Type' content='text/html; charset=utf-8'>
      <meta name="viewport" content="width=device-width, initial-scale=1">
      <link rel="shortcut icon" href="fable.ico" />
    </head>
    <body>
        <p>Fable is running</p>
        <p>You can click on this button:</p>
        <button class="my-button">Click me</button>
        <script src="bundle.js"></script>
    </body>
    </html>
    """
    // |> parseReactStatic
    |> IO.writeFile indexPath

    printfn "Render complete!"

printfn "path> %s" indexPath

render ()
