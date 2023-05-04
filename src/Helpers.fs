module StaticWebGenerator

open System.Text.RegularExpressions
open Fable.Core.JsInterop
open Fable.Core
open Feliz
open Node

module private Util =
    let private highlight: obj = importAll "highlight.js"
    let private marked: obj = importAll "marked"

    marked?setOptions (createObj [ "highlight"
                                   ==> fun code lang -> highlight?highlightAuto (code, [| lang |])?value ])

    let private renderer =
        let renderer = createNew marked?Renderer ()
        // NOTE: heading and link are not work with let bindings.
        renderer?heading <- fun text level ->
                                let escapedText = Regex.Replace(string text, @"[^\w]+", "-")

                                sprintf
                                    """<h%s><a name="%s" class="anchor" href="#%s">%s</a></h%s>"""
                                    level
                                    escapedText
                                    escapedText
                                    text
                                    level

        renderer?link <- fun href title text ->
                             sprintf """<a href="%s">%s</a>""" (Regex.Replace(href, @"\.md\b", ".html")) text

        renderer

    let parseMarkdown (content: string) : string =
        marked?parse
        $ (content, createObj [ "renderer" ==> renderer ])

module Parser =
    let parseMarkdownFile path =
        printfn "path: %s" path

        fs.readFileSync(path).ToString()
        |> Util.parseMarkdown

    /// Parses a markdown string
    let parseMarkdown str = Util.parseMarkdown str

    let parseMarkdownAsReactEl className content =
        Html.div [ prop.className [ className ]
                   prop.dangerouslySetInnerHTML (parseMarkdown content) ]

    /// Parses a React element invoking ReactDOMServer.renderToString
    let parseReact el = ReactDOMServer.renderToString el

    /// Parses a React element invoking ReactDOMServer.renderToStaticMarkup
    let parseReactStatic el = ReactDOMServer.renderToStaticMarkup el

let frame titleText content =
    let cssLink path =
        Html.link [ prop.rel "stylesheet"
                    prop.type' "text/css"
                    prop.href path ]

    Html.html [ Html.head [ yield Html.title [ prop.title <| string titleText ]
                            yield
                                Html.meta [ prop.custom ("httpEquiv", "Content-Type")
                                            prop.content "text/html; charset=utf-8" ]
                            yield
                                Html.meta [ prop.name "viewport"
                                            prop.content "width=device-width, initial-scale=1" ]
                            yield cssLink "https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css"
                            yield cssLink "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.5.1/css/bulma.min.css" ]
                Html.body [ Html.div [ prop.children [ content ] ] ] ]

module IO =
    let inline resolve (path: string) = File.absolutePath path
    let writeFile = File.write
    let readFile = File.read
    let copy = File.copy
    let getFiles = Directory.getFiles true
