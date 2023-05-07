module StaticWebGenerator

open System.Text.RegularExpressions
open Fable.Core.JsInterop
open Fable.Core
open Feliz
open Node
open Marked
open HighlightJs

module private Util =
    let private me: ResizeArray<Marked.MarkedExtension> =
        let markedHighlight: obj -> Marked.MarkedExtension = importMember "marked-highlight"

        let renderer =
            let heading =
                fun (text: string) (level: int) ->
                    let escapedText = Regex.Replace(string text, @"[^\w]+", "-")
                    let l = level.ToString()

                    sprintf
                        """<h%s><a name="%s" class="anchor" href="#%s">%s</a></h%s>"""
                        l
                        escapedText
                        escapedText
                        text
                        l

            let link =
                fun href title text ->
                    let ref =
                        match href with
                        | Some s -> Regex.Replace(s, @"\.md\b", ".html")
                        | None -> ""

                    sprintf """<a href="%s">%s</a>""" ref text

            let mops = !!{| heading = heading; link = link |}


            jsOptions<Marked.MarkedExtension> (fun o ->
                o.renderer <- Some <| U2.Case2 mops
                o.gfm <- Some true
                o.headerIds <- Some true)

        let highlighter =
            let highlight =
                fun (code: string) (lang: string) ->
                    (hljs.highlight (code, !!{| language = lang |}))
                        .value

            markedHighlight !!{| highlight = highlight |}

        let mes = [ renderer; highlighter ]
        ResizeArray mes

    marked.``use`` me

    let parseMarkdown (content: string) : string = marked.parse $ (content)

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
    let resolve (path: string) = File.absolutePath path
    let writeFile = File.write
    let readFile = File.read
    let copy = File.copy
    let getFiles = Directory.getFiles true
