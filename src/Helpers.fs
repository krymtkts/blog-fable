module StaticWebGenerator

open System.Text.RegularExpressions
open Fable.Core.JsInterop
open Fable.Core
open Feliz

module Node = Node.Api

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

/// Parses a markdown file
let parseMarkdownFile path =
    Node.fs.readFileSync(path).ToString()
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
    type private IJsLib =
        abstract getDirName: unit -> string

    [<ImportAll("../js/path.js")>]
    let private jsLib: IJsLib = jsNative

    let private fsExtra: obj = importDefault "fs-extra"

    let rec private ensureDirExists (dir: string) (cont: (unit -> unit) option) =
        if Node.fs.existsSync (!^dir) then
            match cont with
            | Some c -> c ()
            | None -> ()
        else
            ensureDirExists
                (Node.path.dirname dir)
                (Some (fun () ->
                    if not (Node.fs.existsSync !^dir) then
                        Node.fs.mkdirSync dir |> ignore

                    match cont with
                    | Some c -> c ()
                    | None -> ()))

    let inline resolve (path: string) =
        Node.path.resolve (jsLib.getDirName (), path)

    let writeFile (path: string) (content: string) =
        ensureDirExists (Node.path.dirname path) None
        Node.fs.writeFileSync (path, content)

    let readFile (path: string) = Node.fs.readFileSync(path).ToString()

    let copy (source: string) (target: string) : unit =
        fsExtra?copySync (source, target, createObj [ "overwrite" ==> true ])
