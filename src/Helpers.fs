module StaticWebGenerator

open System.Text.RegularExpressions
open Fable.Core.JsInterop
open Fable.Core
open Feliz
open Node
open Marked
open HighlightJs

module IO =
    let resolve (path: string) = File.absolutePath path
    let writeFile = File.write
    let readFile = File.read
    let copy = File.copy
    let getFiles = Directory.getFiles true

module private Util =

    let mdToHtml s = Regex.Replace(s, @"\.md\b", ".html")

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
                        | Some s -> mdToHtml s
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

    let liA ref title =
        Html.li [ Html.a [ prop.href ref
                           prop.title title
                           prop.text title ] ]

module Parser =
    let parseMarkdownFile path =
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

let frame (navbar: Fable.React.ReactElement) (titleText: string) content =
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
                Html.body [ Html.nav [ navbar ]
                            Html.div [ prop.children [ content ] ] ] ]

let getDistPath (source: string) (dir: string) =
    Directory.leaf source
    |> Util.mdToHtml
    |> Directory.join2 dir
    |> IO.resolve

let isMarkdwon (path: string) = path.EndsWith ".md"

let getMarkdownFiles dir =
    promise {
        let! paths = IO.getFiles dir

        let files =
            paths
            |> List.filter isMarkdwon
            |> List.map (Directory.join2 dir)
            |> List.map IO.resolve

        return files
    }

let getLatestPost paths =
    paths |> List.sortBy Directory.leaf |> Seq.last

let pathToLi group source =
    let leaf = Directory.leaf source
    let title = leaf.Replace(".md", "")
    let ref = Directory.join3 "/" group <| Util.mdToHtml leaf

    Util.liA ref title

let generatePostArchives sourceDir group =
    promise {
        let! files =
            getMarkdownFiles
            <| Directory.join2 sourceDir group

        let archives =
            files
            |> List.filter isMarkdwon
            |> List.sortBy Directory.leaf
            |> List.rev
            |> List.groupBy (fun path ->
                let leaf = Directory.leaf path
                leaf.Substring(0, 7))
            |> List.map (fun (yearMonth, paths) ->
                let lis = paths |> List.map (pathToLi group)

                [ Html.li [ Html.h3 yearMonth ]
                  Html.ul lis ])

        return Html.ul [ prop.children (List.concat archives) ]
    }

let generatePageArchives sourceDir group =
    promise {
        let! files =
            getMarkdownFiles
            <| Directory.join2 sourceDir group

        let archives =
            files
            |> List.filter isMarkdwon
            |> List.sortBy Directory.leaf
            |> List.map (pathToLi group)

        return Html.ul [ prop.children archives ]
    }

let generateArchives sourceDir =
    promise {
        let! posts = generatePostArchives sourceDir "posts"
        let! pages = generatePageArchives sourceDir "pages"

        return
            Html.div [ prop.className [ "content" ]
                       prop.children [ Html.ul [ prop.children [ Html.li [ Html.h2 "Posts" ]
                                                                 posts
                                                                 Html.li [ Html.h2 "Pages" ]
                                                                 pages ] ] ] ]
    }

let generateNavbar =
    Html.ul [ Html.h1 [ prop.text "Blog Title" ]
              Util.liA "/index.html" "Index"
              Util.liA "/archives.html" "Archives"
              Util.liA "/pages/about.html" "About Me"
              Util.liA "/atom.xml" "RSS"
              Util.liA "/tags.html" "Tags" ]
