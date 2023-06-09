module Common

open System.Text.RegularExpressions
open Fable.Core.JsInterop
open Fable.Core
open Feliz
open Node
open Marked
open HighlightJs
open Yaml

module IO =
    let resolve (path: string) = File.absolutePath path
    let writeFile = File.write
    let readFile = File.read
    let copy = File.copy
    let getFiles = Directory.getFiles true
    let leaf = Directory.leaf
    let parent = Directory.dirname

module private Util =

    let mdToHtml s = Regex.Replace(s, @"\.md\b", ".html")

    let private me: ResizeArray<Marked.MarkedExtension> =
        let markedHighlight: obj -> Marked.MarkedExtension = importMember "marked-highlight"

        let renderer =
            let heading =
                fun (text: string) (level: int) ->
                    let escapedText = Regex.Replace(string text, @"[^\w]+", "-")
                    let l = level.ToString()

                    $"""<h{l}><a name="{escapedText}" class="anchor" href="#{escapedText}">{text}</a></h{l}>"""

            let link =
                fun href title text ->
                    let ref =
                        match href with
                        | Some s -> mdToHtml s
                        | None -> ""

                    $"""<a href="{ref}">{text}</a>"""

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

module Component =
    let liAWithClass ref title classes =
        Html.li [ prop.classes classes
                  prop.children [ Html.a [ prop.href ref
                                           prop.title title
                                           prop.text title ] ] ]

    type NavItem =
        | Text of string
        | Element of string * Fable.React.ReactElement

    let liA ref (title: NavItem) =
        let children =
            function
            | Element (s, el) -> [ prop.title s; prop.children [ el ] ]
            | Text (s) -> [ prop.title s; prop.text s ]

        Html.li [ Html.a <| prop.href ref :: children title ]

    let liSpanA (span: string) ref title =
        Html.li [ Html.span [ prop.text span ]
                  Html.a [ prop.href ref
                           prop.title title
                           prop.text title ] ]

    let pathToLi root source =
        let leaf = Directory.leaf source
        let title = Regex.Replace(leaf, "\.(md|html)", "")
        let ref = Directory.join3 "/" root <| Util.mdToHtml leaf

        liA ref <| Text title

module Parser =
    type FrontMatter =
        abstract title: string
        abstract tags: string array option
        abstract date: string option

    let private pattern =
        Regex(@"^---\s*\n(?<frontMatter>[\s\S]*?)\n?---\s*\n?(?<content>[\s\S]*)")

    let private extractFrontMatter (str: string) =
        match pattern.IsMatch str with
        | true ->
            let matches = pattern.Match str
            let f: FrontMatter = Yaml.parse matches.Groups.["frontMatter"].Value
            Some(f), matches.Groups.["content"].Value
        | _ -> None, str

    /// Parses a markdown string
    let parseMarkdown str = Util.parseMarkdown str

    let parseMarkdownAsReactEl (tagToElement: string -> ReactElement) content =
        let (frontMatter, content) = extractFrontMatter content

        let el =
            match frontMatter with
            | Some fm ->
                [ Html.h1 [ prop.className [ "title" ]
                            prop.text fm.title ]
                  Html.ul [ prop.className [ "tags" ]
                            prop.children (
                                match fm.tags with
                                | Some tags -> tags
                                | None -> [||]
                                |> Seq.map tagToElement
                            ) ]
                  Html.div [ prop.dangerouslySetInnerHTML (parseMarkdown content) ] ]
            | None -> [ Html.div [ prop.dangerouslySetInnerHTML (parseMarkdown content) ] ]

        frontMatter, el


    /// Parses a React element invoking ReactDOMServer.renderToString
    let parseReact el = ReactDOMServer.renderToString el

    /// Parses a React element invoking ReactDOMServer.renderToStaticMarkup
    let parseReactStatic el = ReactDOMServer.renderToStaticMarkup el

[<AutoOpen>]
module Misc =
    open System
    let argv = Process.argv

    type Layout =
        | Post of string
        | Page

    let discriminateLayout source =
        let leaf = Directory.leaf source

        match leaf.Split '-' |> List.ofArray with
        | year :: month :: day :: _ ->
            match [ year; month; day ]
                  |> List.map System.Int32.TryParse
                with
            | [ (true, year); (true, month); (true, day) ] ->
                let date = $"%04d{year}-%02d{month}-%02d{day}"
                Post(date)
            | _ -> Page
        | _ -> Page

    type Meta =
        { frontMatter: Parser.FrontMatter option
          content: string
          layout: Layout
          source: string
          leaf: string
          date: string }

    type FixedSiteContent =
        { navbar: ReactElement
          title: string
          copyright: string
          favicon: string
          devInjection: string option }

    let frame site (content: Fable.React.ReactElement list) =
        let cssLink path integrity =
            Html.link [ prop.rel "stylesheet"
                        prop.type' "text/css"
                        prop.href path
                        prop.integrity integrity
                        prop.crossOrigin.anonymous
                        prop.referrerPolicy.noReferrer ]

        Html.html [ Html.head [ Html.title [ prop.text site.title ]
                                Html.meta [ prop.custom ("httpEquiv", "Content-Type")
                                            prop.content "text/html; charset=utf-8" ]
                                Html.meta [ prop.name "viewport"
                                            prop.content "width=device-width, initial-scale=1" ]
                                Html.link [ prop.rel "icon"
                                            prop.href site.favicon ]
                                cssLink
                                    "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/fontawesome.min.css"
                                    "sha512-SgaqKKxJDQ/tAUAAXzvxZz33rmn7leYDYfBP+YoMRSENhf3zJyx3SBASt/OfeQwBHA1nxMis7mM3EV/oYT6Fdw=="
                                cssLink
                                    "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.9.4/css/bulma.min.css"
                                    "sha512-HqxHUkJM0SYcbvxUw5P60SzdOTy/QVwA1JJrvaXJv4q7lmbDZCmZaqz01UPOaQveoxfYRv1tHozWGPMcuTBuvQ=="
                                cssLink
                                    "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.8.0/styles/base16/solarized-dark.min.css"
                                    "sha512-kBHeOXtsKtA97/1O3ebZzWRIwiWEOmdrylPrOo3D2+pGhq1m+1CroSOVErIlsqn1xmYowKfQNVDhsczIzeLpmg==" ]
                    Html.body [ Html.nav [ prop.className "tabs"
                                           prop.children site.navbar ]
                                Html.main [ prop.className "container"
                                            prop.children [ Html.div [ prop.className "content"
                                                                       prop.children content ] ] ] ]
                    Html.footer [ prop.className "footer"
                                  prop.children [ Html.div [ prop.className "container"
                                                             prop.text ($"Copyright © {site.copyright}") ] ] ]
                    match site.devInjection with
                    | Some src ->
                        Html.script [ prop.lang "javascript"
                                      prop.type' "text/javascript"
                                      prop.src src ]
                    | None -> null ]

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
        paths |> Seq.sortBy Directory.leaf |> Seq.last

    let sourceToSitemap root source =
        let leaf: string = Directory.leaf source
        let path = Directory.join3 "/" root <| Util.mdToHtml leaf
        path.Replace("\\", "/")

    let metaToLi root meta =
        let leaf = Directory.leaf meta.source

        let prefix =
            match meta.layout with
            | Post (date) -> $"{date} - "
            | _ -> ""

        let title =
            match meta.frontMatter with
            | Some fm -> $"{prefix}{fm.title}"
            | None -> leaf

        let ref = Directory.join3 "/" root <| Util.mdToHtml leaf

        Component.liA ref <| Component.Text title

    let now = DateTime.Now

    let simpleEscape (s: string) =
        Regex.Replace(
            s,
            """[&<>'"]""",
            (fun s ->
                match s.Value with
                | "&" -> "&amp;"
                | "<" -> "&lt;"
                | ">" -> "&gt;"
                | "'" -> "&apos;"
                | "\"" -> "&quot;"
                | x -> x)
        )
