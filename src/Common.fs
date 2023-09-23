module Common

open System.Text.RegularExpressions
open Fable.Core
open Fable.Core.JsInterop
open Feliz
open Node

module IO =
    let resolve (path: string) = File.absolutePath path
    let writeFile = File.write
    let readFile = File.read
    let copy = File.copy
    let getFiles = Directory.getFiles true
    let leaf = Directory.leaf
    let parent = Directory.dirname

module private Util =
    open HighlightJs
    open Marked

    let mdToHtml s = Regex.Replace(s, @"\.md\b", ".html")

    let private me: ResizeArray<Marked.MarkedExtension> =
        let markedHighlight: obj -> Marked.MarkedExtension = importMember "marked-highlight"

        let renderer =
            let heading text level =
                let escapedText = Regex.Replace(text, @"[^\w]+", "-")
                let l = level

                $"""<h%d{l}><a name="%s{escapedText}" href="#%s{escapedText}">%s{text}</a></h%d{l}>"""

            let link href title text =
                let ref =
                    match href with
                    | Some s -> mdToHtml s
                    | None -> ""

                let title =
                    match title with
                    | null -> text
                    | _ -> title

                $"""<a href="%s{ref}" title="%s{title}">%s{text}</a>"""

            let listitem (text: string) task check =
                let checkState =
                    match check with
                    | true -> "checked"
                    | false -> ""

                match task with
                | true ->
                    let str, rst =
                        match text.IndexOf("<") with
                        | -1 -> text, ""
                        | i -> text.Substring(0, i), text.Substring(i)

                    $"""<li><label class="checkbox"><input type="checkbox" class="checkbox" disabled %s{checkState} />%s{str}</label>%s{rst}</li>"""
                | false -> $"""<li>%s{text}</li>"""

            let checkbox _ =
                // NOTE: checkbox generation is handled by listitem.
                ""

            let mops =
                !!{| heading = heading
                     link = link
                     listitem = listitem
                     checkbox = checkbox |}


            jsOptions<Marked.MarkedExtension> (fun o ->
                o.renderer <- Some <| U2.Case2 mops
                o.gfm <- Some true
                o.headerIds <- Some true)

        let highlighter =
            let highlight (code: string) (lang: string) =
                (hljs.highlight code !!{| language = lang |} false)
                    .value

            markedHighlight !!{| highlight = highlight |}

        let mes = [ renderer; highlighter ]
        ResizeArray mes

    marked.``use`` me

    let parseMarkdown (content: string) : string = marked.parse $ (content)


module Parser =
    open Yaml

    type FrontMatter =
        abstract title: string
        abstract tags: string array option
        abstract date: string option

    let private matchFrontMatter s =
        Regex.Match(s, @"^---\s*\n(?<frontMatter>[\s\S]*?)\n?---\s*\n?(?<content>[\s\S]*)")

    let (|Empty|Matched|) (xs: Match) =
        match xs.Success with
        | false -> Empty
        | _ -> Matched xs

    let private extractFrontMatter (str: string) =
        matchFrontMatter str
        |> function
            | Empty -> None, str
            | Matched matches ->
                let f: FrontMatter = Yaml.parse matches.Groups.["frontMatter"].Value
                Some(f), matches.Groups.["content"].Value

    /// Parses a markdown string
    let parseMarkdown str = Util.parseMarkdown str

    let parseMarkdownAsReactEl content =
        let (frontMatter, content) = extractFrontMatter content
        let content = Html.div [ prop.dangerouslySetInnerHTML (parseMarkdown content) ]

        frontMatter, content

    /// Parses a React element invoking ReactDOMServer.renderToString
    let parseReact el = ReactDOMServer.renderToString el

    /// Parses a React element invoking ReactDOMServer.renderToStaticMarkup
    let parseReactStaticMarkup el = ReactDOMServer.renderToStaticMarkup el

    let parseReactStaticHtml el =
        @"<!DOCTYPE html>"
        + ReactDOMServer.renderToStaticMarkup el

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

    let header (tagToElement: string -> ReactElement) pubDate (fm: Parser.FrontMatter option) =
        let date pubDate fmDate =
            let date =
                match pubDate, fmDate with
                | Some pub, Some upd -> $"%s{pub} - Last updated on %s{upd}"
                | Some pub, _ -> pub
                | _, Some pub -> pub
                | _ -> null

            Html.div [ prop.className "date"
                       prop.text date ]

        let header =
            match fm with
            | Some fm ->
                [ date pubDate fm.date
                  Html.h1 [ prop.className [ "title" ]
                            prop.text fm.title ]
                  Html.ul [ prop.className [ "tags" ]
                            prop.children (
                                match fm.tags with
                                | Some tags -> tags
                                | None -> [||]
                                |> Seq.map tagToElement
                            ) ] ]
            | None -> []

        header

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
            match [ year; month; day ] |> List.map Int32.TryParse with
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
        { lang: string
          navbar: ReactElement
          name: string
          title: string
          description: string
          pathRoot: string
          url: string
          copyright: string
          favicon: string
          style: string
          devInjection: string option }

    let wrapContent (elm: Fable.React.ReactElement list) =
        Html.div [ prop.className "content"
                   prop.children elm ]

    let frame site (content: Fable.React.ReactElement) =
        let cssLink path integrity =
            Html.link [ prop.rel "stylesheet"
                        prop.type' "text/css"
                        prop.href path
                        prop.integrity integrity
                        prop.crossOrigin.anonymous
                        prop.referrerPolicy.noReferrer ]

        Html.html [ prop.lang site.lang
                    prop.children [ Html.head [ Html.title [ prop.text site.title ]
                                                Html.meta [ prop.charset "utf-8" ]
                                                Html.meta [ prop.name "description"
                                                            prop.content site.description ]
                                                Html.meta [ prop.name "viewport"
                                                            prop.content "width=device-width, initial-scale=1" ]
                                                Html.meta [ prop.custom ("property", "og:site_name")
                                                            prop.content site.name ]
                                                Html.meta [ prop.custom ("property", "og:title")
                                                            prop.content site.title ]
                                                Html.meta [ prop.custom ("property", "og:description")
                                                            prop.content site.description ]
                                                Html.meta [ prop.custom ("property", "og:url")
                                                            prop.content site.url ]
                                                Html.link [ prop.rel "canonical"
                                                            prop.href site.url ]
                                                Html.link [ prop.rel "icon"
                                                            prop.href site.favicon ]
                                                Html.link [ prop.rel "stylesheet"
                                                            prop.type' "text/css"
                                                            prop.href $"%s{site.pathRoot}%s{site.style}" ]
                                                cssLink
                                                    "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.8.0/styles/base16/solarized-dark.min.css"
                                                    "sha512-kBHeOXtsKtA97/1O3ebZzWRIwiWEOmdrylPrOo3D2+pGhq1m+1CroSOVErIlsqn1xmYowKfQNVDhsczIzeLpmg==" ]
                                    Html.body [ Html.nav [ prop.className "tabs"
                                                           prop.children site.navbar ]
                                                Html.main [ prop.className "container"
                                                            prop.children [ content ] ] ]
                                    Html.footer [ prop.className "footer"
                                                  prop.children [ Html.div [ prop.className "container"
                                                                             prop.text (
                                                                                 $"Copyright © %s{site.copyright}"
                                                                             ) ] ] ]
                                    match site.devInjection with
                                    | Some src ->
                                        Html.script [ prop.lang "javascript"
                                                      prop.type' "text/javascript"
                                                      prop.src $"%s{site.pathRoot}%s{src}" ]
                                    | None -> null ] ]

    let getDestinationPath (source: string) (dir: string) =
        Directory.leaf source
        |> Util.mdToHtml
        |> Directory.join2 dir
        |> IO.resolve

    let isMarkdown (path: string) = path.EndsWith ".md"

    let getMarkdownFiles dir =
        promise {
            let! paths = IO.getFiles dir

            let files =
                paths
                |> List.filter isMarkdown
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
            | Post (date) -> $"%s{date} - "
            | _ -> ""

        let title =
            match meta.frontMatter with
            | Some fm -> $"%s{prefix}%s{fm.title}"
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

module String =

    let inline format pattern x =
        (^a: (member ToString: string -> string) (x, pattern))

module DateTime =
    open System

    let options: obj =
        !!{| weekday = "short"
             year = "numeric"
             month = "short"
             day = "2-digit"
             hour = "numeric"
             minute = "numeric"
             second = "numeric"
             hourCycle = "h23"
             timeZone = "Asia/Tokyo" // TODO: parameterize it.
             timeZoneName = "short" |}

    // TODO: write binding.
    let formatter: obj = Intl.DateTimeFormat "en-US" options

    let toRFC822DateTimeString (d: DateTime) =
        let parts: obj [] = formatter?formatToParts (d)
        let p: string [] = parts |> Array.map (fun x -> x?value)
        let d = $"%s{p.[0]}%s{p.[1]}%s{p.[4]} %s{p.[2]} %s{p.[6]}"
        let t = (p.[8..12] |> String.concat "")

        let z =
            match p.[14] with
            | "UTC" -> "+0000"
            | z ->
                let item = Regex.Matches(z, @"GMT([+-])(\d+)")
                let group = item.Item 0
                let op = (group.Groups.Item 1).Value
                let offset = int (group.Groups.Item 2).Value

                $"%s{op}%02d{offset}00"

        $"%s{d} %s{t} %s{z}"

    let parseToRFC822DateTimeString (s: string) =
        DateTime.Parse(s) |> toRFC822DateTimeString

    let toRFC3339Date (d: DateTime) = d |> String.format "yyyy-MM-dd"
