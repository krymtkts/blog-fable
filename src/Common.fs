module Common

open System
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

module String =

    let inline format (pattern: string) x =
        (^a: (member ToString: string -> string) (x, pattern))

    let inline truncate (length: int) x =
        match x with
        | x when String.length x <= length -> x
        | x -> x.[.. (length - 4)] + "..."

module DateTime =
    let toRFC822DateTimeString = DateTime.toRFC822DateTimeString
    let toRFC3339Date (d: DateTime) = d |> String.format "yyyy-MM-dd"

module private Util =
    open HighlightJs
    open Marked

    let mdToHtml s = Regex.Replace(s, @"\.md\b", ".html")

    let private me: ResizeArray<Marked.MarkedExtension> =
        let markedHighlight: obj -> Marked.MarkedExtension = importMember "marked-highlight"
        let markedFootnote: obj -> Marked.MarkedExtension = importDefault "marked-footnote"

        let renderer =
            let heading (item: Marked.Tokens.Heading) =
                let text = marked.Parser.parseInline item.tokens

                let escapedText = Regex.Replace(text, @"[^\w]+", "-")
                let l = (int) item.depth + 1
                let meta = if l = 2 then " data-pagefind-meta=\"title\" " else ""

                $"""<h%d{l} %s{meta}><a name="%s{escapedText}" href="#%s{escapedText}">%s{text}</a></h%d{l}>"""

            let link (item: Marked.Tokens.Link) =
                let ref =
                    match item.href with
                    | null -> ""
                    | s when s.StartsWith("http") -> s
                    | s -> mdToHtml s

                let text = marked.Parser.parseInline item.tokens

                let title =
                    match item.title with
                    | null -> text
                    | x -> x

                $"""<a href="%s{ref}" title="%s{title}">%s{text}</a>"""

            let listitem (item: Marked.Tokens.ListItem) =
                let checkState =
                    match item.``checked`` with
                    | None
                    | Some false -> ""
                    | _ -> "checked"

                // NOTE: should use parse because listitem includes block level tokens.
                let text: string = marked.Parser.parse <| U2.Case1 item.tokens

                match item.task with
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

            let image (item: Marked.Tokens.Image) =
                // NOTE: add lazy loading attribute.
                let title =
                    match item.title with
                    | "" -> item.text
                    | x -> x

                $"""<img src="%s{item.href}" title="%s{title}" alt="%s{item.text}" loading="lazy" />"""

            let mops =
                !!{| heading = heading
                     link = link
                     listitem = listitem
                     checkbox = checkbox
                     image = image |}


            jsOptions<Marked.MarkedExtension> (fun o ->
                o.useNewRenderer <- Some true
                o.renderer <- Some <| U2.Case1 mops
                o.gfm <- Some true)

        let highlighter =
            let highlight (code: string) (lang: string) =
                let code =
                    (hljs.highlight code !!{| language = lang |} false)
                        .value

                Regex.Replace(
                    code,
                    """[\n\t]""",
                    (fun s ->
                        match s.Value with
                        | "\n" -> "<br />"
                        | "\t" -> "&emsp;"
                        | x -> x)
                )

            markedHighlight !!{| highlight = highlight |}

        let footNote = markedFootnote !!{| description = "<hr />" |}

        let mes = [ renderer; highlighter; footNote ]
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

        let content =
            Html.div [ prop.className "section"
                       prop.dangerouslySetInnerHTML (parseMarkdown content) ]

        frontMatter, content

    /// Parses a React element invoking ReactDOMServer.renderToString
    let parseReact el = ReactDOMServer.renderToString el

    /// Parses a React element invoking ReactDOMServer.renderToStaticMarkup
    let parseReactStaticMarkup el = ReactDOMServer.renderToStaticMarkup el

    let parseReactStaticHtml el =
        @"<!DOCTYPE html>"
        + ReactDOMServer.renderToStaticMarkup el

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

    let isInvalidMarkdownFilenamePattern (s: string) =
        Regex.IsMatch(s, @"^[a-zA-Z0-9-.\s]+\.md$") |> not

    let isInvalidPostsFilenamePattern (s: string) =
        Regex.IsMatch(s, @"^\d{4}-\d{2}-\d{2}-[a-zA-Z0-9-.\s]+\.md$")
        |> not

    type Meta =
        { frontMatter: Parser.FrontMatter option
          content: ReactElement
          description: string
          layout: Layout
          source: string
          leaf: string
          date: string
          pubDate: string option
          publish: bool
          index: bool }

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

    let getImagePathPairs src dest =
        promise {
            let! paths = IO.getFiles src

            return
                paths
                |> List.map (fun path ->
                    let src = Directory.join2 src path
                    let dest = Directory.join2 dest path

                    src, dest)
        }

    let sourceToSitemap root source =
        let leaf: string = Directory.leaf source
        let path = Directory.join3 "/" root <| Util.mdToHtml leaf
        path.Replace("\\", "/")

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

    let summarizeHtml (length: int) (s: string) =
        Regex.Replace(s, """(<[^>]+>)""", "")
        |> fun s -> Regex.Replace(s, "\s+", " ")
        |> String.truncate length

    let leafHtml source = source |> IO.leaf |> Util.mdToHtml

module Xml =
    open Fable.SimpleXml.Generator

    type SiteLocation =
        { loc: string
          lastmod: string
          priority: string }

    let createSitemap (root: string) (locs: SiteLocation seq) =
        let urls =
            locs
            |> Seq.map (fun loc ->
                node
                    "url"
                    []
                    [ node "loc" [] [ text $"{root}{loc.loc}" ]
                      node "lastmod" [] [ text loc.lastmod ]
                      //   node "changefreq" [] [ text "monthly" ]
                      node "priority" [] [ text loc.priority ] ])
            |> List.ofSeq

        let urlSet =
            node
                "urlset"
                [ attr.value ("xmlns", "http://www.sitemaps.org/schemas/sitemap/0.9")
                  attr.value ("xmlns:xsi", "http://www.w3.org/2001/XMLSchema-instance") ]
                urls

        urlSet
        |> serializeXml
        |> (+) @"<?xml version=""1.0"" encoding=""UTF-8""?>"

    type RssItem =
        { guid: string
          link: string
          title: string
          description: string
          pubDate: string }

    let metaToRssItem (timeZone: string) (pathRoot: string) (meta: Meta) =
        let link = $"{pathRoot}/{meta.leaf}"

        let pubDate =
            match meta.frontMatter with
            | Some fm ->
                match fm.date with
                | Some d -> d
                | None -> meta.date
            | None -> meta.date
            |> DateTime.parseToRFC822DateTimeString timeZone

        { guid = link
          link = link
          title =
            match meta.frontMatter with
            | Some fm -> fm.title
            | None -> meta.leaf
          description =
            meta.content
            |> Parser.parseReactStaticMarkup
            |> simpleEscape
          pubDate = pubDate }

    type RssChannel =
        { title: string
          description: string
          link: string
          xml: string
          lastBuildDate: string
          generator: string }

    let createRss (channel: RssChannel) (items: RssItem seq) =
        let itemNodes =
            items
            |> Seq.map (fun item ->
                node
                    "item"
                    []
                    [ node "guid" [] [ text item.guid ]
                      node "link" [] [ text item.link ]
                      node "title" [] [ text item.title ]
                      node "description" [] [ text item.description ]
                      node "pubDate" [] [ text item.pubDate ] ])
            |> List.ofSeq

        node
            "rss"
            [ attr.value ("version", "2.0")
              attr.value ("xmlns:atom", "http://www.w3.org/2005/Atom") ]
            [ node "channel" []
              <| [ node
                       "atom:link"
                       [ attr.value ("href", $"{channel.link}{channel.xml}")
                         attr.value ("rel", "self")
                         attr.value ("type", "application/rss+xml") ]
                       []
                   node "title" [] [ text channel.title ]

                   node "description" [] [ text channel.description ]
                   node "link" [] [ text channel.link ]
                   node "lastBuildDate" [] [ text channel.lastBuildDate ]
                   node "generator" [] [ text channel.generator ] ]
                 @ itemNodes ]
        |> serializeXml
        |> (+) @"<?xml version=""1.0"" encoding=""UTF-8""?>"

[<AutoOpen>]
module Component =

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

    let tagToLi root tag count =
        let leaf = Directory.leaf $"{tag}.html"
        let title = Regex.Replace(leaf, "\.(md|html)", "")
        let ref = Directory.join3 "/" root <| Util.mdToHtml leaf

        liA ref <| Text $"{title} ({count})"

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

        liA ref <| Text title

    let header (tagRoot: string) (pubDate: string option) (fm: Parser.FrontMatter option) =
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
                  Html.div [ prop.className [ "tags" ]
                             prop.children (
                                 match fm.tags with
                                 | Some tags -> tags
                                 | None -> [||]
                                 |> Seq.map (fun tag ->
                                     Html.a [ prop.href $"%s{tagRoot}%s{tag}.html"
                                              prop.title tag
                                              prop.className "tag is-medium"
                                              prop.text tag ])
                             ) ] ]
            | None -> []

        header

    type FrameConfiguration =
        { lang: string
          navItems: ReactElement list
          name: string
          title: string
          description: string
          url: string
          copyright: string
          favicon: string
          style: string
          highlightStyle: string
          pagefindStyle: string
          pagefindScript: string
          scriptInjection: string list }

    let frame (conf: FrameConfiguration) (content: Fable.React.ReactElement list) =
        let themeSelector =
            [ Html.li [ prop.children [ Html.button [ prop.className "theme-toggle"
                                                      prop.custom ("data-theme", "light")
                                                      prop.text "🌞"
                                                      prop.title "Light theme" ]
                                        Html.button [ prop.className "theme-toggle"
                                                      prop.custom ("data-theme", "dark")
                                                      prop.text "🌙"
                                                      prop.title "Dark theme" ]
                                        Html.button [ prop.className "theme-toggle"
                                                      prop.custom ("data-theme", "system")
                                                      prop.text "🖥️"
                                                      prop.title "System Default" ] ] ] ]

        let navbar = Html.ul [ prop.children (conf.navItems @ themeSelector) ]

        let main =
            [ Html.head [ Html.title [ prop.text conf.title ]
                          Html.meta [ prop.charset "utf-8" ]
                          Html.meta [ prop.name "description"
                                      prop.content conf.description ]
                          Html.meta [ prop.name "viewport"
                                      prop.content "width=device-width, initial-scale=1" ]
                          Html.meta [ prop.custom ("property", "og:site_name")
                                      prop.content conf.name ]
                          Html.meta [ prop.custom ("property", "og:title")
                                      prop.content conf.title ]
                          Html.meta [ prop.custom ("property", "og:description")
                                      prop.content conf.description ]
                          Html.meta [ prop.custom ("property", "og:url")
                                      prop.content conf.url ]
                          Html.link [ prop.rel "canonical"
                                      prop.href conf.url ]
                          Html.link [ prop.rel "icon"
                                      prop.href conf.favicon ]
                          Html.link [ prop.rel "stylesheet"
                                      prop.href conf.pagefindStyle ]
                          Html.script [ prop.src conf.pagefindScript ]
                          Html.link [ prop.rel "stylesheet"
                                      prop.type' "text/css"
                                      prop.href conf.style ]
                          Html.link [ prop.rel "stylesheet"
                                      prop.type' "text/css"
                                      prop.href conf.highlightStyle ] ]
              Html.body [ Html.nav [ prop.className "tabs"
                                     prop.children [ navbar ] ]
                          Html.main [ prop.className "container"
                                      prop.children [ Html.div [ prop.className "content"
                                                                 prop.children content ] ] ] ]
              Html.footer [ prop.className "footer"
                            prop.children [ Html.div [ prop.className "container"
                                                       prop.text ($"Copyright © %s{conf.copyright}") ] ] ] ]

        let scripts =
            conf.scriptInjection
            |> List.map (fun src ->
                Html.script [ prop.lang "javascript"
                              prop.type' "text/javascript"
                              prop.src src ])

        Html.html [ prop.lang conf.lang
                    prop.children (scripts @ main) ]

    type FooterButton =
        | Prev
        | Next

    let footer (postRoot: string) (prev: Meta option) (next: Meta option) =

        let button button meta =
            match meta with
            | Some meta ->
                let ref = $"%s{postRoot}%s{meta.leaf}"

                let text, className =
                    let t =
                        match meta.frontMatter with
                        | Some fm -> $"%s{meta.date} %s{fm.title}"
                        | None -> $"%s{meta.date} %s{meta.leaf}"

                    match button with
                    | Prev -> $"<< %s{t}", "prev"
                    | Next -> $"%s{t} >>", "next"

                Html.a [ prop.classes [ className; "button" ]
                         prop.href ref
                         prop.title text
                         prop.children [ Html.span [ prop.text text ] ] ]
            | None -> null

        let prev = button Prev prev
        let next = button Next next

        [ Html.div [ prop.className "buttons"
                     prop.children [ prev; next ] ] ]
