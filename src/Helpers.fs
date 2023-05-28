module StaticWebGenerator

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

    let liAWithClass ref title classes =
        Html.li [ prop.classes classes
                  prop.children [ Html.a [ prop.href ref
                                           prop.title title
                                           prop.text title ] ] ]

    let liA ref title =
        Html.li [ Html.a [ prop.href ref
                           prop.title title
                           prop.text title ] ]

    let liSpanA (span: string) ref title =
        Html.li [ Html.span [ prop.text span ]
                  Html.a [ prop.href ref
                           prop.title title
                           prop.text title ] ]

module Parser =
    type FrontMatter =
        abstract title: string
        abstract tags: string array option

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

    let parseMarkdownAsReactEl className content =
        let (frontMatter, content) = extractFrontMatter content

        let tagLi tag =
            Util.liAWithClass (sprintf "/tags/%s.html" tag) tag [ "tag" ]

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
                                |> Seq.map tagLi
                            ) ]
                  Html.div [ prop.dangerouslySetInnerHTML (parseMarkdown content) ] ]
            | None -> [ Html.div [ prop.dangerouslySetInnerHTML (parseMarkdown content) ] ]

        frontMatter,
        Html.div [ prop.className [ className ]
                   prop.children el ]


    /// Parses a React element invoking ReactDOMServer.renderToString
    let parseReact el = ReactDOMServer.renderToString el

    /// Parses a React element invoking ReactDOMServer.renderToStaticMarkup
    let parseReactStatic el = ReactDOMServer.renderToStaticMarkup el

type Layout =
    | Post of string
    | Page

let discriminateLayout source =
    let leaf = Directory.leaf source

    match leaf.Split '-' |> List.ofArray with
    | year :: month :: day :: xs ->
        match [ year; month; day ]
              |> List.map System.Int32.TryParse
            with
        | [ (true, year); (true, month); (true, day) ] ->
            let date = sprintf "%04d-%02d-%02d" year month day
            Post(date)
        | _ -> Page
    | _ -> Page


type Meta =
    { frontMatter: Parser.FrontMatter option
      layout: Layout
      source: string
      dist: string }

let frame (navbar: Fable.React.ReactElement) (titleText: string) content =
    let cssLink path integrity =
        Html.link [ prop.rel "stylesheet"
                    prop.type' "text/css"
                    prop.href path
                    prop.integrity integrity
                    prop.crossOrigin.anonymous
                    prop.referrerPolicy.noReferrer ]

    Html.html [ Html.head [ yield Html.title [ prop.title <| string titleText ]
                            yield
                                Html.meta [ prop.custom ("httpEquiv", "Content-Type")
                                            prop.content "text/html; charset=utf-8" ]
                            yield
                                Html.meta [ prop.name "viewport"
                                            prop.content "width=device-width, initial-scale=1" ]
                            yield
                                cssLink
                                    "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/fontawesome.min.css"
                                    "sha512-SgaqKKxJDQ/tAUAAXzvxZz33rmn7leYDYfBP+YoMRSENhf3zJyx3SBASt/OfeQwBHA1nxMis7mM3EV/oYT6Fdw=="
                            yield
                                cssLink
                                    "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.9.4/css/bulma.min.css"
                                    "sha512-HqxHUkJM0SYcbvxUw5P60SzdOTy/QVwA1JJrvaXJv4q7lmbDZCmZaqz01UPOaQveoxfYRv1tHozWGPMcuTBuvQ=="
                            yield
                                cssLink
                                    "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.8.0/styles/base16/solarized-dark.min.css"
                                    "sha512-kBHeOXtsKtA97/1O3ebZzWRIwiWEOmdrylPrOo3D2+pGhq1m+1CroSOVErIlsqn1xmYowKfQNVDhsczIzeLpmg==" ]
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
    paths |> Seq.sortBy Directory.leaf |> Seq.last

let pathToLi group source =
    let leaf = Directory.leaf source
    let title = Regex.Replace(leaf, "\.(md|html)", "")
    let ref = Directory.join3 "/" group <| Util.mdToHtml leaf

    Util.liA ref title

let metaToLi group meta =
    let leaf = Directory.leaf meta.source

    let prefix =
        match meta.layout with
        | Post (date) -> sprintf "%s - " date
        | _ -> ""

    let title =
        match meta.frontMatter with
        | Some fm -> sprintf "%s%s" prefix fm.title
        | None -> leaf

    let ref = Directory.join3 "/" group <| Util.mdToHtml leaf

    Util.liA ref title

let generatePostArchives (meta: Meta seq) group =
    promise {
        let archives =
            meta
            |> Seq.sortBy (fun meta -> Directory.leaf meta.source)
            |> Seq.rev
            |> Seq.groupBy (fun meta ->
                let leaf = Directory.leaf meta.source
                leaf.Substring(0, 7))
            |> Seq.map (fun (yearMonth, metas) ->
                let lis = metas |> Seq.map (fun meta -> metaToLi group meta)

                [ Html.li [ Html.h3 yearMonth ]
                  Html.ul lis ])

        return Html.ul [ prop.children (List.concat archives) ]
    }

let generatePageArchives (meta: Meta seq) group =
    promise {
        let archives =
            meta
            |> Seq.sortBy (fun meta -> Directory.leaf meta.source)
            |> Seq.map (metaToLi group)

        return Html.ul [ prop.children archives ]
    }

let generateArchives (metaPosts: Meta seq) (metaPages: Meta seq) =
    promise {
        let! posts = generatePostArchives metaPosts "posts"
        let! pages = generatePageArchives metaPages "pages"

        return
            Html.div [ prop.className [ "content" ]
                       prop.children [ Html.ul [ prop.children [ Html.li [ Html.h2 "Posts" ]
                                                                 posts
                                                                 Html.li [ Html.h2 "Pages" ]
                                                                 pages ] ] ] ]
    }

let generateTagsContent (meta: Meta seq) =
    let tagAndPage =
        meta
        |> Seq.map (fun meta ->
            match meta.frontMatter with
            | Some fm ->
                match fm.tags with
                | Some tags -> tags |> Seq.map (fun t -> (t, meta))
                | None -> [||]
            | None -> [||])
        |> Seq.concat
        |> Seq.fold
            (fun acc (tag, meta) ->
                match Map.tryFind tag acc with
                | Some pages -> Map.add tag (meta :: pages) acc
                | None -> Map.add tag [ meta ] acc)
            Map.empty

    let tagsContent =
        let tags =
            tagAndPage
            |> Map.toList
            |> List.map (fun (tag, _) -> pathToLi "tags" <| sprintf "%s.html" tag)

        Html.div [ prop.className [ "content" ]
                   prop.children [ Html.ul [ prop.children [ Html.li [ Html.h2 "Tags" ]
                                                             Html.ul [ prop.children tags ] ] ] ] ]

    let tagPageContens =
        tagAndPage
        |> Map.toList
        |> List.map (fun (tag, metas) ->
            let lis = metas |> List.map (metaToLi "posts")

            tag,
            Html.div [ prop.className [ "content" ]
                       prop.children [ Html.ul [ prop.children [ Html.li [ Html.h2 tag ]
                                                                 Html.ul lis ] ] ] ])

    tagsContent, tagPageContens


let generateNavbar (title: string) =
    Html.ul [ Html.h1 [ prop.text title ]
              Util.liA "/index.html" "Index"
              Util.liA "/archives.html" "Archives"
              Util.liA "/pages/about.html" "About Me"
              Util.liA "/atom.xml" "RSS"
              Util.liA "/tags.html" "Tags" ]

let generate404 =
    Html.div [ prop.className [ "content" ]
               prop.children [ Html.h1 [ prop.text "404 Page not found" ]
                               Html.p [ prop.text "Sorry! The page you're looking for does not exist." ] ] ]
