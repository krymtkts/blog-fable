module StaticWebGenerator

open Common
open Feliz
open Fable.SimpleXml.Generator

[<AutoOpen>]
module Generation =
    let generatorName = "blog-fable"

    type SiteLocation =
        { loc: string
          lastmod: string
          priority: string }

    let generatePostArchives (meta: Meta seq) root =
        promise {
            let archives =
                meta
                |> Seq.sortBy (fun meta -> IO.leaf meta.source)
                |> Seq.rev
                |> Seq.groupBy (fun meta ->
                    let leaf = IO.leaf meta.source
                    leaf.Substring(0, 7))
                |> Seq.map (fun (yearMonth, metas) ->
                    let lis =
                        metas
                        |> Seq.map (fun meta -> metaToLi root meta)
                        |> List.ofSeq

                    [ Html.li [ Html.h3 yearMonth ]
                      Html.ul lis ])

            return Html.ul [ prop.children (List.concat archives) ]
        }

    let generatePageArchives (meta: Meta seq) root =
        promise {
            let archives =
                meta
                |> Seq.sortBy (fun meta -> IO.leaf meta.source)
                |> Seq.map (metaToLi root)

            return Html.ul [ prop.children archives ]
        }

    type ArchiveDef =
        { title: string
          metas: Meta seq
          root: string
          priority: string }

    type Archive =
        | Posts of ArchiveDef
        | Pages of ArchiveDef

    let generateArchives pathRoot (archives: Archive list) =
        promise {
            let! a =
                archives
                |> List.map (fun archive ->
                    let generate, def =
                        match archive with
                        | Posts d -> generatePostArchives, d
                        | Pages d -> generatePageArchives, d

                    let refs =
                        def.metas
                        |> Seq.map (fun meta ->
                            { loc = sourceToSitemap $"{pathRoot}{def.root}" meta.source
                              lastmod = meta.date
                              priority = def.priority })

                    generate def.metas $"{pathRoot}{def.root}"
                    |> Promise.map (fun content ->
                        [ Html.li [ Html.h2 def.title ]
                          content ],
                        refs))
                |> Promise.all

            let a, refs = a |> List.ofSeq |> List.unzip
            let locs = refs |> Seq.concat

            return [ Html.ul [ prop.children (List.concat a) ] ], locs
        }

    type TagDef =
        { title: string
          metas: Meta seq
          tagRoot: string
          postRoot: string
          priority: string }

    let generateTagsContent pathRoot def =
        let tagAndPage =
            def.metas
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
                |> List.map (fun (tag, _) -> Component.pathToLi $"{pathRoot}{def.tagRoot}" $"{tag}.html")

            [ Html.ul [ prop.children [ Html.li [ Html.h2 def.title ]
                                        Html.ul [ prop.children tags ] ] ] ]

        let tagPageContens =
            tagAndPage
            |> Map.toList
            |> List.map (fun (tag, metas) ->
                let lis =
                    metas
                    |> List.map (metaToLi $"{pathRoot}{def.postRoot}")

                tag,
                [ Html.ul [ prop.children [ Html.li [ Html.h2 tag ]
                                            Html.ul lis ] ] ])

        let locs =
            tagAndPage
            |> Map.toList
            |> Seq.map (fun (tag, _) ->
                { loc = sourceToSitemap $"{pathRoot}{def.tagRoot}" $"{tag}.html"
                  lastmod = now.ToString("yyyy-MM-dd")
                  priority = def.priority })

        tagsContent, tagPageContens, locs

    type UseSitemap =
        | Yes of string
        | No

    type NavItem =
        { text: string
          path: string
          sitemap: UseSitemap }

    type Nav =
        | Title of NavItem
        | Link of NavItem


    let generateNavbar pathRoot (navs: Nav list) =
        let toSitemap =
            function
            | Title navi
            | Link navi ->
                match navi.sitemap with
                | Yes n ->
                    Some
                        { loc = $"{pathRoot}{navi.path}"
                          lastmod = now.ToString("yyyy-MM-dd")
                          priority = n }
                | No -> None

        navs
        |> List.map (function
            | Title navi ->
                Component.liA $"{pathRoot}{navi.path}"
                <| Component.Element(navi.text, Html.h1 [ prop.text navi.text ])
            | Link navi ->
                Component.liA $"{pathRoot}{navi.path}"
                <| Component.Text navi.text)
        |> Html.ul,
        navs
        |> Seq.map toSitemap
        |> Seq.filter (function
            | Some _ -> true
            | None -> false)
        |> Seq.map Option.get

    let generate404 =
        [ Html.h1 [ prop.text "404 Page not found" ]
          Html.p [ prop.text "Sorry! The page you're looking for does not exist." ] ]

    let generateSitemap root locs =
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

    type RssChannel =
        { title: string
          description: string
          link: string
          xml: string
          lastBuildDate: string
          generator: string }

    type FeedConf =
        { title: string
          description: string
          link: string
          feed: string
          postRoot: string
          posts: Meta seq }

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

    let generateFeed (conf: FeedConf) =
        let items =
            conf.posts
            |> Seq.rev
            |> Seq.map (fun meta ->
                let link = $"{conf.link}{conf.postRoot}/{meta.leaf}"

                let pubDate =
                    match meta.frontMatter with
                    | Some fm ->
                        match fm.date with
                        | Some d -> d
                        | None -> meta.date
                    | None -> meta.date
                    |> String.toRFC822DateTime

                { guid = link
                  link = link
                  title =
                    match meta.frontMatter with
                    | Some fm -> fm.title
                    | None -> meta.leaf
                  description = meta.content |> simpleEscape
                  pubDate = pubDate })


        let rss =
            createRss
                { title = conf.title
                  description = conf.description
                  link = conf.link
                  xml = conf.feed
                  lastBuildDate = now |> DateTime.toRFC822DateTime
                  generator = generatorName }
                items

        rss
        |> serializeXml
        |> (+) @"<?xml version=""1.0"" encoding=""UTF-8""?>"

[<AutoOpen>]
module Rndering =
    let argv = Misc.argv
    type FixedSiteContent = Misc.FixedSiteContent

    let private readAndWrite (site: FixedSiteContent) tagDest source dest =
        promise {
            printfn $"Rendering {source}..."
            let! m = IO.readFile source

            let tagToElement tag =
                Component.liAWithClass $"{site.pathRoot}{tagDest}/{tag}.html" tag [ "tag" ]

            let leaf = IO.leaf dest

            let path =
                dest.Replace("\\", "/").Split($"{site.pathRoot}/")
                |> Seq.last

            let fm, content, page =
                m
                |> Parser.parseMarkdownAsReactEl tagToElement
                |> fun (fm, h, c) ->
                    let title =
                        match fm with
                        | Some fm -> $"{site.title} - {fm.title}"
                        | None -> site.title

                    fm,
                    c |> Parser.parseReactStaticMarkup,
                    List.append h [ c ]
                    |> wrapContent
                    |> frame
                        { site with
                            title = title
                            url = $"{site.url}/{path}" }

                    |> Parser.parseReactStaticHtml


            printfn $"Writing {dest}..."

            do! IO.writeFile dest page

            let layout = discriminateLayout source

            let chooseDate (fm: Parser.FrontMatter option) alt =
                match fm with
                | Some fm ->
                    match fm.date with
                    | None -> alt
                    | Some x -> x
                | None -> alt

            let date =
                match layout with
                | Post d -> chooseDate fm d
                | Page -> chooseDate fm <| now.ToString("yyyy-MM-dd")

            return
                { frontMatter = fm
                  content = content
                  layout = layout
                  source = source
                  leaf = leaf
                  date = date }
        }

    let renderMarkdowns site tagDest sourceDir destDir =
        promise {
            let! files = getMarkdownFiles sourceDir
            let rw = readAndWrite site tagDest

            return!
                files
                |> List.map (fun source ->
                    let dest = getDestinationPath source destDir

                    promise {
                        let! meta = rw source dest

                        return meta

                    })
                |> Promise.all
        }

    let renderIndex site tagDest metaPosts dest =
        let latest =
            metaPosts
            |> Seq.map (fun m -> m.source)
            |> getLatestPost

        promise {
            let rw = readAndWrite site tagDest
            let dest = IO.resolve dest

            do! rw latest dest |> Promise.map ignore
        }

    let renderArchives site archives dest =
        promise {
            printfn "Rendering archives..."
            let! archives, locs = generateArchives site.pathRoot archives

            let content =
                archives
                |> wrapContent
                |> frame
                    { site with
                        title = $"{site.title} - Archives"
                        url = $"{site.url}{site.pathRoot}/{IO.leaf dest}" }
                |> Parser.parseReactStaticHtml

            printfn $"Writing archives {dest}..."

            do! IO.writeFile dest content
            return locs
        }

    let renderTags (site: FixedSiteContent) def dest =
        let tagsContent, tagPageContents, locs = generateTagsContent site.pathRoot def

        promise {
            printfn "Rendering tags..."
            let title = $"{site.title} - Tags"

            let content =
                tagsContent
                |> wrapContent
                |> frame
                    { site with
                        title = title
                        url = $"{site.url}{site.pathRoot}/{IO.leaf dest}" }
                |> Parser.parseReactStaticHtml

            printfn $"Writing tags {dest}..."

            do! IO.writeFile dest content

            do!
                tagPageContents
                |> List.map (fun (tag, tagPageContent) ->
                    let dest = IO.resolve ($"""{dest.Replace(".html", "")}/{tag}.html""")
                    let parent = dest |> IO.parent |> IO.leaf
                    printfn $"Writing tag {dest}..."

                    let content =
                        tagPageContent
                        |> wrapContent
                        |> frame
                            { site with
                                title = $"{title} - {tag}"
                                url = $"{site.url}{site.pathRoot}/{parent}/{IO.leaf dest}" }
                        |> Parser.parseReactStaticHtml

                    IO.writeFile dest content |> Promise.map ignore)
                |> Promise.all
                |> Promise.map ignore

            return locs
        }

    let render404 site dest =
        promise {
            printfn "Rendering 404..."

            let content =
                generate404
                |> wrapContent
                |> frame
                    { site with
                        title = $"{site.title} - 404"
                        url = $"{site.url}{site.pathRoot}/{IO.leaf dest}" }
                |> Parser.parseReactStaticHtml

            printfn $"Writing 404 {dest}..."

            do! IO.writeFile dest content
        }

    let renderSitemap root dest (locs: SiteLocation seq) =
        promise {
            printfn "Rendering sitemap..."
            let sitemap = generateSitemap root locs

            printfn $"Writing sitemap {dest}..."
            do! IO.writeFile dest sitemap
        }

    let renderFeed (conf: FeedConf) dest =
        promise {
            printfn "Rendering feed..."
            let feed = generateFeed conf

            printfn $"Writing feed {dest}..."
            do! IO.writeFile dest feed
        }

    let copyResources resources =
        promise {
            printfn "Copying resources..."

            return!
                resources
                |> List.map (fun (source, dest) ->
                    promise {
                        printfn $"Copying {source}..."
                        do! IO.copy source dest
                    })
                |> Promise.all
                |> Promise.map ignore
        }

type Mode =
    | Development
    | Production

type Content = { root: string; title: string }

type AdditionalNav = { text: string; path: string }

type RnderOptions =
    { stage: Mode
      siteName: string
      description: string
      siteUrl: string
      pathRoot: string
      lang: string
      copyright: string
      favicon: string

      src: string
      dst: string

      posts: Content
      pages: Content
      tags: Content
      archives: Content

      additionalNavs: AdditionalNav list

      feedName: string

     }

let private buildNavList opts =
    let index = "/index.html"
    let feed = $"/{opts.feedName}.xml"

    index,
    feed,
    List.concat [ [ Title
                        { text = opts.siteName
                          path = index
                          sitemap = Yes "1.0" }
                    Link
                        { text = opts.archives.title
                          path = $"{opts.archives.root}.html"
                          sitemap = Yes "0.9" }
                    Link
                        { text = opts.tags.title
                          path = $"{opts.tags.root}.html"
                          sitemap = Yes "0.9" } ]
                  List.map
                      (fun n ->
                          Link
                              { text = n.text
                                path = n.path
                                sitemap = No })
                      opts.additionalNavs
                  [ Link
                        { text = "RSS"
                          path = feed
                          sitemap = No } ] ]

let private buildDevScript opts =
    match opts.stage with
    | Development -> Some("/js/dev.js"), [ ("src/Dev.fs.js", $"{opts.dst}{opts.pathRoot}/js/dev.js") ]
    | Production -> None, []

let render (opts: RnderOptions) =
    promise {
        let index, feed, navs = buildNavList opts

        let (navbar: Fable.React.ReactElement), navSitemap =
            generateNavbar opts.pathRoot navs

        let devInjection, devScript = buildDevScript opts

        let site: FixedSiteContent =
            { lang = opts.lang
              navbar = navbar
              name = opts.siteName
              title = opts.siteName
              description = opts.description
              url = opts.siteUrl
              pathRoot = opts.pathRoot // TODO: remove pathRoot from here and add to new created path type that includes src, dst and pathRoot.
              copyright = opts.copyright
              favicon = opts.favicon
              devInjection = devInjection }

        let renderPostAndPages = renderMarkdowns site opts.tags.root
        let! metaPosts = renderPostAndPages $"{opts.src}{opts.posts.root}" $"{opts.dst}{opts.pathRoot}{opts.posts.root}"
        let! metaPages = renderPostAndPages $"{opts.src}{opts.pages.root}" $"{opts.dst}{opts.pathRoot}{opts.pages.root}"

        do! renderIndex site opts.tags.root metaPosts $"{opts.dst}{opts.pathRoot}{index}"

        let archiveDefs =
            [ Posts
                  { title = opts.posts.title
                    metas = metaPosts
                    root = opts.posts.root
                    priority = "0.8" }
              Pages
                  { title = opts.pages.title
                    metas = metaPages
                    root = opts.pages.root
                    priority = "0.8" } ]

        let! archiveLocs = renderArchives site archiveDefs $"{opts.dst}{site.pathRoot}{opts.archives.root}.html"

        let tagDef =
            { title = opts.tags.title
              metas = Seq.concat [ metaPosts; metaPages ]
              tagRoot = opts.tags.root
              postRoot = opts.posts.root
              priority = "0.9" }

        let! tagLocs = renderTags site tagDef $"{opts.dst}{site.pathRoot}{opts.tags.root}.html"
        do! render404 site $"{opts.dst}{opts.pathRoot}/404.html"

        do!
            renderSitemap
                site.url
                $"{opts.dst}{opts.pathRoot}/sitemap.xml"
                (Seq.concat [ navSitemap
                              tagLocs
                              archiveLocs ])

        do!
            renderFeed
                { title = opts.siteName
                  description = opts.description
                  link = $"{opts.siteUrl}{opts.pathRoot}"
                  feed = feed
                  postRoot = opts.posts.root
                  posts = metaPosts }
                $"{opts.dst}{opts.pathRoot}{feed}"

        do!
            copyResources
            <| [ ($"{opts.src}{opts.favicon}", $"{opts.dst}{opts.pathRoot}{opts.favicon}") ]
               @ devScript

        printfn "Render complete!"
    }
    |> ignore

let dev =
    match List.ofSeq argv with
    | [ _; _; mode ] when mode = "dev" -> Development
    | _ -> Production
