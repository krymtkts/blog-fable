module StaticWebGenerator

open Common
open Feliz
open Fable.SimpleXml.Generator

[<AutoOpen>]
module Generation =
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

    let generateArchives (archives: Archive list) =
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
                            { loc = sourceToSitemap def.root meta.source
                              lastmod = meta.date
                              priority = def.priority })

                    generate def.metas def.root
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
          root: string
          postRoot: string
          priority: string }

    let generateTagsContent def =
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
                |> List.map (fun (tag, _) -> Component.pathToLi def.root $"{tag}.html")

            [ Html.ul [ prop.children [ Html.li [ Html.h2 def.title ]
                                        Html.ul [ prop.children tags ] ] ] ]

        let tagPageContens =
            tagAndPage
            |> Map.toList
            |> List.map (fun (tag, metas) ->
                let lis = metas |> List.map (metaToLi def.postRoot)

                tag,
                [ Html.ul [ prop.children [ Html.li [ Html.h2 tag ]
                                            Html.ul lis ] ] ])

        let locs =
            tagAndPage
            |> Map.toList
            |> Seq.map (fun (tag, _) ->
                { loc = sourceToSitemap def.root $"{tag}.html"
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


    let generateNavbar (navs: Nav list) =
        let toSitemap =
            function
            | Title navi
            | Link navi ->
                match navi.sitemap with
                | Yes n ->
                    Some
                        { loc = navi.path
                          lastmod = now.ToString("yyyy-MM-dd")
                          priority = n }
                | No -> None

        navs
        |> List.map (function
            | Title navi ->
                Component.liA navi.path
                <| Component.Element(navi.text, Html.h1 [ prop.text navi.text ])
            | Link navi ->
                Component.liA navi.path
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
      lastBuildDate: string
      generator: string }

type FeedConf =
    { title: string
      description: string
      link: string
      generator: string
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
                   [ attr.value ("href", channel.link)
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
        |> Seq.map (fun meta ->
            let link = $"{conf.link}{conf.postRoot}/{meta.leaf}"

            { guid = link
              link = link
              title =
                match meta.frontMatter with
                | Some fm -> fm.title
                | None -> meta.leaf
              description = meta.content |> simpleEscape
              pubDate =
                match meta.frontMatter with
                | Some fm ->
                    match fm.date with
                    | Some d -> d
                    | None -> meta.date
                | None -> meta.date })

    let rss =
        createRss
            { title = conf.title
              description = conf.description
              link = conf.link
              lastBuildDate = now.ToString("yyyy-MM-dd")
              generator = conf.generator }
            items

    rss
    |> serializeXml
    |> (+) @"<?xml version=""1.0"" encoding=""UTF-8""?>"

[<AutoOpen>]
module Page =
    let argv = Misc.argv
    type FixedSiteContent = Misc.FixedSiteContent

    let private readAndWrite (site: FixedSiteContent) tagDist source dist =
        promise {
            printfn $"Rendering {source}..."
            let! m = IO.readFile source

            let tagToElement tag =
                Component.liAWithClass $"{tagDist}/{tag}.html" tag [ "tag" ]

            let leaf = IO.leaf dist

            // TODO: add root path to fixed site content for removing this condition.
            let path =
                match leaf with
                | "index.html" -> leaf
                | _ -> $"{dist |> IO.parent |> IO.leaf}/{leaf}"

            let fm, content, page =
                m
                |> Parser.parseMarkdownAsReactEl tagToElement
                |> fun (fm, c) ->
                    let title =
                        match fm with
                        | Some fm -> $"{site.title} - {fm.title}"
                        | None -> site.title

                    let content = wrapContent c

                    fm,
                    content |> Parser.parseReactStatic,
                    frame
                        { site with
                            title = title
                            url = $"{site.url}/{path}" }
                        content
                    |> Parser.parseReactStatic


            printfn $"Writing {dist}..."

            do! IO.writeFile dist page

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

    let renderMarkdowns site tagDist sourceDir distDir =
        promise {
            let! files = getMarkdownFiles sourceDir
            let rw = readAndWrite site tagDist

            return!
                files
                |> List.map (fun source ->
                    let dist = getDistPath source distDir

                    promise {
                        let! meta = rw source dist

                        return meta

                    })
                |> Promise.all
        }

    let renderIndex site tagDist metaPosts dist =
        let latest =
            metaPosts
            |> Seq.map (fun m -> m.source)
            |> getLatestPost

        promise {
            let rw = readAndWrite site tagDist
            let dist = IO.resolve dist

            do! rw latest dist |> Promise.map ignore
        }

    let renderArchives site archives dist =
        promise {
            printfn "Rendering archives..."
            let! archives, locs = generateArchives archives

            let content =
                archives
                |> wrapContent
                |> frame
                    { site with
                        title = $"{site.title} - Archives"
                        url = $"{site.url}/{IO.leaf dist}" }
                |> Parser.parseReactStatic

            printfn $"Writing archives {dist}..."

            do! IO.writeFile dist content
            return locs
        }

    let renderTags (site: FixedSiteContent) def dist =
        let tagsContent, tagPageContents, locs = generateTagsContent def

        promise {
            printfn "Rendering tags..."
            let title = $"{site.title} - Tags"

            let content =
                tagsContent
                |> wrapContent
                |> frame
                    { site with
                        title = title
                        url = $"{site.url}/{IO.leaf dist}" }
                |> Parser.parseReactStatic

            printfn $"Writing tags {dist}..."

            do! IO.writeFile dist content

            do!
                tagPageContents
                |> List.map (fun (tag, tagPageContent) ->
                    let dist = IO.resolve ($"""{dist.Replace(".html", "")}/{tag}.html""")
                    let parent = dist |> IO.parent |> IO.leaf
                    printfn $"Writing tag {dist}..."

                    let content =
                        tagPageContent
                        |> wrapContent
                        |> frame
                            { site with
                                title = $"{title} - {tag}"
                                url = $"{site.url}/{parent}/{IO.leaf dist}" }
                        |> Parser.parseReactStatic

                    IO.writeFile dist content |> Promise.map ignore)
                |> Promise.all
                |> Promise.map ignore

            return locs
        }

    let render404 site dist =
        promise {
            printfn "Rendering 404..."

            let content =
                generate404
                |> wrapContent
                |> frame
                    { site with
                        title = $"{site.title} - 404"
                        url = $"{site.url}/{IO.leaf dist}" }
                |> Parser.parseReactStatic

            printfn $"Writing 404 {dist}..."

            do! IO.writeFile dist content
        }

    let renderSitemap root dist (locs: SiteLocation seq) =
        promise {
            printfn "Rendering sitemap..."
            let sitemap = generateSitemap root locs

            printfn $"Writing sitemap {dist}..."
            do! IO.writeFile dist sitemap
        }

    let renderFeed (conf: FeedConf) dist =
        promise {
            printfn "Rendering feed..."
            let feed = generateFeed conf

            printfn $"Writing feed {dist}..."
            do! IO.writeFile dist feed
        }

    let copyResources resources =
        promise {
            printfn "Copying resources..."

            return!
                resources
                |> List.map (fun (source, dist) ->
                    promise {
                        printfn $"Copying {source}..."
                        do! IO.copy source dist
                    })
                |> Promise.all
                |> Promise.map ignore
        }
