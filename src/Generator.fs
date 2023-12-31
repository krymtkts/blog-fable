module StaticWebGenerator

open Common
open Feliz

[<AutoOpen>]
module Generation =
    let generatorName = "blog-fable"

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
                    let lis = metas |> Seq.map (metaToLi root) |> List.ofSeq

                    [ Html.li [ Html.h3 $"{yearMonth} ({List.length lis})" ]
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

                    let refs: Xml.SiteLocation seq =
                        def.metas
                        |> Seq.map (fun meta ->
                            { loc = sourceToSitemap $"%s{pathRoot}%s{def.root}" meta.source
                              lastmod = meta.date
                              priority = def.priority })

                    generate def.metas $"%s{pathRoot}%s{def.root}"
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
          pageRoot: string
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
                |> List.map (fun (tag, metas) ->
                    Component.tagToLi def.tagRoot tag
                    <| List.length metas)

            [ Html.ul [ prop.children [ Html.li [ Html.h2 def.title ]
                                        Html.ul [ prop.children tags ] ] ] ]

        let tagPageContents =
            tagAndPage
            |> Map.toList
            |> List.map (fun (tag, metas) ->
                let lis =
                    metas
                    |> List.map (fun meta ->
                        let parent =
                            match meta.layout with
                            | Post _ -> def.postRoot
                            | Page -> def.pageRoot

                        metaToLi parent meta)

                tag,
                [ Html.ul [ prop.children [ Html.li [ Html.h2 $"{tag} ({List.length metas})" ]
                                            Html.ul lis ] ] ])

        let locs: Xml.SiteLocation seq =
            tagAndPage
            |> Map.toList
            |> Seq.map (fun (tag, _) ->
                { loc = sourceToSitemap def.tagRoot $"%s{tag}.html"
                  lastmod = now |> DateTime.toRFC3339Date
                  priority = def.priority })

        tagsContent, tagPageContents, locs

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
                        { Xml.SiteLocation.loc = $"%s{pathRoot}%s{navi.path}"
                          Xml.SiteLocation.lastmod = now |> DateTime.toRFC3339Date
                          Xml.SiteLocation.priority = n }
                | No -> None

        navs
        |> List.map (function
            | Title navi ->
                liA $"%s{pathRoot}%s{navi.path}"
                <| Element(navi.text, Html.h1 [ prop.text navi.text ])
            | Link navi -> liA $"%s{pathRoot}%s{navi.path}" <| Text navi.text)
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

    let generateSitemap = Xml.createSitemap

    type FeedConf =
        { title: string
          description: string
          link: string
          feed: string
          postRoot: string
          posts: Meta seq
          timeZone: string }

    let generateFeed (conf: FeedConf) =
        let items =
            conf.posts
            |> Seq.rev
            |> Seq.map (Xml.metaToRssItem conf.timeZone $"{conf.link}{conf.postRoot}")

        Xml.createRss
            { title = conf.title
              description = conf.description
              link = conf.link
              xml = conf.feed
              lastBuildDate =
                now
                |> DateTime.toRFC822DateTimeString conf.timeZone
              generator = generatorName }
            items

[<AutoOpen>]
module Rendering =
    let argv = Misc.argv

    type PathConfiguration =
        { siteRoot: string
          postRoot: string
          pageRoot: string
          tagRoot: string }

    let private readSource source =
        promise {
            printfn $"Rendering %s{source}..."
            let! md = IO.readFile source
            let layout = discriminateLayout source

            let pubDate =
                match layout with
                | Post d -> Some(d)
                | Page -> None

            let fm, content =
                md
                |> Parser.parseMarkdownAsReactEl
                |> fun (fm, c) -> fm, c

            let today = DateTime.toRFC3339Date now

            let chooseDate (fm: Parser.FrontMatter option) alt =
                let date =
                    match alt with
                    | Some date -> date
                    | _ -> today

                match fm with
                | Some fm ->
                    match fm.date with
                    | None -> date
                    | Some date -> date
                | None -> date

            let date = chooseDate fm pubDate

            return
                { frontMatter = fm
                  content = content
                  description = content |> Parser.parseReact |> summarizeHtml 120
                  layout = layout
                  source = source
                  leaf = leafHtml source
                  date = date
                  pubDate = pubDate
                  publish = date <= today }
        }

    let private writeContent
        (conf: FrameConfiguration)
        (root: PathConfiguration)
        (meta: Meta)
        (dest: string)
        prev
        next
        =
        promise {
            let path =
                dest
                    .Replace("\\", "/")
                    .Split($"%s{root.siteRoot}/")
                |> Seq.last

            let title =
                match meta.frontMatter with
                | Some fm -> $"%s{conf.title} - %s{fm.title}"
                | None -> conf.title

            let header =
                Component.header $"%s{root.siteRoot}%s{root.tagRoot}/" meta.pubDate meta.frontMatter

            let footer =
                match meta.layout with
                | Post _ -> Component.footer $"%s{root.siteRoot}%s{root.postRoot}/" prev next
                | _ -> []

            let page =
                List.concat [ header
                              [ meta.content ]
                              footer ]
                |> frame
                    { conf with
                        title = title
                        description = meta.description
                        url = $"%s{conf.url}/%s{path}" }
                |> Parser.parseReactStaticHtml

            printfn $"Writing %s{dest}..."

            do! IO.writeFile dest page
        }

    let renderMarkdowns (conf: FrameConfiguration) (site: PathConfiguration) sourceDir destDir =
        promise {
            let! files = getMarkdownFiles sourceDir
            let! metas = files |> List.map readSource |> Promise.all
            let metas = metas |> Array.filter (fun m -> m.publish)

            return!
                metas
                |> Seq.mapi (fun i meta ->
                    promise {
                        let prev, next =
                            match i with
                            | 0 -> None, Some(metas.[i + 1])
                            | i when (i = Seq.length metas - 1) -> Some(metas.[i - 1]), None
                            | i -> Some(metas.[i - 1]), Some(metas.[i + 1])

                        let dest = getDestinationPath meta.source destDir
                        do! writeContent conf site meta dest prev next
                        return meta
                    })
                |> Promise.all
        }

    let renderIndex conf site metaPosts dest =
        let meta, metaPrev =
            match metaPosts
                  |> Seq.sortBy (fun m -> IO.leaf m.source)
                  |> Seq.rev
                  |> Seq.take 2
                  |> List.ofSeq
                with
            | [ post ] -> post, None
            | [ post; prev ] -> post, Some(prev)
            | _ -> failwith "requires at last one post."

        promise {
            let dest = IO.resolve dest
            do! writeContent conf site meta dest metaPrev None
        }

    let renderArchives conf site archives dest =
        promise {
            printfn "Rendering archives..."
            let! archives, locs = generateArchives site.siteRoot archives

            let content =
                archives
                |> frame
                    { conf with
                        title = $"%s{conf.title} - Archives"
                        url = $"%s{conf.url}%s{site.siteRoot}/%s{IO.leaf dest}" }
                |> Parser.parseReactStaticHtml

            printfn $"Writing archives %s{dest}..."

            do! IO.writeFile dest content
            return locs
        }

    let renderTags (conf: FrameConfiguration) (site: PathConfiguration) def dest =
        let tagsContent, tagPageContents, locs = generateTagsContent def

        promise {
            printfn "Rendering tags..."
            let title = $"%s{conf.title} - Tags"

            let content =
                tagsContent
                |> frame
                    { conf with
                        title = title
                        url = $"%s{conf.url}%s{site.siteRoot}/%s{IO.leaf dest}" }
                |> Parser.parseReactStaticHtml

            printfn $"Writing tags %s{dest}..."

            do! IO.writeFile dest content

            do!
                tagPageContents
                |> List.map (fun (tag, tagPageContent) ->
                    let dest = IO.resolve ($"""%s{dest.Replace(".html", "")}/%s{tag}.html""")
                    let parent = dest |> IO.parent |> IO.leaf
                    printfn $"Writing tag %s{dest}..."

                    let content =
                        tagPageContent
                        |> frame
                            { conf with
                                title = $"%s{title} - %s{tag}"
                                url = $"%s{conf.url}%s{site.siteRoot}/%s{parent}/%s{IO.leaf dest}" }
                        |> Parser.parseReactStaticHtml

                    IO.writeFile dest content |> Promise.map ignore)
                |> Promise.all
                |> Promise.map ignore

            return locs
        }

    let render404 conf site dest =
        promise {
            printfn "Rendering 404..."

            let content =
                generate404
                |> frame
                    { conf with
                        title = $"%s{conf.title} - 404"
                        url = $"%s{conf.url}%s{site.siteRoot}/%s{IO.leaf dest}" }
                |> Parser.parseReactStaticHtml

            printfn $"Writing 404 {dest}..."

            do! IO.writeFile dest content
        }

    let renderSitemap root dest (locs: Xml.SiteLocation seq) =
        promise {
            printfn "Rendering sitemap..."
            let sitemap = generateSitemap root locs

            printfn $"Writing sitemap %s{dest}..."
            do! IO.writeFile dest sitemap
        }

    let renderFeed (conf: FeedConf) dest =
        promise {
            printfn "Rendering feed..."
            let feed = generateFeed conf

            printfn $"Writing feed %s{dest}..."
            do! IO.writeFile dest feed
        }

    let copyResources resources =
        promise {
            printfn "Copying resources..."

            return!
                resources
                |> List.map (fun (source, dest) ->
                    promise {
                        printfn $"Copying %s{source} %s{dest}..."
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

type sitemap =
    { index: float
      archives: float
      tags: float
      posts: float
      pages: float }

type RenderOptions =
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
      images: string

      additionalNavs: AdditionalNav list

      feedName: string

      timeZone: string

      sitemap: sitemap

      highlightStyle: string }

module RenderOptions =
    let indexPath = "/index.html"
    let feedPath opts = $"/%s{opts.feedName}.xml"
    let archivesPath opts = $"%s{opts.archives.root}.html"
    let tagsPath opts = $"%s{opts.tags.root}.html"
    let stylePath opts = $"%s{opts.pathRoot}/css/style.css"

    let highlightStylePath opts =
        $"%s{opts.pathRoot}/css/%s{IO.leaf opts.highlightStyle}"

    let devScriptPath opts = $"%s{opts.pathRoot}/js/dev.js"
    let faviconPath opts = $"%s{opts.pathRoot}%s{opts.favicon}"

    let postsRootPath opts = $"%s{opts.pathRoot}%s{opts.posts.root}"
    let pagesRootPath opts = $"%s{opts.pathRoot}%s{opts.pages.root}"
    let tagsRootPath opts = $"%s{opts.pathRoot}%s{opts.tags.root}"

    let siteUrl opts = $"%s{opts.siteUrl}%s{opts.pathRoot}"

    let postsSourceRoot opts = $"%s{opts.src}%s{opts.posts.root}"

    let pagesSourceRoot opts = $"%s{opts.src}%s{opts.pages.root}"
    let devScriptSourcePath = "src/Dev.fs.js"
    let imagesSourcePath opts = $"%s{opts.src}/%s{opts.images}"

    let destinationRoot opts = $"%s{opts.dst}%s{opts.pathRoot}"

    let indexDestinationPath opts =
        $"%s{destinationRoot opts}%s{indexPath}"

    let postsDestinationRoot opts =
        $"%s{destinationRoot opts}%s{opts.posts.root}"

    let pagesDestinationRoot opts =
        $"%s{destinationRoot opts}%s{opts.pages.root}"

    let archivesDestinationPath opts =
        $"%s{destinationRoot opts}%s{archivesPath opts}"

    let tagsDestinationPath opts =
        $"%s{destinationRoot opts}%s{tagsPath opts}"

    let ``404DestinationPath`` opts = $"%s{destinationRoot opts}/404.html"

    let sitemapDestinationPath opts = $"%s{destinationRoot opts}/sitemap.xml"

    let feedDestinationPath opts =
        $"%s{destinationRoot opts}%s{feedPath opts}"

    let devScriptDestinationPath opts = $"%s{opts.dst}%s{devScriptPath opts}"

    let imagesDestinationPath opts =
        $"%s{destinationRoot opts}/%s{opts.images}"

    let highlightStyleDestinationPath opts =
        $"%s{destinationRoot opts}/css/%s{IO.leaf opts.highlightStyle}"

let private buildNavList opts =
    let feed = RenderOptions.feedPath opts

    feed,
    List.concat [ [ Title
                        { text = opts.siteName
                          path = RenderOptions.indexPath
                          sitemap = Yes <| string opts.sitemap.index }
                    Link
                        { text = opts.archives.title
                          path = RenderOptions.archivesPath opts
                          sitemap = Yes <| string opts.sitemap.archives }
                    Link
                        { text = opts.tags.title
                          path = RenderOptions.tagsPath opts
                          sitemap = Yes <| string opts.sitemap.tags } ]
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
    | Development ->
        Some(RenderOptions.devScriptPath opts),
        [ (RenderOptions.devScriptSourcePath, RenderOptions.devScriptDestinationPath opts) ]
    | Production -> None, []

let private buildHighlightStyle opts =
    RenderOptions.highlightStylePath opts, [ (opts.highlightStyle, RenderOptions.highlightStyleDestinationPath opts) ]

let render (opts: RenderOptions) =
    promise {
        let feed, navs = buildNavList opts

        let (navbar: Fable.React.ReactElement), navSitemap =
            generateNavbar opts.pathRoot navs

        let devInjection, devScript = buildDevScript opts
        let highlightInjection, highlightStyle = buildHighlightStyle opts

        let site: PathConfiguration =
            { siteRoot = opts.pathRoot
              postRoot = opts.posts.root
              pageRoot = opts.pages.root
              tagRoot = opts.tags.root }

        let conf: FrameConfiguration =
            { lang = opts.lang
              navbar = navbar
              name = opts.siteName
              title = opts.siteName
              description = opts.description
              url = opts.siteUrl
              copyright = opts.copyright
              favicon = RenderOptions.faviconPath opts
              style = RenderOptions.stylePath opts
              highlightStyle = highlightInjection
              devInjection = devInjection }

        let renderPostAndPages = renderMarkdowns conf site

        let! metaPosts =
            renderPostAndPages
            <| RenderOptions.postsSourceRoot opts
            <| RenderOptions.postsDestinationRoot opts

        let! metaPages =
            renderPostAndPages
            <| RenderOptions.pagesSourceRoot opts
            <| RenderOptions.pagesDestinationRoot opts

        do!
            renderIndex conf site metaPosts
            <| RenderOptions.indexDestinationPath opts

        let archiveDefs =
            [ Posts
                  { title = opts.posts.title
                    metas = metaPosts
                    root = opts.posts.root
                    priority = string opts.sitemap.posts }
              Pages
                  { title = opts.pages.title
                    metas = metaPages
                    root = opts.pages.root
                    priority = string opts.sitemap.pages } ]

        let! archiveLocs =
            renderArchives conf site archiveDefs
            <| RenderOptions.archivesDestinationPath opts

        let tagDef =
            { title = opts.tags.title
              tagRoot = RenderOptions.tagsRootPath opts
              postRoot = RenderOptions.postsRootPath opts
              pageRoot = RenderOptions.pagesRootPath opts
              metas = Seq.concat [ metaPosts; metaPages ]
              priority = string opts.sitemap.tags }

        let! tagLocs =
            renderTags conf site tagDef
            <| RenderOptions.tagsDestinationPath opts

        do!
            render404 conf site
            <| RenderOptions.``404DestinationPath`` opts

        do!
            renderSitemap conf.url
            <| RenderOptions.sitemapDestinationPath opts
            <| (Seq.concat [ navSitemap
                             tagLocs
                             archiveLocs ])

        do!
            renderFeed
                { title = opts.siteName
                  description = opts.description
                  link = RenderOptions.siteUrl opts
                  feed = feed
                  postRoot = opts.posts.root
                  posts = metaPosts
                  timeZone = opts.timeZone }
            <| RenderOptions.feedDestinationPath opts

        let! paths =
            getImagePathPairs
            <| RenderOptions.imagesSourcePath opts
            <| RenderOptions.imagesDestinationPath opts

        do!
            copyResources
            <| devScript @ paths @ highlightStyle

        printfn "Render complete!"
    }
    |> ignore

let dev =
    match List.ofSeq argv with
    | [ _; _; mode ] when mode = "dev" -> Development
    | _ -> Production
