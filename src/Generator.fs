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

            let ref =
                meta
                |> Seq.map (fun meta ->
                    { loc = sourceToSitemap root meta.source
                      lastmod = meta.date
                      priority = "0.8" })

            return Html.ul [ prop.children (List.concat archives) ], ref
        }

    let generatePageArchives (meta: Meta seq) root =
        promise {
            let archives =
                meta
                |> Seq.sortBy (fun meta -> IO.leaf meta.source)
                |> Seq.map (metaToLi root)

            return Html.ul [ prop.children archives ]
        }

    type Archives =
        { title: string
          metas: Meta seq
          root: string }

    let generateArchives (archives: Archives list) =
        promise {
            let! a =
                archives
                |> List.map (fun archive ->
                    generatePostArchives archive.metas archive.root
                    |> Promise.map (fun (content, refs) ->
                        [ Html.li [ Html.h2 archive.title ]
                          content ],
                        refs))
                |> Promise.all

            let a, refs = a |> List.ofSeq |> List.unzip
            let locs = refs |> Seq.concat

            return [ Html.ul [ prop.children (List.concat a) ] ], locs
        }

    let generateTagsContent (meta: Meta seq) tagRoot =
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
                |> List.map (fun (tag, _) ->
                    Component.pathToLi tagRoot
                    <| sprintf "%s.html" tag)

            [ Html.ul [ prop.children [ Html.li [ Html.h2 "Tags" ]
                                        Html.ul [ prop.children tags ] ] ] ]

        let tagPageContens =
            tagAndPage
            |> Map.toList
            |> List.map (fun (tag, metas) ->
                let lis = metas |> List.map (metaToLi "blog-fable/posts")

                tag,
                [ Html.ul [ prop.children [ Html.li [ Html.h2 tag ]
                                            Html.ul lis ] ] ])

        let locs =
            tagAndPage
            |> Map.toList
            |> Seq.map (fun (tag, _) ->
                { loc = sourceToSitemap tagRoot <| sprintf "%s.html" tag
                  lastmod = now.ToString("yyyy-MM-dd")
                  priority = "0.9" })

        tagsContent, tagPageContens, locs


    type NavItem =
        { text: string
          path: string
          useSitemap: bool }

    type Nav =
        | Title of NavItem
        | Link of NavItem


    let generateNavbar (navs: Nav list) =
        let toSitemap =
            let d = now.ToString("yyyy-MM-dd")

            function
            | Title navi ->
                { loc = navi.path
                  lastmod = d
                  priority = "1.0" }
            | Link navi ->
                { loc = navi.path
                  lastmod = d
                  priority = "0.9" }

        navs
        |> List.map (fun nav ->
            match nav with
            | Title navi ->
                Component.liA navi.path
                <| Component.Element(navi.text, Html.h1 [ prop.text navi.text ])
            | Link navi ->
                Component.liA navi.path
                <| Component.Text navi.text)
        |> Html.ul,
        navs
        |> Seq.filter (function
            | Title ni -> ni.useSitemap
            | Link ni -> ni.useSitemap)
        |> Seq.map toSitemap

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

[<AutoOpen>]
module Page =
    let argv = Misc.argv
    type FixedSiteContent = Misc.FixedSiteContent

    let private readAndWrite (site: FixedSiteContent) tagDist source dist =
        promise {
            printfn "Rendering %s..." source
            let! m = IO.readFile source

            let tagToElement tag =
                Component.liAWithClass (sprintf "%s/%s.html" tagDist tag) tag [ "tag" ]

            let fm, content =
                m
                |> Parser.parseMarkdownAsReactEl tagToElement
                |> fun (fm, c) ->
                    let title =
                        match fm with
                        | Some fm -> sprintf "%s - %s" site.title fm.title
                        | None -> site.title

                    fm,
                    frame { site with title = title } c
                    |> Parser.parseReactStatic


            printfn "Writing %s..." dist

            do! IO.writeFile dist content

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
                  layout = layout
                  source = source
                  dist = dist
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
                |> frame { site with title = sprintf "%s - Archives" site.title }
                |> Parser.parseReactStatic

            printfn "Writing archives %s..." dist

            do! IO.writeFile dist content
            return locs
        }

    let renderTags (site: FixedSiteContent) tagRoot meta dist =
        let tagsContent, tagPageContents, locs = generateTagsContent meta tagRoot

        promise {
            printfn "Rendering tags..."
            let title = (sprintf "%s - Tags" site.title)

            let content =
                tagsContent
                |> frame { site with title = title }
                |> Parser.parseReactStatic

            printfn "Writing tags %s..." dist

            do! IO.writeFile dist content

            do!
                tagPageContents
                |> List.map (fun (tag, tagPageContent) ->
                    let dist =
                        IO.resolve (
                            sprintf "%s/%s.html"
                            <| dist.Replace(".html", "")
                            <| tag
                        )

                    printfn "Writing tag %s..." dist

                    let content =
                        tagPageContent
                        |> frame { site with title = sprintf "%s - %s" title tag }
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
                |> frame { site with title = sprintf "%s - 404" site.title }
                |> Parser.parseReactStatic

            printfn "Writing 404 %s..." dist

            do! IO.writeFile dist content
        }

    let renderSitemap root dist (locs: SiteLocation seq) =
        promise {
            printfn "Rendering sitemap..."
            let sitemap = generateSitemap root locs

            printfn "Writing archives %s..." dist
            do! IO.writeFile dist sitemap
        }

    let copyResources resources =
        promise {
            printfn "Copying resources..."

            return!
                resources
                |> List.map (fun (source, dist) ->
                    promise {
                        printfn "Copying %s..." source
                        do! IO.copy source dist
                    })
                |> Promise.all
                |> Promise.map ignore
        }
