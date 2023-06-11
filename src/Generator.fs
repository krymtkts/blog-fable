module StaticWebGenerator

open Common
open Feliz

[<AutoOpen>]
module Generation =

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
                    let lis = metas |> Seq.map (fun meta -> metaToLi root meta)

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

    type Archives =
        { title: string
          metas: Meta seq
          root: string }

    let generateArchives (archives: Archives list) =
        promise {
            let! a =
                archives
                |> List.map (fun archives ->
                    generatePostArchives archives.metas archives.root
                    |> Promise.map (fun content ->
                        [ Html.li [ Html.h2 archives.title ]
                          content ]))
                |> Promise.all

            return [ Html.ul [ prop.children (List.concat a) ] ]
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

        tagsContent, tagPageContens


    type NavItem = { text: string; path: string }

    type Nav =
        | Title of NavItem
        | Link of NavItem

    let generateNavbar (navs: Nav list) =
        navs
        |> List.map (fun nav ->
            match nav with
            | Title navi ->
                Component.liA navi.path
                <| Component.Element(navi.text, Html.h1 [ prop.text navi.text ])
            | Link navi ->
                Component.liA navi.path
                <| Component.Text navi.text)
        |> Html.ul

    let generate404 =
        [ Html.h1 [ prop.text "404 Page not found" ]
          Html.p [ prop.text "Sorry! The page you're looking for does not exist." ] ]

[<AutoOpen>]
module Page =
    type FixedSiteContent =
        { navbar: ReactElement
          title: string
          copyright: string
          favicon: string }

    let private readAndWrite site tagDist source dist =
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
                    frame site.navbar title site.copyright site.favicon c
                    |> Parser.parseReactStatic


            printfn "Writing %s..." dist

            do! IO.writeFile dist content

            return
                { frontMatter = fm
                  layout = discriminateLayout source
                  source = source
                  dist = dist }
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
            let! archives = generateArchives archives

            let content =
                archives
                |> frame site.navbar (sprintf "%s - Archives" site.title) site.copyright site.favicon
                |> Parser.parseReactStatic

            printfn "Writing archives %s..." dist

            do! IO.writeFile dist content
        }

    let renderTags site tagRoot meta dist =
        let tagsContent, tagPageContents = generateTagsContent meta tagRoot
        let frame = frame site.navbar

        promise {
            printfn "Rendering tags..."
            let title = (sprintf "%s - Tags" site.title)

            let content =
                tagsContent
                |> frame title site.copyright site.favicon
                |> Parser.parseReactStatic

            printfn "Writing tags %s..." dist

            do! IO.writeFile dist content

            return!
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
                        |> frame (sprintf "%s - %s" title tag) site.copyright site.favicon
                        |> Parser.parseReactStatic

                    IO.writeFile dist content |> Promise.map ignore)
                |> Promise.all
                |> Promise.map ignore
        }

    let render404 site dist =
        promise {
            printfn "Rendering 404..."

            let content =
                generate404
                |> frame site.navbar (sprintf "%s - 404" site.title) site.copyright site.favicon
                |> Parser.parseReactStatic

            printfn "Writing 404 %s..." dist

            do! IO.writeFile dist content
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
