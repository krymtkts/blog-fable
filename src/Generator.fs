module StaticWebGenerator

open Common
open Feliz

[<AutoOpen>]
module Generation =

    let generatePostArchives (meta: Meta seq) group =
        promise {
            let archives =
                meta
                |> Seq.sortBy (fun meta -> IO.leaf meta.source)
                |> Seq.rev
                |> Seq.groupBy (fun meta ->
                    let leaf = IO.leaf meta.source
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
                |> Seq.sortBy (fun meta -> IO.leaf meta.source)
                |> Seq.map (metaToLi group)

            return Html.ul [ prop.children archives ]
        }

    let generateArchives (metaPosts: Meta seq) (metaPages: Meta seq) =
        promise {
            let! posts = generatePostArchives metaPosts "posts"
            let! pages = generatePageArchives metaPages "pages"

            return
                [ Html.ul [ prop.children [ Html.li [ Html.h2 "Posts" ]
                                            posts
                                            Html.li [ Html.h2 "Pages" ]
                                            pages ] ] ]
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
                |> List.map (fun (tag, _) -> Component.pathToLi "tags" <| sprintf "%s.html" tag)

            [ Html.ul [ prop.children [ Html.li [ Html.h2 "Tags" ]
                                        Html.ul [ prop.children tags ] ] ] ]

        let tagPageContens =
            tagAndPage
            |> Map.toList
            |> List.map (fun (tag, metas) ->
                let lis = metas |> List.map (metaToLi "posts")

                tag,
                [ Html.ul [ prop.children [ Html.li [ Html.h2 tag ]
                                            Html.ul lis ] ] ])

        tagsContent, tagPageContens


    let generateNavbar (title: string) =
        Html.ul [ Html.h1 [ prop.text title ]
                  Component.liA "/index.html" "Index"
                  Component.liA "/archives.html" "Archives"
                  Component.liA "/pages/about.html" "About Me"
                  Component.liA "/atom.xml" "RSS"
                  Component.liA "/tags.html" "Tags" ]

    let generate404 =
        [ Html.h1 [ prop.text "404 Page not found" ]
          Html.p [ prop.text "Sorry! The page you're looking for does not exist." ] ]

[<AutoOpen>]
module Page =
    let private readAndWrite navbar title source dist =
        promise {
            printfn "Rendering %s..." source
            let! m = IO.readFile source

            let fm, content =
                m
                |> Parser.parseMarkdownAsReactEl
                |> fun (fm, c) ->
                    let title =
                        match fm with
                        | Some fm -> sprintf "%s - %s" title fm.title
                        | None -> title

                    fm, frame navbar title c |> Parser.parseReactStatic


            printfn "Writing %s..." dist

            do! IO.writeFile dist content

            return
                { frontMatter = fm
                  layout = discriminateLayout source
                  source = source
                  dist = dist }
        }

    let renderMarkdowns navbar title sourceDir distDir =
        promise {
            let! files = getMarkdownFiles sourceDir
            let rw = readAndWrite navbar title

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

    let renderIndex navbar title metaPosts =
        let latest =
            metaPosts
            |> Seq.map (fun m -> m.source)
            |> getLatestPost

        promise {
            let rw = readAndWrite navbar title
            let dist = IO.resolve "docs/index.html"

            do! rw latest dist |> Promise.map ignore
        }

    let renderArchives navbar title metaPosts metaPages dist =
        promise {
            printfn "Rendering archives..."
            let! archives = generateArchives metaPosts metaPages

            let content =
                archives
                |> frame navbar (sprintf "%s - Archives" title)
                |> Parser.parseReactStatic

            printfn "Writing archives %s..." dist

            do! IO.writeFile dist content
        }

    let renderTags navbar title meta dist =
        let tagsContent, tagPageContents = generateTagsContent meta
        let frame = frame navbar

        promise {
            printfn "Rendering tags..."
            let title = (sprintf "%s - Tags" title)

            let content =
                tagsContent
                |> frame title
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
                        |> frame (sprintf "%s - %s" title tag)
                        |> Parser.parseReactStatic

                    IO.writeFile dist content |> Promise.map ignore)
                |> Promise.all
                |> Promise.map ignore
        }

    let render404 navbar title dist =
        promise {
            printfn "Rendering 404..."

            let content =
                generate404
                |> frame navbar (sprintf "%s - 404" title)
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
