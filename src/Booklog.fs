module Booklog

open Common

module Parser =
    open Yaml

    type Booklog =
        abstract date: string
        abstract bookTitle: string
        abstract bookAuthor: string
        abstract readCount: int option
        abstract previouslyRead: bool option
        abstract pages: string
        abstract tags: string ResizeArray option
        abstract notes: string option

    let parseBooklogs (str: string) =
        // NOTE: requires to define as ResizeArray to convert from raw JavaScript array.
        let bs: Booklog ResizeArray = Yaml.parse str

        bs |> List.ofSeq


[<AutoOpen>]
module Misc =
    open Feliz
    open Parser

    // TODO: sample implementation. generating HTML table from Booklog list.
    let generateBooklogTable (logs: Booklog list) =
        let rows =
            logs
            |> List.map (fun log ->
                let tags =
                    log.tags
                    |> function
                        | Some ts -> ts |> List.ofSeq
                        | None -> []
                    |> String.concat ", "

                Html.tr [ Html.td [ Html.text log.date ]
                          Html.td [ Html.text log.bookTitle ]
                          Html.td [ Html.text log.bookAuthor ]
                          Html.td [ Html.text $"read count: {log.readCount |> Option.defaultValue 0}" ]
                          Html.td [ Html.text $"previously read: {log.previouslyRead |> Option.defaultValue false}" ]
                          Html.td [ Html.text $"pages: {log.pages}" ]
                          Html.td [ Html.text tags ]
                          Html.td [ Html.text (log.notes |> Option.defaultValue "") ] ])

        let table =
            Html.table [ Html.thead [ Html.tr [ Html.th [ Html.text "Date" ]
                                                Html.th [ Html.text "Title" ]
                                                Html.th [ Html.text "Author" ]
                                                Html.th [ Html.text "Read Count" ]
                                                Html.th [ Html.text "Previously Read" ]
                                                Html.th [ Html.text "Pages" ]
                                                Html.th [ Html.text "Tags" ]
                                                Html.th [ Html.text "Notes" ] ] ]
                         Html.tbody rows ]

        [ table ]
