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
    open System
    open Feliz
    open Parser

    let datesInYear year =
        let startDate =
            let d = DateTime(year, 1, 1)
            if d.DayOfWeek = DayOfWeek.Sunday then
                d
            else
                d.AddDays(-(2.0 - (d.DayOfWeek |> float)))
        let endDate =
            let d = DateTime(year, 12, 31)
            if d.DayOfWeek = DayOfWeek.Saturday then
                d
            else
                d.AddDays(2.0 - (d.DayOfWeek |> float))
        printfn "startDate: %A, endDate: %A" startDate endDate

        List.unfold
            (fun date ->
                if date > endDate then
                    None
                else
                    Some(date, date.AddDays(1.0)))
            startDate

    let generateCalendar (year: int) (logs: Booklog list) =
        let map = logs |> List.map (_.date >> DateTime.Parse) |> Set.ofList
        let days = datesInYear year |> List.groupBy _.DayOfWeek
        let calendar=
            days |> List.map (fun (dow, dates) ->
                dates
                |> List.groupBy id
                |> List.map (fun (_, logs) ->
                    logs |> List.mapi (fun i log -> (i,log))
                )
                |> List.concat
                |> List.map (fun (i, d) ->
                    let cls =
                        if Set.contains d map then
                            "log"
                        else if d.Year <> year then
                            "other-year"
                        else
                            "no-log"

                    Html.td [
                        prop.className cls
                        prop.children [
                            Html.a [
                                prop.href $"#{d |> DateTime.toRFC3339Date}-{i+1}"
                            ]
                        ]
                    ]
                )
                |> Html.tr
        )
        Html.div [
            prop.className "section calendar-container"
            prop.children [
                Html.table [
                    prop.className "calendar"
                    prop.children [
                        Html.thead [
                            Html.tr [
                            ]
                        ]
                        Html.tbody calendar
                    ] ]
            ]
        ]

    let generateBooklogLinks baseUrl years =
        let links =
            years
            |> List.map (fun year ->
                Html.li [
                    Html.a [ prop.href $"{baseUrl}/{year}.html"; prop.children [ Html.text (year |> string) ]]])

        Html.ul [ prop.className "booklog-links"; prop.children links ]

    let generateBooklogTable links (year: int) (logs: Booklog list) =
        let header = Html.h1 [ prop.className "title" ; prop.children (Html.text $"Booklog {year}") ]
        let booklogCalendar =  generateCalendar year logs
        let booklogRows =
            logs
            |> List.groupBy _.date
            |> List.map (fun (_, logs) ->
                logs |> List.mapi (fun i log -> (i,log))
            )
            |> List.concat
            |> List.map (fun (i, log) ->
                let tags =
                    log.tags
                    |> function
                        | Some ts -> ts |> List.ofSeq
                        | None -> []
                    |> String.concat ", "

                Html.div [
                    prop.className "section"
                    prop.children [
                        Html.h2 [
                            prop.id $"{log.date}-{i+1}"
                            prop.className "subtitle"
                            prop.children [
                                Html.text log.date
                                Html.small [
                                    Html.text log.bookTitle
                                    Html.text (log.readCount |> function | Some rc -> $" ({rc})" | None -> "")
                                    Html.text (
                                        log.previouslyRead
                                        |> function
                                        | Some pr -> if pr then " rereading " else ""
                                        | None -> "")
                                    Html.text log.pages
                                ]
                            ]]
                        Html.p [ Html.text (log.notes |> Option.defaultValue "")]
                    ]
                ])

        [ header; booklogCalendar; Html.div booklogRows; links; ]

    let groupBooklogs (booklogs: Booklog list) =
        let minYear = booklogs |> List.map (_.date >> DateTime.Parse >> _.Year) |> List.min
        let booklogPerYear = booklogs |> List.groupBy (_.date >> DateTime.Parse >> _.Year) |> Map.ofList
        (minYear, booklogPerYear)
