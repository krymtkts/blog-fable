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
        let dateMap =
            logs
            |> List.map (_.date >> DateTime.Parse)
            |> List.groupBy id
            |> List.map (fun (d, logs) -> (d, logs |> List.length))
            |> Map.ofList

        let days = datesInYear year |> List.groupBy _.DayOfWeek
        let empty = Html.th []

        let header =
            days
            |> List.head
            |> snd
            |> List.map (fun d ->
                let startOfWeek = d
                let endOfWeek = d.AddDays(6)

                if startOfWeek.Day = 1 then
                    startOfWeek |> DateTime.toShortMonthName
                else if endOfWeek.Year <> year then
                    ""
                else if startOfWeek.Month <> endOfWeek.Month then
                    endOfWeek |> DateTime.toShortMonthName
                else
                    "")
            |> List.map (fun m -> Html.th [ prop.children [ Html.span m ] ])

        let calendar =
            days
            |> List.map (fun (dow, dates) ->
                let dowCell =
                    match dow with
                    | DayOfWeek.Sunday
                    | DayOfWeek.Wednesday
                    | DayOfWeek.Friday ->
                        Html.td [
                            prop.className "dow"
                            prop.children [ Html.span (dow |> DateTime.toShortDayName) ]
                        ]
                    | _ -> Html.td []

                let dowRows =
                    dates
                    |> List.map (fun d ->
                        let cls, anchor =
                            match Map.tryFind d dateMap with
                            | Some count ->
                                let date = d |> DateTime.toRFC3339Date
                                "log", [ Html.a [ prop.href $"#{date}-1"; prop.title $"{date} ({count})" ] ]
                            | None -> if d.Year <> year then "other-year", [] else "no-log", []

                        Html.td [ prop.className cls; prop.children anchor ])

                Html.tr [ prop.children (dowCell :: dowRows) ])

        Html.div [
            prop.className "section calendar-container"
            prop.children [
                Html.table [
                    prop.className "calendar"
                    prop.children [ Html.thead [ Html.tr (empty :: header) ]; Html.tbody calendar ]
                ]
            ]
        ]

    let generateBooklogLinks baseUrl years =
        let links =
            years
            |> List.map (fun year ->
                Html.li [
                    Html.a [
                        prop.href $"{baseUrl}/{year}.html"
                        prop.children [ Html.text (year |> string) ]
                    ]
                ])

        Html.ul [ prop.className "booklog-links"; prop.children links ]

    let generateBooklogTable links (year: int) (logs: Booklog list) =
        let header =
            Html.h1 [ prop.className "title"; prop.children (Html.text $"Booklog {year}") ]

        let booklogCalendar = generateCalendar year logs

        let booklogRows =
            logs
            |> List.groupBy _.date
            |> List.map (fun (_, logs) -> logs |> List.mapi (fun i log -> (i, log)))
            |> List.concat
            |> List.map (fun (i, log) ->
                // TODO: add tags to booklog.
                let tags =
                    log.tags
                    |> function
                        | Some ts -> ts |> List.ofSeq
                        | None -> []
                    |> String.concat ", "

                Html.div [
                    prop.className "section"
                    prop.children [
                        Html.p [
                            prop.id $"{log.date}-{i + 1}"
                            prop.className "title is-4"
                            prop.children [ Html.text log.date; Html.small [ Html.text $" - {log.bookTitle}" ] ]
                        ]
                        Html.p [
                            prop.className "subtitle is-6"
                            prop.children [
                                Html.text "page: "
                                Html.text log.pages
                                Html.text ", read count: "
                                Html.text (
                                    let rc =
                                        log.readCount
                                        |> function
                                            | Some rc -> rc
                                            | None -> 1

                                    log.previouslyRead
                                    |> function
                                        | Some pr when pr -> $"n+{rc}"
                                        | _ -> $"{rc}"
                                )
                            ]
                        ]
                        Html.p [ Html.text (log.notes |> Option.defaultValue "") ]
                    ]
                ])

        [ header; booklogCalendar; Html.div booklogRows; links ]

    let groupBooklogs (booklogs: Booklog list) =
        let minYear = booklogs |> List.map (_.date >> DateTime.Parse >> _.Year) |> List.min

        let booklogPerYear =
            booklogs |> List.groupBy (_.date >> DateTime.Parse >> _.Year) |> Map.ofList

        (minYear, booklogPerYear)
