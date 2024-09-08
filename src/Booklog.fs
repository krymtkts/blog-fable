module Booklog

open Common

module Parser =
    open Yaml

    type Booklog =
        abstract date: string
        abstract bookTitle: string
        abstract readCount: int option
        abstract pages: string
        abstract notes: string option

    type Book =
        abstract id: string
        abstract bookTitle: string
        abstract bookAuthor: string
        abstract previouslyRead: bool option

    let parseBooklogs (str: string) =
        // NOTE: requires to define as ResizeArray to convert from raw JavaScript array.
        let bs: Booklog ResizeArray = Yaml.parse str

        bs |> List.ofSeq

    let parseBooks (str: string) =
        // NOTE: requires to define as ResizeArray to convert from raw JavaScript array.
        let bs: Book ResizeArray = Yaml.parse str

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
                                "log", [ Html.a [ prop.href $"#{date}-{count}"; prop.title $"{date} ({count})" ] ]
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

    // TODO: refactor
    let generateBookLinks baseUrl (books: Book list) =
        let links =
            books
            |> List.map (fun book ->
                Html.li [
                    Html.a [
                        prop.href $"{baseUrl}/{book.id}.html"
                        prop.children [ Html.text (book.bookTitle) ]
                    ]
                ])

        Html.ul [ prop.className "book-links"; prop.children links ]

    let generateBooklogTable links (books: Map<string, Book>) (year: int) (logs: Booklog list) =
        let header =
            Html.h1 [ prop.className "title"; prop.children (Html.text $"Booklog {year}") ]

        let booklogCalendar = generateCalendar year logs

        let booklogRows =
            logs
            |> List.groupBy _.date
            |> List.map (fun (_, logs) -> logs |> List.mapi (fun i log -> (i, log)))
            |> List.concat
            |> List.rev
            |> List.map (fun (i, log) ->
                let notes =
                    log.notes
                    |> function
                        | Some notes -> Html.p [ prop.dangerouslySetInnerHTML (parseMarkdown notes) ]
                        | None -> Html.p []

                let getPreviouslyRead bookTitle =
                    Map.tryFind bookTitle books
                    |> function
                        | Some book -> book.previouslyRead
                        | None -> None

                Html.div [
                    prop.className "section"
                    prop.children [
                        Html.p [
                            prop.id $"{log.date}-{i + 1}"
                            prop.className "title is-4"
                            prop.children [ Html.text log.date ]
                        ]
                        Html.p [
                            prop.className "subtitle is-6"
                            prop.children [
                                Html.text $"{log.bookTitle}"
                                Html.text ", read count: "
                                Html.text (
                                    let rc =
                                        log.readCount
                                        |> function
                                            | Some rc -> rc
                                            | None -> 1

                                    log.bookTitle
                                    |> getPreviouslyRead
                                    |> function
                                        | Some pr when pr -> $"n+{rc}"
                                        | _ -> $"{rc}"
                                )
                                Html.br []
                                Html.text "page: "
                                Html.text log.pages

                            ]
                        ]
                        notes
                    ]
                ])

        [ header; booklogCalendar; Html.div booklogRows; links ]

    let generateBooklogSummary links (book: Book) (logs: Booklog list) =
        let header =
            Html.h1 [
                prop.className "title"
                prop.children (Html.text $"Booklog - {book.bookTitle}")
            ]

        let booklogRows =
            logs
            |> List.filter (_.notes >> Option.isSome)
            |> List.map (fun (log) ->
                let notes =
                    log.notes
                    |> function
                        | Some notes -> Html.p [ prop.dangerouslySetInnerHTML (parseMarkdown notes) ]
                        | None -> Html.p []

                [ notes
                  Html.p [
                      prop.className "content is-small booklog-info"
                      prop.children [
                          Html.text log.date
                          Html.text ", read count: "
                          Html.text (
                              let rc =
                                  log.readCount
                                  |> function
                                      | Some rc -> rc
                                      | None -> 1

                              book.previouslyRead
                              |> function
                                  | Some pr when pr -> $"n+{rc}"
                                  | _ -> $"{rc}"
                          )
                          Html.text ", page: "
                          Html.text log.pages
                      ]
                  ] ])


        [ header
          Html.div [ prop.className "section"; prop.children (booklogRows |> List.concat) ]
          links ]

    let groupBooklogsByYear (booklogs: Booklog list) =
        let minYear = booklogs |> List.map (_.date >> DateTime.Parse >> _.Year) |> List.min

        let booklogPerYear =
            booklogs |> List.groupBy (_.date >> DateTime.Parse >> _.Year) |> Map.ofList

        (minYear, booklogPerYear)

    let groupBooklogsByTitle (booklogs: Booklog list) =
        booklogs |> List.groupBy (_.bookTitle) |> Map.ofList

    let getBookMap (books: Book list) =
        books |> List.map (fun book -> book.bookTitle, book) |> Map.ofList

    type BooklogDef =
        { priority: string
          basePath: string
          links: Fable.React.ReactElement
          books: Map<string, Book>
          year: int }

    let parseBooklogTable (conf: FrameConfiguration) (def: BooklogDef) (booklogs: Booklog list) =
        let content =
            booklogs
            |> generateBooklogTable def.links def.books def.year
            |> frame
                { conf with
                    title = $"%s{conf.title} - %d{def.year}"
                    url = $"%s{conf.url}%s{def.basePath}" }
            |> Parser.parseReactStaticHtml

        let lastmod = booklogs |> List.maxBy _.date |> _.date

        let loc: Xml.SiteLocation =
            { loc = sourceToSitemap def.basePath (def.year |> string)
              lastmod = lastmod
              priority = def.priority }

        content, loc, def.year

    type BookDef =
        { priority: string
          basePath: string
          links: Fable.React.ReactElement
          book: Book }

    // TODO; refactor
    let parseBooklogSummary (conf: FrameConfiguration) (def: BookDef) (booklogs: Booklog list) =
        let content =
            booklogs
            |> generateBooklogSummary def.links def.book
            |> frame
                { conf with
                    title = $"%s{conf.title} - %s{def.book.bookTitle}"
                    url = $"%s{conf.url}%s{def.basePath}" }
            |> Parser.parseReactStaticHtml

        let lastmod = booklogs |> List.maxBy _.date |> _.date

        let loc: Xml.SiteLocation =
            { loc = sourceToSitemap def.basePath def.book.id
              lastmod = lastmod
              priority = def.priority }

        content, loc, def.book.id
