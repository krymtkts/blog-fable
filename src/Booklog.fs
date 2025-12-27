module Booklog

open System
open Common

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

type Streak =
    { startDate: DateTime
      endDate: DateTime
      count: int }

type StreakSummary =
    { longest: Streak option
      current: Streak option
      maxPagesRead: int }

[<RequireQualifiedAccess>]
module Parser =
    open Yaml

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
    open Feliz

    let private datesInYear year =
        let startDate =
            let d = DateTime(year, 1, 1)

            if d.DayOfWeek = DayOfWeek.Sunday then
                d
            else
                let toSunday = d.DayOfWeek |> float
                d.AddDays(-toSunday)

        let endDate =
            let d = DateTime(year, 12, 31)

            if d.DayOfWeek = DayOfWeek.Saturday then
                d
            else
                let toSaturday = DayOfWeek.Saturday - d.DayOfWeek |> float
                d.AddDays(toSaturday)

        printfn "startDate: %A, endDate: %A" startDate endDate

        List.unfold
            (fun date ->
                if date > endDate then
                    None
                else
                    Some(date, date.AddDays(1.0)))
            startDate

    let private generateYearLink baseUrl year (className: string) =
        Html.a [
            prop.className className
            prop.href $"{baseUrl}/{year}.html"
            prop.children [ Html.span [ Html.text (string year) ] ]
        ]

    let private generateCalendar
        (year: int)
        (prevYear: int option)
        (nextYear: int option)
        basePath
        (logs: Booklog list)
        =
        let dateMap =
            logs
            |> List.map (_.date >> DateTime.Parse)
            |> List.groupBy id
            |> List.map (fun (d, logs) -> d, logs |> List.length)
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
                    | DayOfWeek.Monday
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
                    prop.children [
                        Html.thead [
                            Html.tr [
                                Html.th [
                                    prop.className "nav"
                                    prop.colSpan (List.length header + 1)
                                    prop.children [
                                        prevYear
                                        |> function
                                            | Some year -> generateYearLink basePath year "prev"
                                            | None -> Html.span []
                                        nextYear
                                        |> function
                                            | Some year -> generateYearLink basePath year "next"
                                            | None -> Html.span []
                                    ]
                                ]
                            ]
                            Html.tr (empty :: header)
                        ]
                        Html.tbody calendar
                    ]
                ]
            ]
        ]

    let private (|Int|_|) (str: string) =
        match System.Int32.TryParse str with
        | true, int -> Some int
        | _ -> None

    let private romanMap = Map [ 'i', 1; 'v', 5; 'x', 10 ]

    let private toInt x =
        x
        |> Char.ToLower
        |> romanMap.TryFind
        |> function
            | Some v -> v
            | None -> 0

    let private romanToArabic (roman: string) =
        let roman = roman |> _.Trim()

        roman
        |> Seq.map toInt
        |> Seq.pairwise
        |> Seq.fold (fun acc (v1, v2) -> if v1 < v2 then acc - v1 else acc + v1) 0
        |> (+) (Seq.last roman |> toInt)

    let private (|Roman|_|) (str: string) =
        match romanToArabic str with
        | 0 -> None
        | i -> Some i

    let readPages (pages: string) =
        pages
        |> _.Split([| ',' |])
        |> Seq.map _.Split([| '~' |])
        |> Seq.map (function
            | [| Int s; Int e |] -> e - s + 1
            | [| Int _ |] -> 1
            | [| Roman s; Roman e |] -> e - s + 1
            | [| Roman _ |] -> 1
            | _ -> 0)
        |> Seq.sum

    let private getMaxPagesRead (booklogs: Booklog list) =
        booklogs |> List.map (_.pages >> readPages) |> List.max

    let private getStreakSummary (booklogs: Booklog list) =
        let current, longest =
            booklogs
            |> List.map (_.date >> DateTime.Parse)
            |> List.groupBy id
            |> List.map fst
            |> List.sortBy id
            |> List.fold
                (fun (current, longest) dt ->
                    let current =
                        match current with
                        | Some current when dt = current.endDate.AddDays(1) ->
                            { current with
                                endDate = dt
                                count = current.count + 1 }
                        | Some _
                        | None ->
                            { startDate = dt
                              endDate = dt
                              count = 1 }

                    let longest =
                        match longest with
                        | Some longest -> if current.count > longest.count then current else longest
                        | None -> current

                    Some current, Some longest)
                (None, None)

        let maxPagesRead = getMaxPagesRead booklogs

        { longest = longest
          current = current
          maxPagesRead = maxPagesRead }

    let generateBooklogStats (booklogs: Booklog list) =
        let { longest = longest
              current = current
              maxPagesRead = maxPagesRead } =
            booklogs |> getStreakSummary

        let current, longest =
            match current, longest with
            | Some current, Some longest -> current.count, longest.count
            | _ -> 0, 0 // NOTE: Neither current nor longest will ever be None individually.

        Html.div [
            prop.className "streak"
            prop.children [
                Html.text $"current: {current}, longest: {longest}, max pages read: {maxPagesRead}"
            ]
        ]

    let private generateLink (href: 'T -> string) (children: 'T -> ReactElement list) (x: 'T) =
        Html.a [ prop.href (href x); prop.children (children x) ]

    let private generateLinks (className: string) (href: 'T -> string) (children: 'T -> ReactElement list) pages =
        let links = pages |> List.map (fun x -> Html.li [ generateLink href children x ])

        Html.ul [ prop.className className; prop.children links ]

    let generateBooklogLinks baseUrl years =
        generateLinks
            "booklog-links"
            (fun year -> $"{baseUrl}/{year}.html")
            (fun year -> [ Html.text (string year) ])
            years

    let bookIdLink (baseUrl: string) (book: Book) = $"{baseUrl}/{book.id}.html"
    let bookTitleText (book: Book) = [ Html.text book.bookTitle ]

    let private generateBookLink baseUrl (book: Book) =
        generateLink (bookIdLink baseUrl) bookTitleText book

    let private tryGetDateRange (logs: Booklog list) =
        match logs with
        | [] -> None
        | _ ->
            let startDate, endDate =
                logs
                |> List.map (_.date >> DateTime.Parse)
                |> List.fold (fun (minD, maxD) d -> min d minD, max d maxD) (DateTime.MaxValue, DateTime.MinValue)
                |> fun (startDate, endDate) -> DateTime.toRFC3339Date startDate, DateTime.toRFC3339Date endDate

            Some(startDate, endDate)

    let generateBookLinks baseUrl (booklogsByTitle: Map<string, Booklog list>) (books: Book list) =
        let links =
            books
            |> List.map (fun (book: Book) ->
                let rangeText =
                    booklogsByTitle
                    |> Map.tryFind book.bookTitle
                    |> Option.bind tryGetDateRange
                    |> Option.map (fun (s, e) -> $"{s}〜{e}")

                Html.li [
                    prop.className "book-link"
                    prop.children [
                        generateLink (bookIdLink baseUrl) bookTitleText book
                        match rangeText with
                        | Some t -> Html.span [ prop.className "content is-small"; prop.children [ Html.text t ] ]
                        | None -> Html.none
                    ]
                ])

        Html.ul [ prop.className "book-links"; prop.children links ]

    let generateBooklogNotes (notes: string option) =
        notes
        |> function
            | Some notes -> Html.p [ prop.dangerouslySetInnerHTML (Parser.parseMarkdown notes) ]
            | None -> Html.p []

    let private countReadPages (pages: string) =
        pages
        |> readPages
        |> function
            | 0 -> ""
            | i -> $", pages read: {i}"

    type BooklogDef =
        { priority: string
          basePath: string
          links: Fable.React.ReactElement
          books: Map<string, Book>
          year: int
          prevYear: int option
          nextYear: int option
          stats: Fable.React.ReactElement
          index: bool }

    let private generateBooklogList (def: BooklogDef) (logs: Booklog list) =
        let header =
            Html.h1 [ prop.className "title"; prop.children (Html.text $"Booklog {def.year}") ]

        let booklogCalendar =
            generateCalendar def.year def.prevYear def.nextYear def.basePath logs

        let booklogRows =
            logs
            |> List.groupBy _.date
            |> List.map (fun (_, logs) -> logs |> List.mapi (fun i log -> i, log))
            |> List.concat
            |> List.rev
            |> List.map (fun (i, log) ->
                let notes = log.notes |> generateBooklogNotes
                let book = Map.tryFind log.bookTitle def.books

                Html.div [
                    prop.className "section"
                    prop.children [
                        Html.p [
                            prop.id $"{log.date}-{i + 1}"
                            prop.className "title is-4"
                            prop.children [ Html.text log.date ]
                        ]
                        Html.p [
                            prop.className "subtitle content is-small"
                            prop.children [
                                match book with
                                | Some book -> book |> generateBookLink def.basePath
                                | None -> Html.text log.bookTitle
                                Html.text ", read count: "
                                Html.text (
                                    let rc =
                                        log.readCount
                                        |> function
                                            | Some rc -> rc
                                            | None -> 1

                                    book
                                    |> function
                                        | Some book -> book.previouslyRead
                                        | _ -> None
                                    |> function
                                        | Some pr when pr -> $"n+{rc}"
                                        | _ -> $"{rc}"
                                )
                                Html.text ", page: "
                                Html.text log.pages
                                log.pages |> countReadPages |> Html.text
                            ]
                        ]
                        notes
                    ]
                ])

        let booksOfYear =
            logs
            |> List.map _.bookTitle
            |> List.distinct
            |> List.choose (fun bookTitle -> Map.tryFind bookTitle def.books)

        let booklogsByTitleInYear = logs |> List.groupBy _.bookTitle |> Map.ofList

        let booksOfYearLinks =
            booksOfYear |> generateBookLinks def.basePath booklogsByTitleInYear

        [ header
          def.stats
          booklogCalendar
          Html.div booklogRows
          Html.ul [
              Html.h2 $"Books of %d{def.year} (%d{booksOfYear |> List.length})"
              booksOfYearLinks
          ]
          def.links ]

    let private generateBooklogSummary links (book: Book) (logs: Booklog list) =
        let header =
            Html.h1 [
                prop.className "title"
                prop.children (Html.text $"Booklog - {book.bookTitle}")
            ]

        let bookAuthor =
            Html.p [ prop.className "subtitle"; prop.children [ Html.text book.bookAuthor ] ]

        let booklogRows =
            logs
            |> List.map (fun log ->
                [ log.notes |> generateBooklogNotes
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
                          log.pages |> countReadPages |> Html.text
                      ]
                  ] ])


        [ header
          bookAuthor
          Html.div [ prop.className "section"; prop.children (booklogRows |> List.concat) ]
          links ]

    let groupBooklogsByYear (booklogs: Booklog list) =
        let minYear = booklogs |> List.map (_.date >> DateTime.Parse >> _.Year) |> List.min

        let booklogPerYear =
            booklogs |> List.groupBy (_.date >> DateTime.Parse >> _.Year) |> Map.ofList

        minYear, booklogPerYear

    let groupBooklogsByTitle (booklogs: Booklog list) =
        booklogs |> List.groupBy _.bookTitle |> Map.ofList

    let getBookMap (books: Book list) =
        books |> List.map (fun book -> book.bookTitle, book) |> Map.ofList

    let inline private parseBooklog<'D, 'T
        when 'D: (member basePath: string) and 'D: (member priority: string) and 'T: (member date: string)>
        (conf: FrameConfiguration)
        (def: 'D)
        (getId: 'D -> string)
        (generate: 'D -> 'T list -> Fable.React.ReactElement list)
        (primary: 'D -> bool)
        (booklogs: 'T list)
        =
        let id = getId def

        let conf =
            if primary def then
                { conf with
                    title = $"%s{conf.title} - %s{id}"
                    url = $"%s{conf.url}%s{def.basePath}.html" }
            else
                { conf with
                    title = $"%s{conf.title} - %s{id}"
                    url = $"%s{conf.url}%s{def.basePath}/%s{id}.html" }

        let content = booklogs |> generate def |> frame conf |> Parser.parseReactStaticHtml

        let lastmod = booklogs |> List.maxBy _.date |> _.date

        let loc: Xml.SiteLocation =
            { loc = sourceToSitemap def.basePath id
              lastmod = lastmod
              priority = def.priority }

        content, loc, id

    let generateYearlyBooklogContent (conf: FrameConfiguration) (def: BooklogDef) (booklogs: Booklog list) =
        parseBooklog
            conf
            def
            (fun def -> def.year |> string)
            (fun def -> generateBooklogList def)
            (fun def -> def.index)
            booklogs

    type BookDef =
        { priority: string
          basePath: string
          links: Fable.React.ReactElement
          book: Book }

    let generateBooklogSummaryContent (conf: FrameConfiguration) (def: BookDef) (booklogs: Booklog list) =
        parseBooklog
            conf
            def
            (fun def -> def.book.id)
            (fun def -> generateBooklogSummary def.links def.book)
            (fun _ -> false)
            booklogs
