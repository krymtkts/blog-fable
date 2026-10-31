module SearchTests

open Expecto
open Microsoft.Playwright
open System.Threading.Tasks
open TestHelpers

let private filters =
    testTask "Pagefind filters classify archive and booklog results" {
        use! (server: DevServer) = DevServer.StartAsync()
        let baseUrl: string = $"http://localhost:%d{server.Port}%s{server.Root}"

        let! (playwright: IPlaywright) = Playwright.CreateAsync()
        use _ = PlaywrightAsyncDisposable playwright
        let! (page: IPage) = playwright.NewChromiumPage()

        let! response = $"%s{baseUrl}/index.html" |> page.GotoAndCheck

        match response with
        | Result.Error msg -> failwith $"%s{msg}"
        | _ -> ()

        let filtersTask: Task<string> =
            page.EvaluateAsync<string>
                """
                async () => {
                    const pagefind = await import("/blog-fable/pagefind/pagefind.js");
                    return JSON.stringify(await pagefind.filters());
                }
            """

        let! filters = filtersTask

        for expected: string in [ "\"section\""; "\"archive\""; "\"booklog\""; "\"tag\""; "\"sample\"" ] do
            if (filters: string).Contains expected |> not then
                failtestf "Pagefind filters did not contain %s: %s" expected filters

        let booklogUrlsTask: Task<string array> =
            page.EvaluateAsync<string array>
                """
                async () => {
                    const pagefind = await import("/blog-fable/pagefind/pagefind.js");
                    const search = await pagefind.search(null, {
                        filters: { section: "booklog" }
                    });
                    const data = await Promise.all(search.results.map(result => result.data()));
                    return data.map(item => item.url);
                }
            """

        let! booklogUrls = booklogUrlsTask

        if
            (booklogUrls: string array).Length = 0
            || booklogUrls
               |> Array.exists (fun (url: string) -> url.Contains("/booklogs/") |> not)
        then
            failtestf "Booklog filter returned unexpected URLs: %s" (String.concat ", " booklogUrls)

        let taggedArchiveUrlsTask: Task<string array> =
            page.EvaluateAsync<string array>
                """
                async () => {
                    const pagefind = await import("/blog-fable/pagefind/pagefind.js");
                    const search = await pagefind.search(null, {
                        filters: { section: "archive", tag: "sample" }
                    });
                    const data = await Promise.all(search.results.map(result => result.data()));
                    return data.map(item => item.url);
                }
            """

        let! taggedArchiveUrls = taggedArchiveUrlsTask

        if
            (taggedArchiveUrls: string array).Length = 0
            || taggedArchiveUrls
               |> Array.exists (fun (url: string) -> url.Contains("/booklogs/"))
        then
            failtestf "Tagged archive filter returned unexpected URLs: %s" (String.concat ", " taggedArchiveUrls)

        let! dropdownCount = page.Locator("pagefind-filter-dropdown").CountAsync()

        if dropdownCount <> 2 then
            failtestf "Expected two Pagefind filter dropdowns, but found %d" dropdownCount

        let modalTrigger = page.Locator("pagefind-modal-trigger .pf-trigger-btn")
        do! modalTrigger.WaitForAsync()
        do! modalTrigger.ClickAsync()

        let! modalScrollbarColor =
            page.Locator(".pf-modal-body").EvaluateAsync<string>("element => getComputedStyle(element).scrollbarColor")

        if modalScrollbarColor = "auto" then
            failtest "Pagefind modal scrollbar did not use the site theme"

        let tagDropdown = page.Locator("pagefind-filter-dropdown[filter='tag']")
        do! tagDropdown.WaitForAsync()

        let sectionDropdown = page.Locator("pagefind-filter-dropdown[filter='section']")
        let sectionTrigger = sectionDropdown.Locator(".pf-dropdown-trigger")
        do! sectionTrigger.WaitForAsync()
        do! sectionTrigger.ClickAsync()

        let! dropdownScrollbarColor =
            sectionDropdown
                .Locator(".pf-dropdown-options")
                .EvaluateAsync<string>("element => getComputedStyle(element).scrollbarColor")

        if dropdownScrollbarColor = "auto" then
            failtest "Pagefind filter scrollbar did not use the site theme"

        let! dropdownOverflow =
            sectionDropdown
                .Locator(".pf-dropdown-menu")
                .EvaluateAsync<string>("element => getComputedStyle(element).overflowY")

        if dropdownOverflow <> "visible" then
            failtestf "Pagefind filter menu should not be a second scroll container: %s" dropdownOverflow

        let booklogOption = sectionDropdown.Locator("[role='option'][data-value='booklog']")
        do! booklogOption.WaitForAsync()
        do! booklogOption.ClickAsync()

        let! sectionTriggerLabel = sectionTrigger.GetAttributeAsync("aria-label")

        match sectionTriggerLabel |> Option.ofObj with
        | Some label when label = "Section, 1 filter selected" -> ()
        | Some label -> failtestf "Unexpected selected section label: %s" label
        | None -> failtest "The selected section label was null"

        let hiddenOptions = LocatorWaitForOptions()
        hiddenOptions.State <- WaitForSelectorState.Hidden
        do! tagDropdown.WaitForAsync(hiddenOptions)
    }

let private tagAnd =
    testTask "Pagefind tag filters use AND semantics" {
        use! (server: DevServer) = DevServer.StartAsync()
        let baseUrl: string = $"http://localhost:%d{server.Port}%s{server.Root}"

        let! (playwright: IPlaywright) = Playwright.CreateAsync()
        use _ = PlaywrightAsyncDisposable playwright
        let! (page: IPage) = playwright.NewChromiumPage()

        let! response = $"%s{baseUrl}/index.html" |> page.GotoAndCheck

        match response with
        | Result.Error msg -> failwith $"%s{msg}"
        | _ -> ()

        let modalTrigger = page.Locator("pagefind-modal-trigger .pf-trigger-btn")
        do! modalTrigger.WaitForAsync()
        do! modalTrigger.ClickAsync()

        let tagDropdown = page.Locator("pagefind-filter-dropdown[filter='tag']")
        do! tagDropdown.WaitForAsync()

        let tagOptions = tagDropdown.Locator(".pf-dropdown-options")
        let attachedOptions = LocatorWaitForOptions()
        attachedOptions.State <- WaitForSelectorState.Attached
        do! tagOptions.WaitForAsync(attachedOptions)

        let! multiselectable = tagOptions.GetAttributeAsync("aria-multiselectable")

        match multiselectable |> Option.ofObj with
        | Some value when value = "true" -> ()
        | Some value -> failtestf "Tag filter should allow multiple selections: %s" value
        | None -> failtest "Tag filter did not expose aria-multiselectable"

        let tagTrigger = tagDropdown.Locator(".pf-dropdown-trigger")
        do! tagTrigger.ClickAsync()

        let sampleOption = tagDropdown.Locator("[role='option'][data-value='sample']")
        do! sampleOption.WaitForAsync()
        do! sampleOption.ClickAsync()

        let yamlOption = tagDropdown.Locator("[role='option'][data-value='yaml']")
        do! yamlOption.WaitForAsync()
        do! yamlOption.ClickAsync()

        let! selectedValues =
            page.EvaluateAsync<string array>
                """
                () => Array.from(document.querySelectorAll(
                    "pagefind-filter-dropdown[filter='tag'] [role='option'][aria-selected='true']"
                ))
                    .map(element => element.dataset.value)
                    .sort()
            """

        let expectedSelectedValues = [| "sample"; "yaml" |]

        if selectedValues <> expectedSelectedValues then
            failtestf "Tag filter did not keep both selections: %s" (String.concat ", " selectedValues)

        let! tagTriggerLabel = tagTrigger.GetAttributeAsync("aria-label")

        match tagTriggerLabel |> Option.ofObj with
        | Some label when label = "Tag, 2 filters selected" -> ()
        | Some label -> failtestf "Unexpected selected tag label: %s" label
        | None -> failtest "The selected tag label was null"

        let tagUrlsTask: Task<string array> =
            page.EvaluateAsync<string array>
                """
                async () => {
                    const pagefind = await import("/blog-fable/pagefind/pagefind.js");
                    const search = await pagefind.search(null, {
                        filters: { tag: ["sample", "yaml"] }
                    });
                    const data = await Promise.all(search.results.map(result => result.data()));
                    return data
                        .map(item => new URL(item.url, location.href).pathname)
                        .sort();
                }
            """

        let! tagUrls = tagUrlsTask

        let expectedTagUrls =
            [|
                "/blog-fable/pages/sampla-page.html"
                "/blog-fable/posts/2023-03-01-sample-post.html"
            |]

        if tagUrls <> expectedTagUrls then
            failtestf "Tag AND filter returned unexpected URLs: %s" (String.concat ", " tagUrls)
    }

let tests = testList "Search" [ filters; tagAnd ]
