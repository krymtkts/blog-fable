module BooklogTests

open Expecto
open Microsoft.Playwright
open TestHelpers

let private fixedNavigation =
    testTask "Booklog fixed navigation jumps to its top and bottom" {
        use! (server: DevServer) = DevServer.StartAsync()
        let baseUrl = $"http://localhost:%d{server.Port}%s{server.Root}"
        let! (playwright: IPlaywright) = Playwright.CreateAsync()
        use _ = PlaywrightAsyncDisposable playwright
        let! (page: IPage) = playwright.NewChromiumPage()

        for width in [ 1280; 375 ] do
            do! page.SetViewportSizeAsync(width, 800)

            for path in [ "/booklogs.html"; "/booklogs/2023.html"; "/booklogs/a-book.html" ] do
                let! response = page.GotoAndCheck(baseUrl + path)

                match response with
                | Result.Error message -> failtest message
                | _ -> ()

                for selector in [ "#booklog-top"; "#booklog-bottom"; ".booklog-navigation" ] do
                    let! count = page.Locator(selector).CountAsync()

                    if count <> 1 then
                        failtestf "%s should have exactly one %s" path selector

                let navigation = page.Locator(".booklog-navigation")

                let! aligned =
                    page.EvaluateAsync<bool>
                        """
                        () => {
                            const nav = document.querySelector('.booklog-navigation');
                            const main = document.querySelector('main.container');
                            const section = document.querySelector('.booklog-page > .section');
                            const lastLink = nav.querySelector('a:last-child');
                            const padding = parseFloat(getComputedStyle(section).paddingRight);
                            const expectedRight = main.getBoundingClientRect().right - padding;
                            const rect = lastLink.getBoundingClientRect();
                            return Math.abs(rect.right - expectedRight) <= 1
                                && getComputedStyle(nav).pointerEvents === 'none'
                                && getComputedStyle(lastLink).pointerEvents === 'auto';
                        }
                        """

                if not aligned then
                    failtestf "Navigation should align with the content and allow click-through on %s" path

                let! (before: LocatorBoundingBoxResult | null) = navigation.BoundingBoxAsync()

                let before =
                    before
                    |> Option.ofObj
                    |> Option.defaultWith (fun () -> failwith "Navigation is not visible")

                do!
                    navigation
                        .GetByRole(AriaRole.Link, LocatorGetByRoleOptions(Name = "Go to bottom", Exact = true))
                        .ClickAsync()

                do!
                    page.WaitForFunctionAsync(
                        "location.hash === '#booklog-bottom' && document.querySelector('#booklog-bottom').getBoundingClientRect().top <= innerHeight"
                    )
                    |> Async.AwaitTask
                    |> Async.Ignore

                let! (after: LocatorBoundingBoxResult | null) = navigation.BoundingBoxAsync()

                let after =
                    after
                    |> Option.ofObj
                    |> Option.defaultWith (fun () -> failwith "Navigation is not visible")

                if abs (before.Y - after.Y) > 1.0f || abs (before.X - after.X) > 1.0f then
                    failtestf "Navigation moved when scrolling %s" path

                do!
                    navigation
                        .GetByRole(AriaRole.Link, LocatorGetByRoleOptions(Name = "Go to top", Exact = true))
                        .ClickAsync()

                do!
                    page.WaitForFunctionAsync(
                        "location.hash === '#booklog-top' && Math.abs(document.querySelector('#booklog-top').getBoundingClientRect().top) <= 1"
                    )
                    |> Async.AwaitTask
                    |> Async.Ignore

        for path in [ "/index.html"; "/pages/about.html"; "/posts/2023-03-01-sample-post.html" ] do
            let! _ = page.GotoAndCheck(baseUrl + path)
            let! count = page.Locator(".booklog-navigation").CountAsync()

            if count <> 0 then
                failtestf "Fixed booklog navigation should not appear on %s" path
    }

let tests = testList "Booklog" [ fixedNavigation ]
