module ThemeTests

open Expecto
open Microsoft.Playwright
open TestHelpers

let private selectionPersists =
    testTask "Theme selection persists across reloads and system mode follows the browser preference" {
        use! (server: DevServer) = DevServer.StartAsync()
        use! (browser: TestBrowser) = TestBrowser.StartAsync()
        let page = browser.Page

        let! response = page.GotoAndCheck $"%s{server.BaseUrl}/index.html"

        match response with
        | Result.Error message -> failtest message
        | _ -> ()

        do! page.WaitForLoadStateAsync(LoadState.Load)
        let root = page.Locator("html")

        let assertTheme (systemTheme: string) (mode: string option) =
            task {
                match mode with
                | Some theme -> do! Assertions.Expect(root).ToHaveAttributeAsync("data-theme", theme)
                | None -> do! Assertions.Expect(page.Locator("html[data-theme]")).ToHaveCountAsync(0)

                let! storedTheme =
                    page.EvaluateAsync<string | null>("localStorage.getItem('theme-mode')")

                Expect.equal (Option.ofObj storedTheme) mode "The stored theme should match the selection"

                do!
                    Assertions
                        .Expect(root)
                        .ToHaveCSSAsync("--pf-checkbox-color-scheme", mode |> Option.defaultValue systemTheme)
            }

        for preference, systemTheme in [ ColorScheme.Light, "light"; ColorScheme.Dark, "dark" ] do
            do! page.EmulateMediaAsync(PageEmulateMediaOptions(ColorScheme = preference))
            do! assertTheme systemTheme None

            for title, mode in
                [
                    "Light theme", Some "light"
                    "Dark theme", Some "dark"
                    "System Default", None
                ] do
                do! page.GetByTitle(title, PageGetByTitleOptions(Exact = true)).ClickAsync()
                do! assertTheme systemTheme mode
                let! _ = page.ReloadAsync()
                do! assertTheme systemTheme mode
    }

let tests = testList "Theme" [ selectionPersists ]
