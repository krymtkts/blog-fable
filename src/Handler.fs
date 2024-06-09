module Handler

open Browser.Dom
open Browser.WebStorage

// NOTE: Don't use Fable library in this file because it is directly bundled int HTML files.
//       This includes, discriminated union, List, etc.

let private themeKey = "theme-mode"
let private themeAttributeName = "data-theme"

let private setThemeMode (t: string) =
    match t with
    | "light"
    | "dark" ->
        localStorage.setItem (themeKey, t)
        document.documentElement.setAttribute (themeAttributeName, t)
    | _ ->
        localStorage.removeItem themeKey
        document.documentElement.removeAttribute themeAttributeName

localStorage.getItem "theme-mode" |> setThemeMode

let private initThemeModeHandler _ =
    let els = document.querySelectorAll (".theme-toggle")

    for i = 0 to els.length - 1 do
        let el = els.item (i)
        let themeMode = el.getAttribute "data-theme"
        el.addEventListener ("click", (fun _ -> setThemeMode themeMode))

window.addEventListener ("load", initThemeModeHandler)
