module Handler

open Browser.Dom
open Browser.WebStorage
open Fable.Core
open Fable.Core.JsInterop

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

type PagefindUI =
    [<Emit "new $0($1, $2)">]
    abstract Create: obj -> unit

[<Global>]
let PagefindUI: PagefindUI = jsNative

window.addEventListener (
    "DOMContentLoaded",
    (fun _ ->
        let elm = document.querySelector "#search"

        if isNull elm then
            ()
        else
            PagefindUI.Create(
                !!{| element = "#search"
                     baseUrl = "/blog-fable/"
                     pageSize = 5
                     translations =
                      !!{| placeholder = "Search"
                           clear_search = "Clear"
                           load_more = "More"
                           search_label = ""
                           zero_results = "\"[SEARCH_TERM]\" now found."
                           many_results = "\"[SEARCH_TERM]\" ([COUNT])"
                           one_result = "\"[SEARCH_TERM]\" ([COUNT])"
                           searching = "Searching \"[SEARCH_TERM]\"..." |} |}
            ))
)
