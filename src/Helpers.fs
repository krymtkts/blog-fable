module StaticWebGenerator

// open System.Text.RegularExpressions
open Fable.Core.JsInterop
open Fable.Core
// open Fable.Helpers.React
// open Fable.Helpers.React.Props

module Node = Node.Api

// module private Util =
//   let highlight: obj = importAll "highlight.js"
//   let marked: obj = importDefault "marked"

//   marked?setOptions(createObj [
//     "highlight" ==> fun code lang ->
//       highlight?highlightAuto(code, [|lang|])?value
//   ])

//   let renderer = createNew marked?Renderer ()

//   renderer?heading <- fun (text: string) level ->
//     let escapedText = Regex.Replace(text.ToLower(), @"[^\w]+", "-")
//     sprintf """<h%s><a name="%s" class="anchor" href="#%s">%s</a></h%s>"""
//       level escapedText escapedText text level

//   renderer?link <- fun href title text ->
//     sprintf """<a href="%s">%s</a>"""
//       (Regex.Replace(href, @"\.md\b", ".html")) text

//   let parseMarkdown(content: string): string =
//     marked $ (content, createObj ["renderer" ==> renderer])

// /// Parses a markdown file
// let parseMarkdownFile (path: string) =
//     Node.fs.readFileSync(path).toString() |> Util.parseMarkdown

// /// Parses a markdown string
// let parseMarkdown (str: string) =
//     Util.parseMarkdown str

// let parseMarkdownAsReactEl className (content: string) =
//     div [
//       Class className
//       DangerouslySetInnerHTML { __html = parseMarkdown content }
//     ] []

// /// Parses a React element invoking ReactDOMServer.renderToString
// let parseReact (el: React.ReactElement) =
//     ReactDomServer.renderToString el

// /// Parses a React element invoking ReactDOMServer.renderToStaticMarkup
// let parseReactStatic (el: React.ReactElement) =
//     ReactDomServer.renderToStaticMarkup el

module IO =
    type IJsLib =
        abstract getDirName: unit -> string

    [<ImportAll("../js/path.js")>]
    let jsLib: IJsLib = jsNative

    let private fsExtra: obj = importAll "fs-extra"

    let rec private ensureDirExists (dir: string) (cont: (unit -> unit) option) =
        if Node.fs.existsSync (!^dir) then
            match cont with
            | Some c -> c ()
            | None -> ()
        else
            ensureDirExists
                (Node.path.dirname dir)
                (Some (fun () ->
                    if not (Node.fs.existsSync !^dir) then
                        Node.fs.mkdirSync dir |> ignore

                    match cont with
                    | Some c -> c ()
                    | None -> ()))

    /// Resolves a path using the location of the target JS file
    /// Note the function is inline so `__dirname` will belong to the calling file
    let inline resolve (path: string) =
        Node.path.resolve (jsLib.getDirName (), path)

    let writeFile (path: string) (content: string) =
        ensureDirExists (Node.path.dirname path) None
        Node.fs.writeFileSync (path, content)

    // let readFile (path: string) = Node.fs.readFileSync(path).toString ()

    /// Copy a file or directory. The directory can have contents. Like cp -r.
    /// Overwrites target files
    let copy (source: string) (target: string) : unit =
        fsExtra?copySync (source, target, createObj["overwrite" ==> true])
