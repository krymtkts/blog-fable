// ts2fable 0.7.1
module rec HighlightJs

open Fable.Core
open System.Text.RegularExpressions

[<ImportDefault("highlight.js")>]
let hljs: Hljs.IExports = jsNative

module Hljs =

    [<AllowNullLiteral>]
    type IExports =
        abstract highlight:
            name: string * value: string * ?ignore_illegals: bool * ?continuation: ICompiledMode -> IHighlightResult

        abstract highlightAuto: value: string * ?languageSubset: ResizeArray<string> -> IAutoHighlightResult
        abstract fixMarkup: value: string -> string
        abstract highlightBlock: block: Node -> unit
        abstract configure: options: IOptions -> unit
        abstract initHighlighting: unit -> unit
        abstract initHighlightingOnLoad: unit -> unit
        abstract registerLanguage: name: string * language: (HLJSStatic -> IModeBase) -> unit
        abstract listLanguages: unit -> ResizeArray<string>
        abstract getLanguage: name: string -> IMode
        abstract ``inherit``: parent: obj * obj: obj -> obj
        abstract COMMENT: ``begin``: U2<string, Regex> * ``end``: U2<string, Regex> * inherits: IModeBase -> IMode
        abstract IDENT_RE: string
        abstract UNDERSCORE_IDENT_RE: string
        abstract NUMBER_RE: string
        abstract C_NUMBER_RE: string
        abstract BINARY_NUMBER_RE: string
        abstract RE_STARTERS_RE: string
        abstract BACKSLASH_ESCAPE: IMode
        abstract APOS_STRING_MODE: IMode
        abstract QUOTE_STRING_MODE: IMode
        abstract PHRASAL_WORDS_MODE: IMode
        abstract C_LINE_COMMENT_MODE: IMode
        abstract C_BLOCK_COMMENT_MODE: IMode
        abstract HASH_COMMENT_MODE: IMode
        abstract NUMBER_MODE: IMode
        abstract C_NUMBER_MODE: IMode
        abstract BINARY_NUMBER_MODE: IMode
        abstract CSS_NUMBER_MODE: IMode
        abstract REGEX_MODE: IMode
        abstract TITLE_MODE: IMode
        abstract UNDERSCORE_TITLE_MODE: IMode

    [<AllowNullLiteral>]
    type Node =
        interface
        end

    [<AllowNullLiteral>]
    type IHighlightResultBase =
        abstract relevance: float with get, set
        abstract language: string with get, set
        abstract value: string with get, set

    [<AllowNullLiteral>]
    type IAutoHighlightResult =
        inherit IHighlightResultBase
        abstract second_best: IAutoHighlightResult option with get, set

    [<AllowNullLiteral>]
    type IHighlightResult =
        inherit IHighlightResultBase
        abstract top: ICompiledMode with get, set

    [<AllowNullLiteral>]
    type HLJSStatic =
        abstract ``inherit``: parent: obj * obj: obj -> obj
        abstract IDENT_RE: string with get, set
        abstract UNDERSCORE_IDENT_RE: string with get, set
        abstract NUMBER_RE: string with get, set
        abstract C_NUMBER_RE: string with get, set
        abstract BINARY_NUMBER_RE: string with get, set
        abstract RE_STARTERS_RE: string with get, set
        abstract BACKSLASH_ESCAPE: IMode with get, set
        abstract APOS_STRING_MODE: IMode with get, set
        abstract QUOTE_STRING_MODE: IMode with get, set
        abstract PHRASAL_WORDS_MODE: IMode with get, set
        abstract C_LINE_COMMENT_MODE: IMode with get, set
        abstract C_BLOCK_COMMENT_MODE: IMode with get, set
        abstract HASH_COMMENT_MODE: IMode with get, set
        abstract NUMBER_MODE: IMode with get, set
        abstract C_NUMBER_MODE: IMode with get, set
        abstract BINARY_NUMBER_MODE: IMode with get, set
        abstract CSS_NUMBER_MODE: IMode with get, set
        abstract REGEX_MODE: IMode with get, set
        abstract TITLE_MODE: IMode with get, set
        abstract UNDERSCORE_TITLE_MODE: IMode with get, set

    [<AllowNullLiteral>]
    type IModeBase =
        abstract className: string option with get, set
        abstract aliases: ResizeArray<string> option with get, set
        abstract ``begin``: U2<string, Regex> option with get, set
        abstract ``end``: U2<string, Regex> option with get, set
        abstract case_insensitive: bool option with get, set
        abstract beginKeyword: string option with get, set
        abstract endsWithParent: bool option with get, set
        abstract lexems: string option with get, set
        abstract illegal: string option with get, set
        abstract excludeBegin: bool option with get, set
        abstract excludeEnd: bool option with get, set
        abstract returnBegin: bool option with get, set
        abstract returnEnd: bool option with get, set
        abstract starts: string option with get, set
        abstract subLanguage: string option with get, set
        abstract subLanguageMode: string option with get, set
        abstract relevance: float option with get, set
        abstract variants: ResizeArray<IMode> option with get, set

    [<AllowNullLiteral>]
    type IMode =
        inherit IModeBase
        abstract keywords: obj option with get, set
        abstract contains: ResizeArray<IMode> option with get, set

    [<AllowNullLiteral>]
    type ICompiledMode =
        inherit IModeBase
        abstract compiled: bool with get, set
        abstract contains: ResizeArray<ICompiledMode> option with get, set
        abstract keywords: obj option with get, set
        abstract terminators: Regex with get, set
        abstract terminator_end: string option with get, set

    [<AllowNullLiteral>]
    type IOptions =
        abstract classPrefix: string option with get, set
        abstract tabReplace: string option with get, set
        abstract useBR: bool option with get, set
        abstract languages: ResizeArray<string> option with get, set
