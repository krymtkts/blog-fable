// based on ts2fable 0.7.1 generation.
module rec HighlightJs

open Fable.Core
open Browser.Types

type Error = System.Exception
type RegExp = System.Text.RegularExpressions.Regex

[<ImportDefault("highlight.js")>]
let hljs: HighlightJs.IExports = jsNative

module HighlightJs =
    type IExports =
        inherit HLJSApi
    // NOTE: omit ModesAPI
    // inherit ModesAPI

    type HLJSApi =
        inherit PublicApi

    [<AllowNullLiteral>]
    type VuePlugin =
        abstract install: (obj option -> unit) with get, set

    [<AllowNullLiteral>]
    type RegexEitherOptions =
        abstract capture: bool option with get, set

    [<AllowNullLiteral>]
    type PublicApi =
        abstract highlight: string -> U2<string, HighlightOptions> -> bool -> HighlightResult
        abstract highlightAuto: string -> ResizeArray<string> -> AutoHighlightResult
        abstract highlightBlock: HTMLElement -> unit
        abstract highlightElement: HTMLElement -> unit
        abstract configure: obj -> unit
        abstract initHighlighting: unit -> unit
        abstract initHighlightingOnLoad: unit -> unit
        abstract highlightAll: unit -> unit
        abstract registerLanguage: string -> LanguageFn -> unit
        abstract unregisterLanguage: string -> unit
        abstract listLanguages: unit -> ResizeArray<string>
        abstract registerAliases: U2<string, ResizeArray<string>> -> PublicApiRegisterAliases -> unit
        abstract getLanguage: string -> Language option
        abstract autoDetection: string -> bool
        // abstract ``inherit``: ('T -> ResizeArray<Record<string, obj option>> -> 'T) with get, set
        abstract addPlugin: (HLJSPlugin -> unit) with get, set
        abstract removePlugin: (HLJSPlugin -> unit) with get, set
        abstract debugMode: (unit -> unit) with get, set
        abstract safeMode: (unit -> unit) with get, set
        abstract versionString: string with get, set
        abstract vuePlugin: (unit -> VuePlugin) with get, set
        abstract regex: PublicApiRegex with get, set
        abstract newInstance: (unit -> HLJSApi) with get, set


    [<AllowNullLiteral>]
    type LanguageFn =
        [<Emit "$0($1...)">]
        abstract Invoke: hljs: HLJSApi -> Language

    [<AllowNullLiteral>]
    type HighlightResultSecondBest =
        abstract code: string option with get, set
        abstract relevance: float with get, set
        abstract value: string with get, set
        abstract language: string option with get, set
        abstract illegal: bool with get, set
        abstract errorRaised: Error option with get, set

    [<AllowNullLiteral>]
    type HighlightResult =
        abstract code: string option with get, set
        abstract relevance: float with get, set
        abstract value: string with get, set
        abstract language: string option with get, set
        abstract illegal: bool with get, set
        abstract errorRaised: Error option with get, set
        abstract secondBest: HighlightResultSecondBest option with get, set

    [<AllowNullLiteral>]
    type AutoHighlightResult =
        inherit HighlightResult

    [<AllowNullLiteral>]
    type BeforeHighlightContext =
        abstract code: string with get, set
        abstract language: string with get, set
        abstract result: HighlightResult option with get, set

    type PluginEvent = HLJSPlugin

    [<AllowNullLiteral>]
    type HLJSPlugin =
        abstract ``after:highlight``: (HighlightResult -> unit) option with get, set
        abstract ``before:highlight``: (BeforeHighlightContext -> unit) option with get, set
        abstract ``after:highlightElement``: (HLJSPluginAfter_highlightElement -> unit) option with get, set
        abstract ``before:highlightElement``: (HLJSPluginBefore_highlightElement -> unit) option with get, set
        abstract ``after:highlightBlock``: (HLJSPluginAfter_highlightBlock -> unit) option with get, set
        abstract ``before:highlightBlock``: (HLJSPluginBefore_highlightBlock -> unit) option with get, set

    [<AllowNullLiteral>]
    type HighlightOptions =
        abstract language: string with get, set
        abstract ignoreIllegals: bool option with get, set

    [<AllowNullLiteral>]
    type HLJSOptions =
        abstract noHighlightRe: RegExp with get, set
        abstract languageDetectRe: RegExp with get, set
        abstract classPrefix: string with get, set
        abstract cssSelector: string with get, set
        abstract languages: ResizeArray<string> option with get, set
        // abstract __emitter: EmitterConstructor with get, set
        abstract ignoreUnescapedHTML: bool option with get, set
        abstract throwUnescapedHTML: bool option with get, set

    [<AllowNullLiteral>]
    type Language =
        interface
        end

    [<AllowNullLiteral>]
    type Emitter =
        abstract addKeyword: text: string * kind: string -> unit
        abstract addText: text: string -> unit
        abstract toHTML: unit -> string
        abstract finalize: unit -> unit
        abstract closeAllNodes: unit -> unit
        abstract openNode: kind: string -> unit
        abstract closeNode: unit -> unit
        abstract addSublanguage: emitter: Emitter * subLanguageName: string -> unit

    [<AllowNullLiteral>]
    type PublicApiRegisterAliases =
        abstract languageName: string with get, set

    [<AllowNullLiteral>]
    type PublicApiRegex =
        abstract concat: (ResizeArray<U2<RegExp, string>> -> string) with get, set
        abstract lookahead: (U2<RegExp, string> -> string) with get, set
        abstract either: (U2<ResizeArray<U2<RegExp, string>>, obj * RegexEitherOptions> -> string) with get, set
        abstract optional: (U2<RegExp, string> -> string) with get, set
        abstract anyNumberOfTimes: (U2<RegExp, string> -> string) with get, set

    [<AllowNullLiteral>]
    type HLJSPluginAfter_highlightElement =
        abstract el: Element with get, set
        abstract result: HighlightResult with get, set
        abstract text: string with get, set

    [<AllowNullLiteral>]
    type HLJSPluginBefore_highlightElement =
        abstract el: Element with get, set
        abstract language: string with get, set

    [<AllowNullLiteral>]
    type HLJSPluginAfter_highlightBlock =
        abstract block: Element with get, set
        abstract result: HighlightResult with get, set
        abstract text: string with get, set

    [<AllowNullLiteral>]
    type HLJSPluginBefore_highlightBlock =
        abstract block: Element with get, set
        abstract language: string with get, set
