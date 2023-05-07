// ts2fable 0.7.1
module rec Marked

open System
open Fable.Core
open Fable.Core.JS

type Array<'T> = System.Collections.Generic.IList<'T>
type RegExp = System.Text.RegularExpressions.Regex

[<Import("marked", "marked")>]
let marked: Marked.IExports = jsNative

[<AllowNullLiteral>]
type IExports =
    /// <summary>Compiles markdown to HTML asynchronously.</summary>
    /// <param name="src">String of markdown source to be compiled</param>
    /// <param name="options">Hash of options, having async: true</param>
    abstract marked: src: string * options: obj -> Promise<string>

    /// <summary>Compiles markdown to HTML synchronously.</summary>
    /// <param name="src">String of markdown source to be compiled</param>
    /// <param name="options">Optional hash of options</param>
    abstract marked: src: string * ?options: Marked.MarkedOptions -> string

    /// <summary>Compiles markdown to HTML asynchronously with a callback.</summary>
    /// <param name="src">String of markdown source to be compiled</param>
    /// <param name="callback">Function called when the markdownString has been fully parsed when using async highlighting</param>
    abstract marked: src: string * callback: (obj option -> string -> unit) -> unit

    /// <summary>Compiles markdown to HTML asynchronously with a callback.</summary>
    /// <param name="src">String of markdown source to be compiled</param>
    /// <param name="options">Hash of options</param>
    /// <param name="callback">Function called when the markdownString has been fully parsed when using async highlighting</param>
    abstract marked: src: string * options: Marked.MarkedOptions * callback: (obj option -> string -> unit) -> unit

    abstract Lexer: LexerStatic
    abstract Parser: ParserStatic
    abstract Tokenizer: TokenizerStatic
    abstract Renderer: RendererStatic
    abstract TextRenderer: TextRendererStatic
    abstract Slugger: SluggerStatic

[<AllowNullLiteral>]
type Lexer =
    inherit Marked.Lexer

[<AllowNullLiteral>]
type LexerStatic =
    [<Emit "new $0($1...)">]
    abstract Create: unit -> Lexer

[<AllowNullLiteral>]
type Parser =
    inherit Marked.Parser

[<AllowNullLiteral>]
type ParserStatic =
    [<Emit "new $0($1...)">]
    abstract Create: unit -> Parser

type Tokenizer = Tokenizer<obj>

[<AllowNullLiteral>]
type Tokenizer<'T> =
    inherit Marked.Tokenizer<'T>

[<AllowNullLiteral>]
type TokenizerStatic =
    [<Emit "new $0($1...)">]
    abstract Create: unit -> Tokenizer<'T>

type Renderer = Renderer<obj>

[<AllowNullLiteral>]
type Renderer<'T> =
    inherit Marked.Renderer<'T>

[<AllowNullLiteral>]
type RendererStatic =
    [<Emit "new $0($1...)">]
    abstract Create: unit -> Renderer<'T>

[<AllowNullLiteral>]
type TextRenderer =
    inherit Marked.TextRenderer

[<AllowNullLiteral>]
type TextRendererStatic =
    [<Emit "new $0($1...)">]
    abstract Create: unit -> TextRenderer

[<AllowNullLiteral>]
type Slugger =
    inherit Marked.Slugger

[<AllowNullLiteral>]
type SluggerStatic =
    [<Emit "new $0($1...)">]
    abstract Create: unit -> Slugger

module Marked =

    [<AllowNullLiteral>]
    type IExports =
        abstract defaults: MarkedOptions

        /// <param name="src">String of markdown source to be compiled</param>
        /// <param name="options">Hash of options</param>
        abstract lexer: src: string * ?options: MarkedOptions -> TokensList

        /// <summary>Compiles markdown to HTML.</summary>
        /// <param name="src">String of markdown source to be compiled</param>
        /// <param name="callback">Function called when the markdownString has been fully parsed when using async highlighting</param>
        abstract parse: src: string * callback: (obj option -> string -> unit) -> unit

        /// <summary>Compiles markdown to HTML asynchronously.</summary>
        /// <param name="src">String of markdown source to be compiled</param>
        /// <param name="options">Hash of options having async: true</param>
        abstract parse: src: string * options: obj -> Promise<string>

        /// <summary>Compiles markdown to HTML synchronously.</summary>
        /// <param name="src">String of markdown source to be compiled</param>
        /// <param name="options">Optional hash of options</param>
        abstract parse: src: string * ?options: MarkedOptions -> string

        /// <summary>Compiles markdown to HTML synchronously.</summary>
        /// <param name="src">String of markdown source to be compiled</param>
        /// <param name="options">Optional hash of options</param>
        /// <param name="callback">Function called when the markdownString has been fully parsed when using async highlighting</param>
        abstract parse: src: string * options: MarkedOptions * callback: (obj option -> string -> unit) -> unit

        /// <param name="src">Tokenized source as array of tokens</param>
        /// <param name="options">Hash of options</param>
        abstract parser: src: U2<ResizeArray<Token>, TokensList> * ?options: MarkedOptions -> string

        /// <summary>Compiles markdown to HTML without enclosing `p` tag.</summary>
        /// <param name="src">String of markdown source to be compiled</param>
        /// <param name="options">Hash of options</param>
        abstract parseInline: src: string * ?options: MarkedOptions -> string

        /// <summary>Sets the default options.</summary>
        /// <param name="options">Hash of options</param>
        abstract options: options: MarkedOptions -> obj

        /// <summary>Sets the default options.</summary>
        /// <param name="options">Hash of options</param>
        abstract setOptions: options: MarkedOptions -> obj

        /// Gets the original marked default options.
        abstract getDefaults: unit -> MarkedOptions
        abstract walkTokens: tokens: U2<ResizeArray<Token>, TokensList> * callback: (Token -> unit) -> obj
        /// Use Extension
        abstract ``use``: [<ParamArray>] extensions: ResizeArray<MarkedExtension> -> unit
        abstract Tokenizer: TokenizerStatic
        abstract Renderer: RendererStatic
        abstract TextRenderer: TextRendererStatic
        abstract Parser: ParserStatic
        abstract Lexer: LexerStatic
        abstract Slugger: SluggerStatic

    type Tokenizer = Tokenizer<obj>

    [<AllowNullLiteral>]
    type Tokenizer<'T> =
        abstract options: MarkedOptions with get, set
        abstract space: this: obj * src: string -> U2<Tokens.Space, 'T>
        abstract code: this: obj * src: string -> U2<Tokens.Code, 'T>
        abstract fences: this: obj * src: string -> U2<Tokens.Code, 'T>
        abstract heading: this: obj * src: string -> U2<Tokens.Heading, 'T>
        abstract hr: this: obj * src: string -> U2<Tokens.Hr, 'T>
        abstract blockquote: this: obj * src: string -> U2<Tokens.Blockquote, 'T>
        abstract list: this: obj * src: string -> U2<Tokens.List, 'T>
        abstract html: this: obj * src: string -> U2<Tokens.HTML, 'T>
        abstract def: this: obj * src: string -> U2<Tokens.Def, 'T>
        abstract table: this: obj * src: string -> U2<Tokens.Table, 'T>
        abstract lheading: this: obj * src: string -> U2<Tokens.Heading, 'T>
        abstract paragraph: this: obj * src: string -> U2<Tokens.Paragraph, 'T>
        abstract text: this: obj * src: string -> U2<Tokens.Text, 'T>
        abstract escape: this: obj * src: string -> U2<Tokens.Escape, 'T>
        abstract tag: this: obj * src: string -> U2<Tokens.Tag, 'T>
        abstract link: this: obj * src: string -> U3<Tokens.Image, Tokens.Link, 'T>

        abstract reflink:
            this: obj * src: string * links: U2<ResizeArray<Tokens.Link>, ResizeArray<Tokens.Image>> ->
                U4<Tokens.Link, Tokens.Image, Tokens.Text, 'T>

        abstract emStrong:
            this: obj * src: string * maskedSrc: string * prevChar: string -> U3<Tokens.Em, Tokens.Strong, 'T>

        abstract codespan: this: obj * src: string -> U2<Tokens.Codespan, 'T>
        abstract br: this: obj * src: string -> U2<Tokens.Br, 'T>
        abstract del: this: obj * src: string -> U2<Tokens.Del, 'T>
        abstract autolink: this: obj * src: string * mangle: (string -> string) -> U2<Tokens.Link, 'T>
        abstract url: this: obj * src: string * mangle: (string -> string) -> U2<Tokens.Link, 'T>
        abstract inlineText: this: obj * src: string * smartypants: (string -> string) -> U2<Tokens.Text, 'T>

    [<AllowNullLiteral>]
    type TokenizerStatic =
        [<Emit "new $0($1...)">]
        abstract Create: ?options: MarkedOptions -> Tokenizer<'T>

    type TokenizerObject = obj

    type Renderer = Renderer<obj>

    [<AllowNullLiteral>]
    type Renderer<'T> =
        abstract options: MarkedOptions with get, set

        abstract code:
            this: U2<Renderer, RendererThis> * code: string * language: string option * isEscaped: bool ->
                U2<string, 'T>

        abstract blockquote: this: U2<Renderer, RendererThis> * quote: string -> U2<string, 'T>
        abstract html: this: U2<Renderer, RendererThis> * html: string -> U2<string, 'T>

        abstract heading:
            this: U2<Renderer, RendererThis> *
            text: string *
            level: RendererHeadingLevel *
            raw: string *
            slugger: Slugger ->
                U2<string, 'T>

        abstract hr: this: U2<Renderer, RendererThis> -> U2<string, 'T>
        abstract list: this: U2<Renderer, RendererThis> * body: string * ordered: bool * start: float -> U2<string, 'T>

        abstract listitem:
            this: U2<Renderer, RendererThis> * text: string * task: bool * ``checked``: bool -> U2<string, 'T>

        abstract checkbox: this: U2<Renderer, RendererThis> * ``checked``: bool -> U2<string, 'T>
        abstract paragraph: this: U2<Renderer, RendererThis> * text: string -> U2<string, 'T>
        abstract table: this: U2<Renderer, RendererThis> * header: string * body: string -> U2<string, 'T>
        abstract tablerow: this: U2<Renderer, RendererThis> * content: string -> U2<string, 'T>

        abstract tablecell:
            this: U2<Renderer, RendererThis> * content: string * flags: RendererTablecellFlags -> U2<string, 'T>

        abstract strong: this: U2<Renderer, RendererThis> * text: string -> U2<string, 'T>
        abstract em: this: U2<Renderer, RendererThis> * text: string -> U2<string, 'T>
        abstract codespan: this: U2<Renderer, RendererThis> * code: string -> U2<string, 'T>
        abstract br: this: U2<Renderer, RendererThis> -> U2<string, 'T>
        abstract del: this: U2<Renderer, RendererThis> * text: string -> U2<string, 'T>

        abstract link:
            this: U2<Renderer, RendererThis> * href: string option * title: string option * text: string ->
                U2<string, 'T>

        abstract image:
            this: U2<Renderer, RendererThis> * href: string option * title: string option * text: string ->
                U2<string, 'T>

        abstract text: this: U2<Renderer, RendererThis> * text: string -> U2<string, 'T>

    [<RequireQualifiedAccess>]
    type RendererHeadingLevel =
        | N1 = 1
        | N2 = 2
        | N3 = 3
        | N4 = 4
        | N5 = 5
        | N6 = 6

    [<AllowNullLiteral>]
    type RendererTablecellFlags =
        abstract header: bool with get, set
        abstract align: RendererTablecellFlagsAlign with get, set

    [<AllowNullLiteral>]
    type RendererStatic =
        [<Emit "new $0($1...)">]
        abstract Create: ?options: MarkedOptions -> Renderer<'T>

    type RendererObject = obj

    [<AllowNullLiteral>]
    type TextRenderer =
        abstract strong: text: string -> string
        abstract em: text: string -> string
        abstract codespan: text: string -> string
        abstract del: text: string -> string
        abstract text: text: string -> string
        abstract link: href: string option * title: string option * text: string -> string
        abstract image: href: string option * title: string option * text: string -> string
        abstract br: unit -> string
        abstract html: text: string -> string

    [<AllowNullLiteral>]
    type TextRendererStatic =
        [<Emit "new $0($1...)">]
        abstract Create: unit -> TextRenderer

    [<AllowNullLiteral>]
    type Parser =
        abstract tokens: U2<ResizeArray<Token>, TokensList> with get, set
        abstract token: Token option with get, set
        abstract options: MarkedOptions with get, set
        abstract renderer: Renderer with get, set
        abstract textRenderer: TextRenderer with get, set
        abstract slugger: Slugger with get, set
        abstract parse: src: U2<ResizeArray<Token>, TokensList> -> string
        abstract parseInline: src: ResizeArray<Token> * ?renderer: Renderer -> string
        abstract next: unit -> Token

    [<AllowNullLiteral>]
    type ParserStatic =
        [<Emit "new $0($1...)">]
        abstract Create: ?options: MarkedOptions -> Parser

        abstract parse: src: U2<ResizeArray<Token>, TokensList> * ?options: MarkedOptions -> string
        abstract parseInline: src: ResizeArray<Token> * ?options: MarkedOptions -> string

    [<AllowNullLiteral>]
    type Lexer =
        abstract tokens: TokensList with get, set
        abstract options: MarkedOptions with get, set
        abstract rules: Rules with get, set
        abstract lex: src: string -> TokensList
        abstract blockTokens: src: string * tokens: ResizeArray<Token> -> ResizeArray<Token>
        abstract blockTokens: src: string * tokens: TokensList -> TokensList
        abstract ``inline``: src: string * ?tokens: ResizeArray<Token> -> ResizeArray<Token>
        abstract inlineTokens: src: string * ?tokens: ResizeArray<Token> -> ResizeArray<Token>
        abstract state: LexerState with get, set

    [<AllowNullLiteral>]
    type LexerStatic =
        [<Emit "new $0($1...)">]
        abstract Create: ?options: MarkedOptions -> Lexer

        abstract rules: Rules with get, set
        abstract lex: src: string * ?options: MarkedOptions -> TokensList
        abstract lexInline: src: string * ?options: MarkedOptions -> ResizeArray<Token>

    [<AllowNullLiteral>]
    type Slugger =
        abstract seen: SluggerSeen with get, set
        abstract slug: value: string * ?options: SluggerOptions -> string

    [<AllowNullLiteral>]
    type SluggerStatic =
        [<Emit "new $0($1...)">]
        abstract Create: unit -> Slugger

    [<AllowNullLiteral>]
    type SluggerOptions =
        abstract dryrun: bool with get, set

    [<AllowNullLiteral>]
    type Rules =
        [<Emit "$0[$1]{{=$2}}">]
        abstract Item: ruleName: string -> U2<RegExp, Rules> with get, set

    [<AllowNullLiteral>]
    type TokensList =
        interface
        end

    type Token = obj

    module Tokens =

        [<AllowNullLiteral>]
        type Space =
            abstract ``type``: string with get, set
            abstract raw: string with get, set

        [<AllowNullLiteral>]
        type Code =
            abstract ``type``: string with get, set
            abstract raw: string with get, set
            abstract codeBlockStyle: CodeCodeBlockStyle option with get, set
            abstract lang: string option with get, set
            abstract text: string with get, set

        [<AllowNullLiteral>]
        type Heading =
            abstract ``type``: string with get, set
            abstract raw: string with get, set
            abstract depth: float with get, set
            abstract text: string with get, set
            abstract tokens: ResizeArray<Token> with get, set

        [<AllowNullLiteral>]
        type Table =
            abstract ``type``: string with get, set
            abstract raw: string with get, set
            abstract align: Array<TableAlignArray> with get, set
            abstract header: ResizeArray<TableCell> with get, set
            abstract rows: ResizeArray<ResizeArray<TableCell>> with get, set

        [<AllowNullLiteral>]
        type TableCell =
            abstract text: string with get, set
            abstract tokens: ResizeArray<Token> with get, set

        [<AllowNullLiteral>]
        type Hr =
            abstract ``type``: string with get, set
            abstract raw: string with get, set

        [<AllowNullLiteral>]
        type Blockquote =
            abstract ``type``: string with get, set
            abstract raw: string with get, set
            abstract text: string with get, set
            abstract tokens: ResizeArray<Token> with get, set

        [<AllowNullLiteral>]
        type List =
            abstract ``type``: string with get, set
            abstract raw: string with get, set
            abstract ordered: bool with get, set
            abstract start: U2<float, string> with get, set
            abstract loose: bool with get, set
            abstract items: ResizeArray<ListItem> with get, set

        [<AllowNullLiteral>]
        type ListItem =
            abstract ``type``: string with get, set
            abstract raw: string with get, set
            abstract task: bool with get, set
            abstract ``checked``: bool option with get, set
            abstract loose: bool with get, set
            abstract text: string with get, set
            abstract tokens: ResizeArray<Token> with get, set

        [<AllowNullLiteral>]
        type Paragraph =
            abstract ``type``: string with get, set
            abstract raw: string with get, set
            abstract pre: bool option with get, set
            abstract text: string with get, set
            abstract tokens: ResizeArray<Token> with get, set

        [<AllowNullLiteral>]
        type HTML =
            abstract ``type``: string with get, set
            abstract raw: string with get, set
            abstract pre: bool with get, set
            abstract text: string with get, set

        [<AllowNullLiteral>]
        type Text =
            abstract ``type``: string with get, set
            abstract raw: string with get, set
            abstract text: string with get, set
            abstract tokens: ResizeArray<Token> option with get, set

        [<AllowNullLiteral>]
        type Def =
            abstract ``type``: string with get, set
            abstract raw: string with get, set
            abstract tag: string with get, set
            abstract href: string with get, set
            abstract title: string with get, set

        [<AllowNullLiteral>]
        type Escape =
            abstract ``type``: string with get, set
            abstract raw: string with get, set
            abstract text: string with get, set

        [<AllowNullLiteral>]
        type Tag =
            abstract ``type``: TagType with get, set
            abstract raw: string with get, set
            abstract inLink: bool with get, set
            abstract inRawBlock: bool with get, set
            abstract text: string with get, set

        [<AllowNullLiteral>]
        type Link =
            abstract ``type``: string with get, set
            abstract raw: string with get, set
            abstract href: string with get, set
            abstract title: string with get, set
            abstract text: string with get, set
            abstract tokens: ResizeArray<Token> with get, set

        [<AllowNullLiteral>]
        type Image =
            abstract ``type``: string with get, set
            abstract raw: string with get, set
            abstract href: string with get, set
            abstract title: string with get, set
            abstract text: string with get, set

        [<AllowNullLiteral>]
        type Strong =
            abstract ``type``: string with get, set
            abstract raw: string with get, set
            abstract text: string with get, set
            abstract tokens: ResizeArray<Token> with get, set

        [<AllowNullLiteral>]
        type Em =
            abstract ``type``: string with get, set
            abstract raw: string with get, set
            abstract text: string with get, set
            abstract tokens: ResizeArray<Token> with get, set

        [<AllowNullLiteral>]
        type Codespan =
            abstract ``type``: string with get, set
            abstract raw: string with get, set
            abstract text: string with get, set

        [<AllowNullLiteral>]
        type Br =
            abstract ``type``: string with get, set
            abstract raw: string with get, set

        [<AllowNullLiteral>]
        type Del =
            abstract ``type``: string with get, set
            abstract raw: string with get, set
            abstract text: string with get, set
            abstract tokens: ResizeArray<Token> with get, set

        [<AllowNullLiteral>]
        type Generic =
            [<Emit "$0[$1]{{=$2}}">]
            abstract Item: index: string -> obj option with get, set

            abstract ``type``: string with get, set
            abstract raw: string with get, set
            abstract tokens: ResizeArray<Token> option with get, set

        [<StringEnum>]
        [<RequireQualifiedAccess>]
        type CodeCodeBlockStyle = | Indented

        [<StringEnum>]
        [<RequireQualifiedAccess>]
        type TableAlignArray =
            | Center
            | Left
            | Right

        [<StringEnum>]
        [<RequireQualifiedAccess>]
        type TagType =
            | Text
            | Html

    [<AllowNullLiteral>]
    type TokenizerThis =
        abstract lexer: Lexer with get, set

    [<AllowNullLiteral>]
    type TokenizerExtension =
        abstract name: string with get, set
        abstract level: TokenizerExtensionLevel with get, set
        abstract start: (TokenizerThis -> string -> U2<float, unit>) option with get, set
        abstract tokenizer: (TokenizerThis -> string -> U2<ResizeArray<Token>, TokensList> -> U2<Tokens.Generic, unit>) with get, set
        abstract childTokens: ResizeArray<string> option with get, set

    [<AllowNullLiteral>]
    type RendererThis =
        abstract parser: Parser with get, set

    [<AllowNullLiteral>]
    type RendererExtension =
        abstract name: string with get, set
        abstract renderer: (RendererThis -> Tokens.Generic -> string) with get, set

    type TokenizerAndRendererExtension = U3<TokenizerExtension, RendererExtension, obj>

    [<AllowNullLiteral>]
    type MarkedExtension =
        /// True will tell marked to await any walkTokens functions before parsing the tokens and returning an HTML string.
        abstract async: bool option with get, set
        /// A prefix URL for any relative link.
        abstract baseUrl: string option with get, set
        /// Enable GFM line breaks. This option requires the gfm option to be true.
        abstract breaks: bool option with get, set
        /// Add tokenizers and renderers to marked
        abstract extensions: ResizeArray<TokenizerAndRendererExtension> option with get, set
        /// Enable GitHub flavored markdown.
        abstract gfm: bool option with get, set
        /// Include an id attribute when emitting headings.
        abstract headerIds: bool option with get, set
        /// Set the prefix for header tag ids.
        abstract headerPrefix: string option with get, set

        /// A function to highlight code blocks. The function can either be
        /// synchronous (returning a string) or asynchronous (callback invoked
        /// with an error if any occurred during highlighting and a string
        /// if highlighting was successful)
        abstract highlight: code: string * lang: string * ?callback: (obj option -> string -> unit) -> U2<string, unit>

        /// Hooks are methods that hook into some part of marked.
        /// preprocess is called to process markdown before sending it to marked.
        /// postprocess is called to process html after marked has finished parsing.
        abstract hooks: MarkedExtensionHooks option with get, set

        /// Set the prefix for code block classes.
        abstract langPrefix: string option with get, set
        /// Mangle autolinks (<email@domain.com>).
        abstract mangle: bool option with get, set
        /// Conform to obscure parts of markdown.pl as much as possible. Don't fix any of the original markdown bugs or poor behavior.
        abstract pedantic: bool option with get, set

        /// Type: object Default: new Renderer()
        ///
        /// An object containing functions to render tokens to HTML.
        abstract renderer: U2<Renderer, RendererObject> option with get, set

        /// Sanitize the output. Ignore any HTML that has been input.
        abstract sanitize: bool option with get, set
        /// Optionally sanitize found HTML with a sanitizer function.
        abstract sanitizer: html: string -> string
        /// Shows an HTML error message when rendering fails.
        abstract silent: bool option with get, set
        /// Use smarter list behavior than the original markdown. May eventually be default with the old behavior moved into pedantic.
        abstract smartLists: bool option with get, set
        /// Use "smart" typograhic punctuation for things like quotes and dashes.
        abstract smartypants: bool option with get, set
        /// The tokenizer defines how to turn markdown text into tokens.
        abstract tokenizer: U2<Tokenizer, TokenizerObject> option with get, set

        /// The walkTokens function gets called with every token.
        /// Child tokens are called before moving on to sibling tokens.
        /// Each token is passed by reference so updates are persisted when passed to the parser.
        /// The return value of the function is ignored.
        abstract walkTokens: (Token -> unit) option with get, set

        /// Generate closing slash for self-closing tags (<br/> instead of <br>)
        abstract xhtml: bool option with get, set

    [<AllowNullLiteral>]
    type MarkedOptions =
        // inherit Omit<MarkedExtension, string>
        /// Type: object Default: new Renderer()
        ///
        /// An object containing functions to render tokens to HTML.
        abstract renderer: Renderer option with get, set

        /// The tokenizer defines how to turn markdown text into tokens.
        abstract tokenizer: Tokenizer option with get, set

    [<StringEnum>]
    [<RequireQualifiedAccess>]
    type RendererTablecellFlagsAlign =
        | Center
        | Left
        | Right

    [<AllowNullLiteral>]
    type LexerState =
        abstract inLink: bool with get, set
        abstract inRawBlock: bool with get, set
        abstract top: bool with get, set

    [<AllowNullLiteral>]
    type SluggerSeen =
        [<Emit "$0[$1]{{=$2}}">]
        abstract Item: slugValue: string -> float with get, set

    [<StringEnum>]
    [<RequireQualifiedAccess>]
    type TokenizerExtensionLevel =
        | Block
        | Inline

    [<AllowNullLiteral>]
    type MarkedExtensionHooks =
        abstract preprocess: (string -> string) option with get, set
        abstract postprocess: (string -> string) option with get, set
