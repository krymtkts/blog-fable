// ts2fable 0.7.1
module rec Yaml

open Fable.Core

[<Import("*", from = "yaml")>]
let Yaml: Yaml.IExports = jsNative

module Yaml =
    [<AllowNullLiteral>]
    type IExports =
        abstract parse: value: string -> 'a
