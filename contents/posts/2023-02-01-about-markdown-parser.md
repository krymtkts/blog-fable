---
title: About Markdown parser
tags: [sample, markdown, fsharp]
---

You can write posts or pages in Markdown.
Markdown is parse by [Marked](https://marked.js.org/).

The sample rendering is following.

---

plain

_italic_

**bold**

`code`

~~strikethrough~~

plain _italic_ **bold** `code` ~~strikethrough~~

`extreme long inline code ......................................................................`

Recommends to start Markdown headings from h2 because it render the blog title as h1.

## blockquote

> blockquote
>
> blockquote
> blockquote

## codeblock

```fsharp
module App

open StaticWebGenerator

let private docsPath = IO.resolve "docs"
let private extremeLongVariableNameXxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx = "this is long string long long long ..........."
```

## list

- bullet list item a
  - [x] level 2 task item 1
  - [ ] level 2 task item 2
    - level 3
      - level 4

1. ordered list
   1. [ ] level 2 task item
      1. level 3
      - [ ] level 4
   2. level 2 item b
   - [ ] level 3

- composition

  1. [x] ordered item 1
     - ~~strikethrough~~

  - unordered item 1 has extreme long label text ......................................
  - [ ] unordered item 2 has extreme long label text ......................................
    - `inline code`
    1. _italic_
       - **bold**
         - **_italic bold_**
  - > blockquote
    >
    > multiline
  - ```fsharp
    module App

    open StaticWebGenerator
    ```

## table

| col1 | col2 |  col3 | col4 |
| ---- | :--- | ----: | :--: |
| 1    | 2    |     3 |  4   |
| one  | two  | three | four |

## link

[krymtkts/blog-fable](https://github.com/krymtkts/blog-fable)
