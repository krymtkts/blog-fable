# [Blog Fable](https://krymtkts.github.io/blog-fable/index.html)

This is a small [Fable](https://fable.io/) app project to generate static pages.
Use React with [Feliz](https://zaid-ajaj.github.io/Feliz/#/).

This repository re-creates a subset of [fable-compiler/static-web-generator](https://github.com/fable-compiler/static-web-generator) with the latest dependencies.

GitHub Pages deployment is [here](https://krymtkts.github.io/blog-fable/index.html).
You can view the sample pages on this site.

## Motivation

- Learn how to write app with Fable
- Rebuild [My personal blog](https://github.com/krymtkts/krymtkts.github.io) with F#

## Supported features

- [x] Post
- [x] Page
- [x] RSS feed
- [x] Sitemap
- [x] Dev server
- [x] Color scheme

## Building and running the app

- Install dependencies: `npm install`
- Build entire pages: `npm run build`
  - It depends `npm run build-fable` and `npm run build-css`
    - Build pages: `npm run build-fable`
    - Build styles: `npm run build-css`
- Build pages and start a development server: `npm run dev`
  - After finished the first compilation, open: `http://localhost:8080/` automatically
  - The simplest way to build and start a server

## Credits

This repository incorporates parts of the following open source software:

- The project based on [fable-compiler/static-web-generator: Simple Fable Node.js app to generate static pages](https://github.com/fable-compiler/static-web-generator)
- `Node.Extra.fs` is originally written in [MangelMaxime/Nacara](https://github.com/MangelMaxime/Nacara)
- `dev-server.fsx` based on [sergey-tihon/suave-angular2-demo: Demo site for Suave 1.0 + Angular 2.0.0-beta.0](https://github.com/sergey-tihon/suave-angular2-demo)
- Color schema based on [Solarized](https://ethanschoonover.com/solarized/).
