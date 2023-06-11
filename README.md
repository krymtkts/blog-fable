# [Blog Fable](https://krymtkts.github.io/blog-fable/index.html)

This is a small [Fable](https://fable.io/) app project to generate static pages.
Use React with [Feliz](https://zaid-ajaj.github.io/Feliz/#/).

This repository re-creates a subset of [fable-compiler/static-web-generator](https://github.com/fable-compiler/static-web-generator) with the latest dependencies.

GitHub Pages deployment is [here](https://krymtkts.github.io/blog-fable/index.html).

## Motivation

- Learn how to write app with Fable
- Rebuild [My personal blog](https://github.com/krymtkts/krymtkts.github.io) with F#

## Supported features

- [x] Post
- [x] Page
- [ ] RSS feed
- [ ] Sitemap
- [ ] Dev server

## Building and running the app

- Install dependencies: `npm install`
- Build pages: `npm run build`
- Build pages and start a development server: `npm run start`
  - After finished the first compilation, open: `http://localhost:8080/`

## Credits

This repository incorporates parts of the following open source software:

- project based on [fable-compiler/static-web-generator: Simple Fable Node.js app to generate static pages](https://github.com/fable-compiler/static-web-generator)
- `Node.Extra.fs` is originally written in [MangelMaxime/Nacara](https://github.com/MangelMaxime/Nacara)
