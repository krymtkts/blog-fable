module App

open StaticWebGenerator

render
    { stage = dev
      lang = "ja"
      siteName = "Blog Fable"
      description = "A small Fable app project to generate static pages"
      siteUrl = "https://krymtkts.github.io"
      pathRoot = "/blog-fable"
      copyright = "2023 krymtkts"
      favicon = "/img/favicon.ico"

      src = "contents"
      dst = "docs"

      posts = { root = "/posts"; title = "Posts" }
      pages = { root = "/pages"; title = "Pages" }
      tags = { root = "/tags"; title = "Tags" }
      archives =
        { root = "/archives"
          title = "Archives" }

      feedName = "feed"

      additionalNavs =
          [ { text = "About"
              path = "/pages/about.html" } ]

    }
