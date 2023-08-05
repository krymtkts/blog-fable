module App

open StaticWebGenerator

render
    { stage = dev
      lang = "ja"
      siteName = "Blog Title"
      description = "Blog Description"
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
          [ { text = "About Me"
              path = "/pages/about.html" } ]

    }
