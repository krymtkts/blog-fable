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

      postsRoot = "/posts"
      postsTitle = "Posts"
      pagesRoot = "/pages"
      paesTitle = "Pages"
      tagsRoot = "/tags"
      tagsTitle = "Tags"
      archivesRoot = "/archives"
      archivesTitle = "Archives"

      feed = "feed"

      additionalNavs =
          [ Link
                { text = "About Me"
                  path = "/pages/about.html"
                  sitemap = No } ]

    }
