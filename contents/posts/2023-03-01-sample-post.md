---
title: Sample post
tags: [sample, yaml, t, tag, this-is-extreme-long-tag-name]
date: 2023-03-02
---

Posts have front matter.
Front matter written in YAML.

The front matter for this post is as follows.

```yaml
---
title: This is sample post
tags: [sample, yaml, t, tag, this-is-extreme-long-tag-name]
date: 2023-03-02
---
```

- `title` is post title. render as h1
- `tags` is list of tag. Tags page generation based on the content of the `tags`
- `date` is update date. optional field. it overrides publication date.
  - Define publication date of a post in the title of the Markdown file
    - ex) `blog-fable/contents/posts/2023-02-01-this-is-sample-post.md`

Use [eemeli/yaml: YAML parser and stringifier for JavaScript](https://github.com/eemeli/yaml) for front matter parsing.

You can define a post without front matter if you want.

See [sample-post-without-front-matter.md](/blog-fable/posts/2023-01-01-sample-post-without-front-matter.html)
