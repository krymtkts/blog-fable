---
title: Flatten posts in nested directory
tags: [sample]
---

Flatten posts in nested directory into `posts` directory.
Intend the nested directories used for organizing source Markdown files.

```plaintext
source files.

\---contents
    \---posts
        \---2022
            \---12
                \---2022-12-31-nested-post.md

destination files.

\---docs
    \${path_root}
        \---posts
            \---2022-12-31-nested-post.md
```
