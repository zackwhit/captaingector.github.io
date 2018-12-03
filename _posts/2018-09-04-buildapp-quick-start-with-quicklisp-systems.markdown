---
layout: post
title:  "Buildapp Quick Start With Quicklisp Systems"
date:   2018-09-04 11:19:45 -0400
categories: update lisp
---

[Buildapp](https://www.xach.com/lisp/buildapp/) is a wonderful little tool that Xach wrote to make the process of Common Lisp standalone executables slightly easier. Although the documentation is pretty good, I had a tiny bit of trouble (1) configuring it to work with Quicklisp systems and (2) creating a multicall binary for the first time, so I figured I might as well save one or two other people on the internet a tiny bit of work:

    buildapp --output ~/programming/bin/cl-multicall --asdf-tree ~/other-code/quicklisp/ --dispatched-entry /monolith:main --load-system monolith --dispatched-entry monolith/monolith:main --load-system graygoo --dispatched-entry graygoo/graygoo:main

or, more generally,

    buildapp --output /path/to/multicall/binary \
             --asdf-tree /path/to/quicklisp/ \
             --dispatched-entry /default-system:default-main \
             --load-system first-auxilary-system --dispatched-entry first-link-name/first-system-package:first-system-entrypoint \
             --load-system second-auxilary-system --dispatched-entry second-link-name/second-system-package:second-system-entrypoint \
             --load-system third-auxilary-system --dispatched-entry third-link-name/third-system-package:third-system-entrypoint

at which point you create links `first-link-name`, `second-link-name`, and `third-link-name` that all point to `/path/to/multicall/binary`. Executing the `first-link-name` link will result in the `first-system-entrypoint` function in the `first-system-package` package being invoked, and so on.

Lessons learned: first, that unless you want to use `--asdf-path` on literally every Quicklisp system ever, you'll need to point `--asdf-tree` at your Quicklisp install, and second, that you need a default `--dispatched-entry` argument (with no link-name before the forward slash `/`) in order for the build procedure to properly work.
