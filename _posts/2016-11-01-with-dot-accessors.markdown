---
layout: post
title:  "WITH-DOT-ACCESSORS"
date:   2016-11-01 17:13:36 -0700
categories: update lisp
---

Several weeks ago, I decided that I wanted to give another shot at writing a little 2D real-time dungeoncrawler in Common Lisp. To do that, I needed some sort of collision engine. [Box2D](http://box2d.org/) seemed like it was along the lines of what I wanted, given how uncomplex it seemed to be. Unfortunately, Box2D is written in C++, which [cl-autowrap](https://github.com/rpav/cl-autowrap) appears to have trouble with in the general case. After a bit of poking, I came across [box2d-lite](https://github.com/dharmatech/box2d-lite), a Scheme port of an early version of Box2D written in C. Now, while I certainly don't have the time to port over any non-trivial C codebase to Common Lisp, I can certainly do a 1.7kloc Scheme library, and after [SirHawbly](https://github.com/SirHawbly) agreed to help, the deal was sealed.

Right away, when was working through my first Scheme file ([vec.sls](https://github.com/dharmatech/box2d-lite/blob/master/vec.sls)), I noticed that Scheme allows you to use C-like `foo.bar` syntax to access the `foo` field of a structure stored in a variable named `bar`, after making an assertion to the Scheme implementation that tells it what type of structure you're going to be accessing.

In Common Lisp, there is no such shortcut that you can take, partially because you can put periods in symbol names. Who's to say that `v.x` isn't simply a variable name instead of a structure member access?

Now, for a short piece of Scheme code like

    (* n v.y)

the corresponding Common Lisp code is

    (* n (vec-y v))

Not too bad. However, much of box2d-lite looks more like this:

    (define (vxv a b)
      (is-vec a)
      (is-vec b)
      (- (* a.x b.y)
         (* a.y b.x)))

...which in Common Lisp looks like

    (defun vxv (a b)
      (- (* (vec-x a) (vec-x b))
         (* (vec-y a) (vec-x b))))

Now imagine that your matrices are just structures with two vectors, so to get the "a" element of a 2x2 matrix, you have to do something along the lines of `(vec-x (mat-col1 m))`.

As you can imagine, this gets tiring quickly.

Eventually, I got fed up with the amount of mental effort I was having to exert to get a single member out of a matrix, so I wrote a macro, called `WITH-DOT-ACCESSORS`. You use it by providing it an alist of types and the variables that hold values of those types, and all it does it looks for symbols of form `FOO.BAR` and rewrites them to `(TYPE-BAR FOO)`, where TYPE is provided by the alist given at the beginning of the form. The invocation looks like

    (with-dot-accessors ((vec (v1 v2)))
      (+ v1.x v2.y))

and expands to

    (progn
      (+ (vec-x v1) (vec-y v2)))

Fragile? Yes. This can (and will) re-write function calls with periods in the names if you're not careful, causing compilation errors. However, it gets the job done, and given that it won't be in any user-facing code (that is, a user of [boxed2d](https://github.com/fouric/boxed2d) won't have to touch it), it doesn't have to be very polished.

Currently, the code lives in [macros.lisp](https://github.com/fouric/boxed2d/blob/master/src/util/macros.lisp), although that might change in the future. Now I just need to start converting over all of the code that was written without it...