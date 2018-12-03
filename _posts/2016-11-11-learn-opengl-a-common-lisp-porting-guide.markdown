---
layout: post
title:  "Learn OpenGL: A Common Lisp Porting Guide"
date:   2016-11-11 18:41:22 -0700
categories: update lisp opengl
---

A few months ago, I found Joey de Vries' excellent [Learn OpenGL](http://learnopengl.com/) tutorial web site. Joey has constructed one of the best tutorials on OpenGL that I've ever seen, and I'm super thankful for that. However, being a Common Lisp programmer, and not a C++ one, I am currently working my way through the tutorials in Common Lisp, using [cl-sdl2](https://github.com/lispgames/cl-sdl2) and [cl-opengl](https://github.com/3b/cl-opengl). Along the way, I've encountered a few...rough spots when translating the tutorial. These posts will document the translation work that I've done, the pitfalls that I've encountered (and how I worked my way around them), and my thoughts and ideas about OpenGL and the tutorial.

I'll start by writing a separate post for the first few chapters (you *will* need the tutorials themselves, as I will not reproduce them in my posts) and add more as I have time. Until people actually start *reading* my posts, though, they will remain largely draft quality. If you want me to spiff the posts up a bit, contact me and I'll go back and fix things.
