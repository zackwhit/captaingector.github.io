---
layout: post
title:  "Learn OpenGL: Chapters 1-4"
date:   2016-11-11 19:00:52 -0700
categories: update lisp opengl
---

This post will document the Common Lisp porting of [Learn OpenGL](http://learnopengl.com/) chapters 1 ("Introduction"), 2 ("OpenGL"), 3 ("Creating a Window"), and 4 ("Hello Window").

Chapter 1: Introduction
-----------------------

This chapter is purely informative. No porting necessary.

Chapter 2: OpenGL
-----------------

Although much of the friction in the tutorials are due to the way that C++ works, note that `cl-opengl` still has some of that friction (it's not a very thick wrapper). We still have to create, bind, unbind, and delete objects, and do manual cleanup after the OpenGL functions. As I work my way through the tutorials, I will try to write macros to make our OpenGL experience a bit more Lispy. For instance, look at the (generic - we'll introduce the actual code later) object-creation code in Common Lisp form:

```common-lisp
(let ((object (gl:gen-object)))
  (gl:bind-object :window-target object)
  (gl:set-object-option :window-target :window-width 800)
  (gl:set-object-option :window-target :window-height 600)
  (gl:bind-object :window-target object))
```

It turns out that we have to bind and unbind a lot of objects in OpenGL. This sounds like a process that we can make easier using a macro:

```common-lisp
(defmacro with-bound-object ((object target) &body body)
  (alexandria:once-only (target)
    `(multiple-value-prog1
         (progn
           (gl:bind-object ,target ,object)
           ,@body)
       (gl:bind-object ,target 0))))
```

Now we can write

```common-lisp
(let ((object (gl:gen-object)))
  (with-bound-object (object :window-target)
    (gl:set-object-option :window-target :window-width 800)
    (gl:set-object-option :window-target :window-height 600)))
```

More concise? A bit. We shaved off a "line" at the expense of a bit of added nesting depth. The bigger gain is in our ability to, like `let`, cordon off a block of code as "a place where a thing is bound, with that thing bound nowhere else", instead of having to mentally mark off portions of a flat code space as having `:window-target` bound or unbound.

Chapter 3: Creating a Window
----------------------------

This entire chapter focuses on building and linking GLFW and GLEW. Because we're using `cl-sdl2` and `cl-opengl`, we have a much easier time. Run `(ql:quickload :sdl2)` and `(ql:quickload :cl-opengl)` and you *should* be good. If you're missing libraries and Quicklisp freaks out, try installing `libsdl2-dev` and making sure that you have some sort of graphics drivers for your system.

Chapter 4: Hello Window
-----------------------

Ah, here's where things get interesting. Here's the window and GLEW initialization code:

```c++
glfwInit();
glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
glfwWindowHint(GLFW_RESIZABLE, GL_FALSE);

GLFWwindow* window = glfwCreateWindow(800, 600, "LearnOpenGL", nullptr, nullptr);
if (window == nullptr)
{
    std::cout << "Failed to create GLFW window" << std::endl;
        glfwTerminate();
            return -1;

}
glfwMakeContextCurrent(window);

glewExperimental = GL_TRUE;
if (glewInit() != GLEW_OK)
{
    std::cout << "Failed to initialize GLEW" << std::endl;
        return -1;

}
```

We'll use cl-sdl2's wonderful `with-init` and `with-window` macros, as well as `gl-set-attrs` to hint at which version of OpenGL we want:

```common-lisp
(sdl2:with-init (:everything)
  (sdl2:gl-set-attrs :context-major-version 3
                     :context-minor-version 3
                     :context-core-profile sdl2-ffi:+sdl-gl-context-profile-core+)
  (sdl2:with-window (window :w 800 :h 600 :flags '(:opengl :resizable))
    (sdl2:with-gl-context (context window)
      (format t "opengl version: ~s~%" (gl:get-string :version))
      (sdl2:gl-make-current window context)

      ...)))
```

(many thanks to axion on #lispgames for helping me figure out which flags `gl-set-attrs` wanted) This takes care of the SDL initialization code. Next, we set the OpenGL viewport size. In the tutorials, this is done with

```c++
int width, height;
glfwGetFramebufferSize(window, &width, &height);
glViewport(0, 0, width, height);
```

`sdl2:get-window-size` returns multiple values on the stack instead of providing a cons cell or list, so we'll use `multiple-value-bind` to catch all of them:

```common-lisp
(multiple-value-bind (width height) (sdl2:get-window-size window)
  (gl:viewport 0 0 width height))
```

The tutorial then goes through some stuff with creating an empty window that OpenGL never touches. I want to skip straight to putting a nice color in that window. We set up our event loop using `sdl2:with-event-loop`:

```common-lisp
(sdl2:with-event-loop (:method :poll)
  (:keyup (:keysym keysym)
          (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
            (sdl2:push-event :quit)))
  (:idle ()
         (gl:clear-color 0.2 0.3 0.3 1.0)
         (gl:clear :color-buffer)

         (sdl2:gl-swap-window window))
  (:quit ()
         t))
```

...and that's it! Compile and run, and you should see a window with a lovely dark-olive-gray color in it:

![a window with an awesome color in it](https://github.com/fouric/fouric.github.io/raw/master/images/learn-opengl/chapter-4-final.png)

...and that's a wrap.

2018-09-07 update: rewrote `WITH-BOUND-OBJECT` using `ALEXANDRIA:ONCE-ONLY` to automatically gensym the `TARGET` argument and `MULTIPLE-VALUE-PROG1` to ensure that the values returned from the body were correctly returned from the macroexpansion.
