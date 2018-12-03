---
layout: post
title:  "Getting set up with Emacs, SLIME, and Evil"
date:   2016-05-20 13:13:37 -0700
categories: update lisp
---

Overview
--------

If you read the previous post about getting set up to hack on Lisp, and kept going, then you might be interested in migrating from Vim to Emacs and SLIME. Why? Because Emacs + SLIME provide a much more "Lispy" hacking experience that allows you write more code more quickly and with more fun. Also, if you use the Evil-mode extension for Emacs, then you don't have to learn an entirely new editing paradigm; you can continue to use the Vim keybindings that you know and (hopefully) love.

What are these things?
----------------------

[Emacs](http://www.gnu.org/software/emacs/index.html) is an IDE written in a Lisp called Emacs Lisp (the code is a small amount of C that implements the Emacs Lisp compiler and a few functions that need to be fast, and then the rest is Emacs Lisp) that includes stuff like an IRC client, web browser, RSS feed reader, and psychoanalyst (yes, really, `M-x doctor RET`). Emacs is *extremely* powerful and customizable, and can be configured to do basically anything that Vim can, plus more. [SLIME](https://common-lisp.net/project/slime/) (Superior Lisp Interaction Mode for Emacs) is an extension for Emacs that allows you to have a running Lisp REPL in Emacs. With SLIME, you can compile code from a file and run it in the REPL, compile and test individual functions, define functions in the REPL that can then be used in your code, handle conditions and debug, profile, and analyze you code (and data!), and more. [Evil](https://www.emacswiki.org/emacs/Evil) (Extensible VI Layer for emacs) is an extension for Emacs that gives you Vim keybindings and functionality. I personally prefer Vim's modal interface over whatever Emacs has, so this is one of the first things that I install on my Emacs. You don't have to install this if you don't want to.

Why should I use Emacs?
-----------------------

It'll pay off later. Emacs provides much more room for expansion than Vim does, meaning that it'll be significantly longer until you get fed up with the constraints of your editor and go off to write your own (or maybe that's just me). Additionally, Emacs + SLIME gives you a *much* better Lisp development environment than what Vim is currently capable of, and there are more extensions (such as Paredit) that make you even more efficient.

Assumption
----------

In this guide, I'm going to assume that you are either (1) using Linux on your own machine or (2) connecting to a CAT machine. If you are using Windows, I would recommend that you do the latter, as I don't know enough about getting Emacs to work on Windows to feel safe giving instructions on it.

Getting Emacs
-------------

If you're on your own machine, then you will need to install the `emacs-24` package. If you're on a CAT machine, then chances are that it's already installed thanks to nightfly++. Running `emacs` should be all you need to start it up.

Because I consider GUI menus to be a waste of screen space, then I would recommend editing your `.emacs` file (with Vim, of course) to include the following:

    (menu-bar-mode 0)
    (if (fboundp 'tool-bar-mode)
      (tool-bar-mode 0))
    (if (fboundp 'scroll-bar-mode)
      (scroll-bar-mode 0))

These will get rid of the menu bar, the tool bar (which only appears if you're using the GUI), and the scrollbars (which *also* only appear if you're using the GUI; that's what the `(if (fboundp ...` stuff is for, it makes sure that you're actually running in GUI mode before trying to turn off a GUI-mode-only feature). Quit Emacs using `C-x C-c`, restart, and all of those things should be gone.

You don't know what that cryptic string means? Welcome to Emacs' keychord notation. `C-x C-c` means "press x while holding down control, then press c while holding down control". `C-S-x M-b RET` means "press x while holding down control and shift, then press b while holding down meta, then press enter (RETurn)". Emacs uses "meta" to mean "alt" due to the design of the [Lisp Machine keyboards](http://xahlee.info/kbd/keyboard_hardware_and_key_choices.html<Paste>). If you mess up on a chord (or anything, really), then just press `C-g` ("press g while holding control") to quit.

Installing Evil
---------------

Let's install Evil *first* so that if you end up editing stuff with Emacs accidentally, you'll have a semi-familiar environment that you can use. Here are the instructions, copied straight from [the Evil homepage](https://www.emacswiki.org/emacs/Evil): start by adding this to your `.emacs`:

    (require 'package)
    (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
    (package-initialize)

Then you can either restart Emacs OR `M-x load-file RET ~/.emacs RET` (which will load the given Emacs Lisp file, in this case your main config file). Then:

    M-x package-refresh-contents
    M-x package-install RET evil

(you should be hitting enter/return at the end of each line) Now, at this point, you should be able to do `M-x evil-mode` to start Evil, and then be able to move around with hjkl and all that jazz. Good job! Now we just need to tell Emacs to start Evil every time. Edit your .emacs (at the end, preferrably, like with everything else):

    (require 'evil)
    (evil-mode 1)

Use `:q!` to quit Emacs and restart, or just `M-x load-file RET ~/.emacs`, and you should (still) have Evil working.

Installing SLIME
----------------

Now we get to the fun part, installing SLIME. Well, technically, the fun part is *using* SLIME, but close enough. Open up SBCL for a second:

    (ql:quickload :quicklisp-slime-helper)

Wait for Quicklisp to do it's thing, then append the following to your `.emacs`:

    (load (expand-file-name "~/quicklisp/slime-helper.el"))
    ;; Replace "sbcl" with the path to your implementation
    (setq inferior-lisp-program "sbcl")

Now back to Emacs:

    M-x load-file RET ~/.emacs
    M-x slime

If everything works, then you should a SBCL prompt in the bottom half of your Emacs window, waiting for you to do something. Try executing something trivial like `(+ 1 2)` to make sure it works. If it does, time to celebrate!

Handy shortcuts
---------------

    M-x slime		; start slime
    C-x 0		; delete a window (like a split in vim)
    C-x <left>		; go back one buffer
    C-x <right>		; go forward one buffer
    C-x b BUFFERNAME	; switch to the buffer with the given name
    , sayoonara		; quit SLIME
    C-c C-k		; compile and load an entire Lisp file
    C-c C-c		; compile whatever Lisp function your cursor is on
    C-c C-z		; switch from your current Lisp file to the SLIME buffer
    C-x o		; switch to the other window
    C-h t		; Emacs tutorial
    C-h k CHORD		; describe what a particular key chord does
    C-h f FUNCTION	; describe what a particular Emacs Lisp function does
    C-h b		; list all keybindings for the current combination of major and minor modes

At this point, you should be ready to *begin* hacking with Emacs, Evil, and SLIME. You are probably going to have a ton of questions, though. Please ask, that's what I'm here for! Additionally, `M-x package-install RET rainbow-delimiters RET M-x rainbow-delimiters-mode RET`

Good luck, have fun!
