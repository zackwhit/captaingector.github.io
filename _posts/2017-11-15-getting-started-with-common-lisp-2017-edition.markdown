---
layout: post
title:  "Getting Started With Common Lisp - 2017 Edition"
date:   2017-11-15 20:33:50 -0700
categories: update lisp
---

A Short Introduction
--------------------

This is a short guide to configuring a modest Common Lisp development environment on Ubuntu (and other Debian-based Linuxes), using SBCL as our implementation, Spacemacs as our editor, Quicklisp as our library manager, SLIME for our editor integration, and Linedit to make SBCL's REPL a bit nicer.

Let's Do It
-----------

Start out by installing SBCL, emacs, and git using apt:

    sudo apt install -y sbcl emacs git

Next, we want to get Quicklisp, so that we can easily install a bunch of other Common Lisp libraries:

    wget https://beta.quicklisp.org/quicklisp.lisp
    sbcl --load quicklisp.lisp

That second command will drop you into SBCL's REPL. Run the rest in there, treating `RET` as "hit the RETurn/enter key":

    (quicklisp-quickstart:install)
    (ql:add-to-init-file)
    RET
    (ql:quickload :babel)
    (ql:quickload :linedit)
    (require :sb-aclrepl)

While we have now technically installed Quicklisp and Linedit, neither of them will load by default. Edit your `~/.sbclrc` and add the following at the end of the file:

    ;;; Check for --no-linedit command-line option.
    (if (member "--no-linedit" sb-ext:*posix-argv* :test 'equal)
      (setf sb-ext:*posix-argv*
        (remove "--no-linedit" sb-ext:*posix-argv* :test 'equal))
      (when (interactive-stream-p *terminal-io*)
        (require :sb-aclrepl)
        (require :linedit)
        (funcall (intern "INSTALL-REPL" :linedit)
                 :wrap-current t
                 :eof-quits t
                 :history "~/.linedit_history")))

Now that we have SBCL and Quicklisp all configured, let's set up our editor, Spacemacs:

    git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d

After finishing the clone, run emacs, either through your graphical interface or by running `emacs` in a terminal. The Spacemacs layer will begin to automatically configure itself, and will ask you a few questions. My suggested answers are below:

    What is your preferred editing style?   Among the stars aboard the Evil flagship (vim)
    What distribution of Spacemacs would you like to start with?   The stardard distribution, recommended (spacemacs)
    What type of completion framework do you want?   A heavy one but full-featured (helm)

Next, edit your dotfile with `SPC f e d` (where `SPC` is the space bar). Remove `smartparens` from the `package-selected-packages` list in `dotspacemacs/user-config`, add `common-lisp` to the list of layers in the `dotspacemacs-configuration-layers` list in `dotspacemacs/layers`, and then install the layer with `SPC f e R`.

You should now have Spacemacs, SBCL, Quicklisp, Linedit, and SLIME set up. Launch Spacemacs using its Emacs base, run SBCL using `sbcl` if you just want a bare Common Lisp REPL, invoke Quicklisp from a REPL using `(ql:quickload :foo)` to install package `foo`, and launch slime using `, '` (comma then single-quote) in a `.lisp` file (or manually using `SPC SPC slime`) in Spacemacs.

Happy hacking!
