---
layout: post
title:  "Getting set up with Lispbot"
date:   2016-05-19 23:23:02 -0700
categories: update lisp
---

Overview
--------

This post is merely a step-by-step set of instructions to get set up with hacking on [Lispbot](https://github.com/fouric/lispbot) with Vim and SBCL. If you aren't attending one of my CAT Lisp workshops, then this might not be very helpful for you. You can give it a shot and see if it works for you, though!

Getting SBCL and Quicklisp
--------------------------

Before we even *think* of hacking up some wicked Lisp parens, we need to get our mitts on a copy of SBCL that is new enough that it will work with Quicklisp. Then we'll get Quicklisp. What's that, you say? You don't know what either of those things are? SBCL is a Lisp compiler, and Quicklisp is a package manager (Lisp calls them "systems") for Common Lisp. User: Educated.

    wget http://downloads.sourceforge.net/project/sbcl/sbcl/1.3.5/sbcl-1.3.5-x86-64-linux-binary.tar.bz2
    tar xjvf sbcl-1.3.5-x86-64-linux-binary.tar.bz2
    mv sbcl-1.3.5-x86-64-linux sbcl
    cd sbcl
    ./run-sbcl.sh

At this point, you should be greeted greeted with an *extremely* minimalist REPL (Read-Evaluate-Print-Loop, aka an interactive shell): a single asterisk.

    *

Great! You got SBCL to work. If you do *not* see the above, then please wave your hand in the air like a madpark until I see you and check out what the heck you messed up. If everything looks good, then type in `(quit)` (WITH the parens, this is Lisp after all) and hit enter, we have things to do.

At this point, I would highly recommend editing your .bashrc to include an alias to your SBCL binary:

    alias sbcl="~/sbcl/run-sbcl.sh"

Done? Cool. Source your `.bashrc` (`source ~/.bashrc`) so that you can actually use the alias. Let's get Quicklisp and then use that to install some basic line-editing stuff in this sucker:

    wget https://beta.quicklisp.org/quicklisp.lisp
    sbcl --load quicklisp.lisp
    (quicklisp-quickstart:install)
    (ql:add-to-init-file)

Now, while the third command actually installs Quicklisp on your system, the fourth is important. It causes SBCL to automatically load Quicklisp every time it starts up, which is *definitely* something that you want. Finally, if you want to test to make sure that Quicklisp installed properly, then quit SBCL (`(quit)`), restart it, and run `(ql:update-all-dists)` or something. If Steel Bank freaks out, something broke, blame stenlai. Otherwise, you should be good to go.

Installing Linedit
------------------

You might have noticed at this point that SBCL has zero line-editing features. Actually zero. You get backspace and that's it, no editing anything in the middle of your line for *you*!

Luckily, there is a quick fix, called `linedit`. It's a Quicklisp system. Ah, so now you see why we rushed to get the thing set up as soon as possible. All you need to do is `quickload` it:

    (ql:quickload :linedit)

Then quit SBCL and add the following lines to your `.sbclrc`:

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

Restart SBCL, and you should have a fancier prompt, as well as the ability to do basic stuff such as invoking previous commands. Yay! (the web site for Linedit is http://www.cliki.net/linedit by the way)

Getting Lispbot
---------------

At this point, you should be ready to clone `lispbot` and begin hacking on it. This should be easy enough:

    cd ~
    git clone https://github.com/fouric/lispbot
    cd lispbot
    echo "(:key \"FOO\")" > auth.dat
    sbcl
    (load "lispbot.lisp")
    (lispbot:run "#bar")

(you'll probably want to change "#bar" to be whatever channel you want the bot to join, and FOO to be the chankey, if any)

At this point, Lispbot should join the given channel with a partially-randomized nick. Find your bot and go have a chat with it! The default bot accepts the "source", "hello", and "drop" commands. Give it a shot!

Now We Do Things
----------------

Now you get to hack on your bot! At this point, you should start looking through the source code of the bot and trying to figure out what stuff does. The [Common Lisp Hyperspec](http://www.lispworks.com/documentation/HyperSpec/Front/X_Master.htm) is *the* gold standard for this stuff, but I'm (1) closer and (2) louder, and so more people can learn at once. Please ask questions! I am *intentionally* stopping this guide here because I want you to do so. Don't be afraid to break things, you can always `git reset --hard HEAD` (did I get that right squid) if you really can't fix it. Happy hacking!
