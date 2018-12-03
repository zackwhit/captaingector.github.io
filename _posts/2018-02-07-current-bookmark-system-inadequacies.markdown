---
layout: post
title:  "Current Bookmark System Inadequacies"
date:   2018-02-09 20:08:45 -0700
categories: update organization
---

### The Problem

I have an issue with the bookmark organization model of Firefox (and by extension other major web browsers, as I assume that their models don't differ in any major way). Firefox provides you two types of organizational primitives: folders and tags. Folders are hierarchical - you can nest them, which is very useful for organizing information into ever-more-specific categories. Tags are non-exclusive - you can apply multiple tags to a single bookmark. Unfortunately, there is no organization primitive that combines these two attributes; a bookmark can only exist in a single folder, and tags cannot be nested. The missing primitive means that some bookmark collections (such as mine) become increasingly difficult to manage over time, as there does not exist an appropriate organizational label for them.

### Two Examples

For instance, consider a bookmark library which has two sibling folders: one for holding articles about a particular *subject*, such as electrical engineering or physics, and another for holding articles about *projects* that the owner is working on. If the owner is working on an embedded systems project, articles for embedded systems engineering could reasonably go in either the project folder or the "computer engineering" folder. The organizer would have to make a decision as to which folder the bookmark should reside in - and no matter which choice they make, there will be a scenario where they're searching for the article in the other folder.

A trivial example involving tags is when you have an article on a very specific subject. For instance, a web page on power optimization of CMOS circuits could be tagged as "ECE483" (a class on low-power IC design), "Schemer CPU" (a hypothetical project with a goal of creating a CPU that executes Guile VM bytecodes), and "CPU design" - three separate categories that could not be reasonably worked into a single hierarchical folder tree. However, tags are "flat", and because we cannot nest them, in order for a search for the "computer engineering" tag to turn up this article, the tag would have to be added by hand - even though it's already implied by the "CPU design" tag! In fact, *every* tag that could possibly be used to search for this article should be added - and if the collection owner is sufficiently ambitious enough, then that leads to hundreds of tags, many of which must be applied to the majority of bookmarks, by hand.

### A Possible Solution

One way to solve this problem is to make tags nestible, so that your "CPU design" tag is a child of your "computer engineering" tag. Now, when you search for the "computer engineering" tag, it'll pick up items that were tagged as "CPU design" because of the parent-child relationship between the two. Are any major browser vendors likely to implement this solution? No. Is it the only solution? No. However, it's something to think about when designing another system that organizes information of any kind.
