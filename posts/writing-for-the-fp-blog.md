---
title: Writing for the FP blog
date: 2017-07-11
project: infra
authors: dlaing
---

## Introduction

This blog is built using [Hakyll](https://jaspervdj.be/hakyll/), a static site generator written in Haskell.

If you haven't used Hakyll before, this will walk you through what you need to know to get going.

If you have used Hakyll before, there are some customizations that we're using that you should probably know about - so keep reading.

## How to write a blog post

The first step is to checkout the blog from [github](https://github.com/qfpl/blog).

The blog posts are all in the `posts` directory, so that's where we add new ones.

They are written using [Pandoc's Markdown](http://pandoc.org/MANUAL.html#pandocs-markdown) syntax, and have a property block at the top of them:

```
---
title: Using Hakyll for the FP blog
date: 2017-07-11
project: infra
authors: dlaing
---
```

The `title` and `date` fields are mandatory - the blog will fail to build if you leave them out, but it'll let you know why.

The `project` field is optional

## How to test out changes locally

## The proposed Hakyll workflow

## Adding a new author

## Adding a new project


