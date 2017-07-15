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

They are written using [Pandoc's Markdown](http://pandoc.org/MANUAL.html#pandocs-markdown) syntax, which is worth getting familiar with.

There is also support for `LaTeX`.
This can include the use of arbitrary packages, but there are currently a few steps involved in adding new packages.
I'll work on making this a bit smoother shortly.

### The property block for blog posts

The posts need to start with a property block, that looks like this:
```
---
title: Using Hakyll for the FP blog
date: 2017-07-11
project: infra
authors: dlaing
---
```

The `title` and `date` fields are mandatory.
The blog will fail to build if you leave them out, but it'll let you know why.

The `project` field is optional.
If it is set, it should be set to the nickname of the relevant project.

If the field is present, the post will be linked from the main page for the project.
The post will also have a footer that links to that main page and has a small blurb about it.

The `authors` field is optional.
If it is set, it should be set to a comma-separated list of nicknames of the authors.

If the field is present, the post will be linked from the main page for each author.
The post will also have footer entries which link to that main pages of each author along with a small blurb.

## How to test out changes locally

If you have nix installed, you can use `nix-shell` to get into an environment that is ready to go.
This will include `LaTeX` and the various packages needed by the blog.

From there you can run
```
cabal build
```
to create the site generator.

After that you can run
```
./dist/build/site/site build
```
to build the site in `_site`, or
```
./dist/build/site/site watch
```
to set up a preview server on `localhost:8000` that will update as you work on your posts.

If you just want to get hold of a build of the site you can use `nix-build`.
After that has finished running, the blog will be in `./result/blog`.

## The proposed Hakyll workflow

Once we have the blog managed by Hydra, you should be able to publish posts by pushing your changes to the `master` branch of the repository.
From there, Hydra will build the site generator, generate the pages, and push them to the `gh-pages` branch so that they appear on our public facing site.

If you are working on a draft post, then it is recommended that you do so in a branch so that you don't end up with half-finished posts being published.

If you are working on something that is sensitive or embargoed, then it is recommended that you do so in a private repository until the embargo is lifted or the post is no longer sensitive.

## Adding things

### Adding a new author

There are three things to do to add a new author to the blog.

At the moment these are pretty free-form, but in the future we might add more structure for standard sections / features - like photos of the team members, etc...

#### 1. Write an author page

Write a page about the author at `./people/<author-nickname>.md` with the following property block at the start of the document:
```
---
title: <Author name here>
---
```

#### 2. Write a snippet for the page listing the team members

This goes in `./snippets/people/<author-nickname>/page.md` and requires the following property block at the start of the document:
```
---
title: <Author name here>
order: <What position on the page the blurb will appear at>
---
```

The blurbs will be sorted in ascending order by the numerical value of the `order` property.
It is probably worth using the highest number not already taken.

#### 3. Write a snippet for blog posts

This goes in `./snippets/people/<author-nickname>/posts.md` and requires the following property block at the start of the document:
```
---
title: <Author name here>
---
```

### Adding a new project

There are three things to do to add a new project to the blog, and they are strikingly similar to the steps for adding a new author.

At the moment these are pretty free-form, but in the future we might add more structure for standard sections / features - like project logos, etc...

#### 1. Write a project page

Write a page about the project at `./projects/<project-nickname>.md` with the following property block at the strat of the document:
```
---
title: <Project name here>
---
```

#### 2. Write a snippet for the page listing the current projects

This goes in `./snippets/projects/<project-nickname>/page.md` and requires the following property block at the start of the document:
```
---
title: <Project name here>
order: <What position on the page the blurb will appear at>
---
```

The blurbs will be sorted in ascending order by the numerical value of the `order` property.
It is probably worth using the highest number not already taken.

If we want to highlight a particular project we might shuffle the order around by changing this property for several projects.

#### 3. Write a snippet for blog posts

This goes in `./snippets/projects/<project-nickname>/posts.md` and requires the following property block at the start of the document:
```
---
title: <Project name here>
---
```

### Adding a new `LaTeX` package

At the moment this is a bit manual.

The blog currently makes the `prftree` package available.

If you want to add a new LaTeX package, you can add it to the lists of packages in `default.nix` and `src/Site.hs` that already contain `prftree`.

This will be made a bit easier later on.

## Conclusion

Blog early and often.
Just have fun with it.
