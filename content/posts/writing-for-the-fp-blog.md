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

If you're working on a draft that you want to share with people in order to get feedback, you can put it in the `drafts` directory.
It will be treated just like a regular post, but it won't be linked from any of the other pages.

Similarly, you can add content to the `links` directory if you have information that you want to share with people that would be noisy or off-topic if it were linked from the other pages.

In either case, the content will be viewable by anyone who has the URL of the content (or anyone clicking through the github repository for our blog), but otherwise won't be discoverable by people clicking around on the blog.

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

There are two directories in the blog repository.

The `generator` directory has the site generator, and the `content` directory has the site content.

If you have Nix installed, you can use
```
nix-build release.nix -A generator
```
to build the site generator, which will be available in `result/bin/site`.

After that you can head to the `content` directory and run
```
../result/bin/site build
```
to build the site in `_site`, or
```
../result/bin/site watch
```
to set up a preview server on `localhost:8000` that will update as you work on your posts.

## The Hakyll workflow

You should be able to publish posts by pushing your changes to the `master` branch of the repository.

The rest is taken care of by Hydra (a continuous integration system for Nix) and Hail (a continuous deployment system for Nix).

From there

  - Hydra will build the site generator and use that to generate the site
  - Hail will pick up the changes and update the content of the blog

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

### Adding a talk

Copy the contents of the talk to `./share/talks/<talk-nickname>`, and then write a file in `./talks/<talk-nickname>.md` that looks like this:
```
---
title: "Reflex: front-end development done awesome"
date: 2017-07-18
venue: BFPG
venue-link: http://bfpg.org
author: dlaing
author-name: Dave Laing
project: reflex
project-name: Reflex
code-url: https://github.com/qfpl/intro-to-reflex-bfpg
slides-base: slides.html
---
```

The only mandatory fields are:

- `title`
- `date`
- `author` and `author-name`

Everything else will get displayed or linked to if it is mentioned.

The talk will only be linked to if `slides-base` is specified, in which case the link will be to `./share/talks/<talk-nickname>/${slides-base}`.

#### Adding Piwik tracking to a talk

If you want to track how many people have viewed a talk and for how long, you should add a snippet of JS to talk to Piwik.

For a HTML talk, add this snippet somewhere in the head of the HTML:
```
        <!-- Piwik -->
        <script type="text/javascript">
          var _paq = _paq || [];
          /* tracker methods like "setCustomDimension" should be called before "trackPageView" */
          _paq.push(['enableHeartBeatTimer', 30]);
          _paq.push(['trackPageView']);
          _paq.push(['enableLinkTracking']);
          (function() {
            var u="//qfpl.qfpl.io/";
            _paq.push(['setTrackerUrl', u+'piwik.php']);
            _paq.push(['setSiteId', '1']);
            var d=document, g=d.createElement('script'), s=d.getElementsByTagName('script')[0];
            g.type='text/javascript'; g.async=true; g.defer=true; g.src=u+'piwik.js'; s.parentNode.insertBefore(g,s);
          })();
        </script>
        <!-- End Piwik Code -->
```

If you have a PDF of your slides, you'd have to write a landing page that includes the snippet.
In the future we might develop a standard page that contains the above snipped and redirects to the PDF when the page loads to help out with that.

### Adding a new `LaTeX` package

At the moment this is a bit manual.

The blog currently makes the `prftree` package available.

If you want to add a new LaTeX package, you can add it to the lists of packages in `default.nix` and `src/Site.hs` that already contain `prftree`.

This will be made a bit easier later on.

### Adding other content

Anything in the `./share` directory will be served up as static content.

For example, the talk contents are available from `https://blog.qfpl.io/share/talks/<name of talk>`.

## Conclusion

Blog early and often.
Just have fun with it.
