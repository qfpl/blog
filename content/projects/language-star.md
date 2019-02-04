---
title: language-*
---

Many programming languages have not been designed with the intent of preventing
developers from making common mistakes, or lack important features to develop
robust software. Even so, this kinds of languages are used widely throught the
industry. Using these languages in a commercial setting means greater costs,
more bugs, and increased time to market.

We believe typed functional programming is an important tool for building reusable and
reliable software. Unfortunately, for one reason or another, many developers are stuck
with less helpful languages. Given this, we want to explore how software developers can
benefit from typed functional programming languages without discarding their existing
software.

The `language-*` project is one attempt at an answer, through the development of principled
Haskell libraries to read, manipulate, and write other programming languages.
The first language we're targeting is Python, due to its widespread use both
within Data61 and in the greater programming community. We have developed a
syntax tree with good refactoring support, a parser and printer that preserves source formatting,
and a DSL for writing new Python code. We currently support Python 3.5 syntax, and our next goal
is to add modular support for newer (and older) versions.

You can view our progress here: [`qfpl/hpython` on GitHub](https://github.com/qfpl/hpython).

