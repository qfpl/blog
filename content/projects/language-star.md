---
title: language-*
---

Many programming languages have not been designed with the intent of preventing
developers from making common mistakes, or lack many important features to develop
robust software. Even so, this class of languages receive widespread use by career
developers. The use of such languages in any commercial setting means greater costs,
more bugs, and increased time to market.

Typed functional programming is one of the foundations of building reliable software,
and is undeniably superior to mainstream alternatives. Given this knowledge, we
are presented with the problem of adoption into existing codebases. How can software
developers reap the benefits of a typed functional programming language without
discarding their existing software?

The `language-*` project aims to address this with the development of principled
Haskell libraries to read, manipulate, and write other programming languages.
The  first language a we're targeting is Python, due to its widespread use both
within Data61 and in greater programming community. Our immediate goal is to develop a
correct by construction intermediate representation, along with a parser and printer
that preserves source formatting. After this, we will explore embedded DSLs for writing
Python programs in Haskell, and then the integration of static analysis techniques for
typechecking, optimization and source code linting.

You can view our progress here: [`qfpl/hpython` on GitHub](https://github.com/qfpl/hpython).
