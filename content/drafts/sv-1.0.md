---
title: Announcing sv 1.0
date: 2018-07-09
authors: gwilson
project: sv
---

sv, a CSV library for Haskell by QFPL, was released over four months ago.
Since then, we've had feedback on what potential users want from the library,
and we have altered the design to the point where we are now releasing sv 1.0

## The Story Thus Far

With sv, we wanted a library that could parse a CSV file and build a data
structure which preserved all syntactic information about the input document.
This allows users to manipulate this representation using lenses and output a
new file that is minimally different from the input. This allows a user to
write custom sanitisation tools and linters for their data.

We wrote such a data structure, and gave it a parser with the
[parsers](https://hackage.haskell.org/package/parsers) package.
`parsers` abstracts over the common parsing primitives and combinators, allowing
someone to instantiate a `parsers` parser to other parsing libraries like
Attoparsec, Trifecta, or Parsec. sv exposed its parser so that a user could use
it as a component of some larger parser. Since it uses `parsers`, our parser is
likely to work with whatever parser library the user is using.

sv was not only for writing custom liniters and sanitisation tools. It was also
for writing decoders, to extract values from CSV documents into Haskell
programs. Importantly, sv does not use type classes for this, but rather an
Applicative combinator-based approach. This allows users to pass around and
manipulate decoders as values, and create multiple decoders of each type.

## What To Do

The performance of sv's parser is
[very slow](https://github.com/haskell-perf/csv/tree/25493c61733f6b2b69a4378313af2801f1cceb3b),
mainly due to extravogent memory usage. There are a number of things we could
do to improve this. One is to be less polymorphic. Instead of using `parsers`,
we could use attoparsec or a similar library directly. Attoparsec is very fast
due to its bytestring-oriented primitives. Since most parser combinator
libraries do not support these, `parsers` does not abstract over them, so
sv's parser does not use them. We could most likely adapt a fork of cassava's
parser as a good starting place for an attoparsec-based parser.

Other than performance, there was also
[keen interest](https://github.com/qfpl/sv/issues/10)
in streaming support, meaning to parse and decode a file without keeping the
whole thing in memory at once. This is appears very difficult to integrate into
sv without significant changes to its syntax tree, and even that might require
compromises on how much syntactic information the user has access to.

## The Times They Are a-Changin'

At YOW! Lambda Jam 2018 in Sydney, I gave a talk about the design of sv. This
prompted many useful conversations about sv throughout and following the rest
of the conference. The key insight I derived was this: sv's two broad goals were
at odds with each other. From a decoding library, users demand speed and
streaming support. By giving sv a syntax-preserving representation and
making its parser as generic as possible, it can also serve as a toolkit for
building custom linting and sanitisation tools, but it is much harder to provide
decoding users the speed they crave. I have decided that a sensible way forward
is to cleave sv in twain. Hence the release of sv 1.0 will include a decoding
library and a syntax-preserving library, rather than one library to cover both
uses. The decoding library will keep the `sv` name, and the syntax-preserving
library will be named `svfactor`, to rhyme with "refactor".

Edward Kmett suggested I speak to John Ky about his high
performance CSV parser: [hw-dsv](https://hackage.haskell.org/package/hw-dsv).
This library uses rank-select data structures to index into CSV documents and
offers both a strict and lazy (streaming) interface. After I had Edward Kmett
explain succinct data structures to me, and after further conversations with
John, I was very keen to play around with this library as a new parser for
sv. And that is what I have done. sv's decoding layer now sits atop `hw-dsv`.

## Future work

sv is still missing key features, such as column-name-based decoding.
Furthermore its encoding is likely to still be slow. I intend to work on these
features.

svfactor's parser still has the performance problems I've described, so it
should be rewritten, probably with attoparsec or similar. It is likely that I
will base this effort on a fork of cassava's parser. It is unlikely that I will
work on streaming support for svfactor.
