---
title: Announcing sv 1.0
date: 2018-07-20
authors: gwilson
project: sv
---

`sv`, a CSV library for Haskell by the Queensland Functional Programming
Lab, was released over four months ago.
Since then, we've had feedback on what potential users want from the library,
and we have altered the design to the point where we are now releasing a new
version: `sv 1.0`

## The Story Thus Far

With `sv`, QFPL wanted a library that could parse a CSV file and build a data
structure which preserved all syntactic information about the input document.
This allowed users to manipulate the document using lenses and output a
new file that was minimally different from the input. With this, a user could
write custom sanitisation tools and linters for their data.

We wrote such a data structure, and gave it a parser with the
[parsers](https://hackage.haskell.org/package/parsers) package.
`parsers` abstracts over the common parsing primitives and combinators, allowing
someone to instantiate a `parsers` parser to other parsing libraries like
`attoparsec`, `trifecta`, or `parsec`. `sv` exposed its parser so that a user
could use it as a component of some larger parser. Since it used `parsers`,
our parser was likely to work with whatever parser library the user is already
using.

`sv` was not only for writing custom linters and sanitisation tools. It was also
for writing decoders to extract values from CSV documents into Haskell
programs. Importantly, `sv` did not use type classes for this, but rather an
Applicative combinator-based approach. Users could pass around and
manipulate decoders as values, and create multiple decoders of each type.

All was not well. The performance of `sv`'s parser was
[very slow](https://github.com/haskell-perf/csv/tree/25493c61733f6b2b69a4378313af2801f1cceb3b),
mainly due to extravogent memory usage. There was also
[keen interest](https://github.com/qfpl/sv/issues/10)
in streaming support, meaning to parse and decode a file without keeping the
whole thing in memory at once. This appeared very difficult to integrate into
`sv`'s design without significant changes to its syntax tree.

## The Times They Are a-Changin'

At YOW! Lambda Jam 2018 in Sydney, I (George Wilson) gave a talk about the design of `sv`. This
prompted many useful conversations about `sv` throughout and following the rest
of the conference. The key insight was this: `sv`'s two broad goals were
at odds with each other. From a decoding library, users demand speed and
streaming support. By giving `sv` a syntax-preserving representation and
making its parser as generic as possible, it can also serve as a toolkit for
building custom linting and sanitisation tools, but it is much harder to provide
decoding users the speed they crave. I have decided that a sensible way forward
is to split up `sv` into a handful of libraries. The decoding will be paired
with a parser of much higher performance that does not keep all syntax
information. Syntax-preserving manipulation will be available separately and
for now will retain its performance problems.

Edward Kmett suggested I speak to John Ky about John's high performance CSV parser,
which has now been released to hackage as
[`hw-dsv`](https://hackage.haskell.org/package/hw-dsv).
This library uses rank-select data structures to index into CSV documents and
offers both a strict and a lazy (streaming) interface. After Edward
explained succinct data structures to me, and after further conversations with
John, I was very keen to play around with this library as a new parser for
`sv`. And that is what I have done. `sv`'s decoding layer now sits atop `hw-dsv`.

The release of `sv 1.0` will be structured as follows:

* __`sv-core`__:    The Decoding/Encoding of `sv`, agnostic of any parsing
* __`sv`__:         `sv-core` atop its new default parser (`hw-dsv`)
* __`sv-cassava`__: `sv-core` atop `cassava`'s parser instead
* __`svfactor`__:   The syntax-preserving parsing/printing/manipulation of
  `sv 0.1`, packaged as its own library, with no dependency on any other `sv`
  package.
* __`sv-svfactor`__: `sv-core` atop svfactor, for those wanting the behaviour of the
  earlier `sv` version.

Most users interested in decoding and encoding should use `sv`, being careful to
set the right cabal flags as explained in the README and cabal package
description in order to get the best performance. Those running GHC
versions below 8.4 will get the better performance by using `sv-cassava` instead.
Those interested not in decoding nor encoding, but interested in a
syntax-preserving CSV datastructure can use `svfactor`. Finally, anyone
interested in writing decoders which depend on structural information of the
CSV, as was the case in the first release of `sv`, can use `sv-svfactor`.

Alternatively, you can use `sv`'s decoders atop `cassava`'s parser by using
[sv-cassava](https://hackage.haskell.org/package/sv-cassava) instead of
sv.

## Future work

`sv` is still missing key features, such as column-name-based decoding. Although
the parser is streaming, `sv` itself still is not.
Furthermore, encoding data as CSV is likely to still be slow. I intend to work
on these features and performance concerns.

`svfactor`'s parser still has performance problems, so it should be altered or
rewritten.
When I get around to this, I intend to blog about it. I also intend to blog
about benchmarking `sv` soon.

## Links and Further Reading

* [An example](https://github.com/qfpl/sv/blob/master/examples/src/Data/Sv/Example/Species.lhs)
  of using sv to decode a real CSV file
* [hw-dsv](https://hackage.haskell.org/package/hw-dsv)
* [sv](https://hackage.haskell.org/package/sv)
* [svfactor](https://hackage.haskell.org/package/svfactor)
* [sv-cassava](https://hackage.haskell.org/package/sv-cassava)
