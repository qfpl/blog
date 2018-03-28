---
title: sv - Introduction, Status, and Road-map
date: 2018-03-28
authors: gwilson
project: sv
---

## What is sv?

`sv` is QFPL's new CSV library for Haskell.

The core data structure of `sv` is a syntax tree for CSV that preserves
white-space, quoting information, newline style, and other data that is usually
thrown away. This gives the property that parsing a file followed by printing
it out will return the original file unaltered: `print . parse = id`

`sv` gives you tools to work with this data structure in a number of ways.
Primarily, `sv` offers four things: parsing, decoding, encoding, and printing.

* Parsing is process of reading a textual representation of a CSV
document into the `Sv` data structure.
* Decoding is extracting from a value of type `Sv` into domain-specific types defined by the user.
* Encoding is constructing a value of type `Sv` from domain-specific types.
* Printing is serialising a value of type `Sv` as textual data.

This terminology should be used consistently in the documentation for `sv`, as well as in its
identifier names, and in these blog posts.

<image src="/images/posts/sv/parsedecodeencodeprint.png" alt="parse, decode, encode, print"/>

A key feature of the `sv` library is its separation of these phases. You can use any of
these phases independently of one another. The consequences of this are many and exciting.

The parser in the `sv` library will parse [RFC 4180](https://tools.ietf.org/html/rfc4180) CSV
files, but it is much more flexible in what it allows. For example, `sv` is
tolerant of spacing around fields. This means that files which use spacing to
horizontally align their data will be accurately handled in `sv`.
`sv` parses non-rectangular data sets, and gives users tools for decoding them.
By "non-rectangular", we mean that each row may not have the same number of
fields.

`sv` notably does not use type classes for decoding nor encoding. We use an
`Applicative` domain-specific-language for describing how to decode data.
This means
we can have multiple decoders for the same type. `Decode` being a first-class
value means we can write interesting transformations and combinators on them
that we couldn't do with a type class instance. We also avoid the dreaded orphan
instance problem.

We are planning more blog posts that will explore some of the benefits of the design choices
we've described here.

## Status

[`sv-0.1`](https://hackage.haskell.org/package/sv-0.1) is on hackage, so you can use `sv` today.

The Parse and Decode aspects of `sv` are considered ready for real-world use. I believe they
are tested and benchmarked sufficiently. By building its parser with `parsers`,
`sv` lets you use your favourite parser library
for the parsing phase. `trifecta` is the default parsing library due to its helpful error
messages, but others can be used, such as `attoparsec`.
Whether with trifecta or with attoparsec, the parser in sv is considerably slower than the 
parser in the `cassava` library.
Exploring this difference will likely be the basis of a future blog post.

The Encode and Print phases are tested but have not been benchmarked, so it is not known
whether they perform reasonably.

`sv` is new, and its API is likely to change. The changelog will be kept up to
date as the library changes. Releases will follow the
[PVP](https://pvp.haskell.org),
so you can keep yourself safe from breaking change by specifying appropriate
version bounds in your cabal file like so: `sv >= 0.1 && < 0.2`.

`sv` does not yet offer any kind of streaming, which means there's a limit on the
size of files that it can practically be used with. This limit depends on your
system memory.

## Where to from here?

`sv` is a young project. The following is a collection of my thoughts on where
to take `sv` from here. If something is missing from this list, please
[open an issue](https://github.com/qfpl/sv/issues/new)
on Github.

Based on my conversations so far with CSV library users, the very next feature
needed is [column-name-based decoding](https://github.com/qfpl/sv/issues/6).
Currently `sv` only has position-based decoding, where we decode a
field based on its horizontal position. Users of cassava or similar libraries
also like to decode based on column names as they appear in the header of the
document. An advantage of this style is that your `Decode`s are resilient
to changes in column ordering.

`sv` needs [streaming](https://github.com/qfpl/sv/issues/10) to support very
large files without eating all of the system's memory.

It would be preferable to have better performance in `sv`, particularly in the
parsing phase.
To lessen this pain for now, you can get the benefit of `sv`'s decoding
without paying for its parser's performance by
[substituting cassava's parser](https://hackage.haskell.org/package/sv-cassava).
That means you can trade off between `sv`'s flexibility and cassava's speed.
It is preferable to have a faster parser in `sv` itself, but other future plans
have a higher priority.

`sv`'s encoding is powered by `Contravariant` and its relatives `Divisible` and
`Decidable`, but I think these aren't particularly well known. I'd like to write
and talk about them, and I'm interested in how they could be
made easier to work with; for example by giving them good infix operator support.

`sv` collects all errors that occur during decoding; not just the first one.
Although they're
collected, these currently aren't very useful because they don't include the
[source location](https://github.com/qfpl/sv/issues/5)
at which they occurred. This should definitely be fixed. It would also be good
to fold together multiple errors of the same sort. For example, if the same
error occurred on ten different lines, you only really need to see its message
once, along with all the lines on which it occurred.

There are other things already listed on the
[issue tracker](https://github.com/qfpl/sv/issues/), and you're welcome to add
your own.

In the longer term, I'd like to build libraries on top of the `sv` library for
higher-level functionality. I think we could take advantage of `sv`'s error
aggregation to build tooling to detect common problems with data sets and
recommend actions to take.

