---
title: sv
---

sv (separated values) is a Haskell library for parsing, decoding, encoding,
and printing CSV and similar formats (such as PSV, TSV, and many more).

sv uses an Applicative combinator style for decoding and encoding, rather than a type class based approach. This means we can have multiple decoders for the same type, multiple combinators of the same type, and we never have to worry about orphan instances. These decoders can be stiched together from provided primitives and combinators, or you can build one from a parser from your favourite parser combinator library.

sv returns values for all errors that occur - not just the first. Errors have more structure than just a string, indicating what went wrong.

sv's parser is exposed so you can use it independently of the decoding, and encoding and printing are similarly standalone.

sv focuses on correctness, on flexible and composable data types, and on useful and informative error values. Speed is also important to us, but it is not as important as these other qualities.

sv tries not to be opinionated about how your data should look. We intend for the user to have a great degree of freedom to build the right decoder for their dataset.