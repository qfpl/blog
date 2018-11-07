---
title: Waargonaut The JSONer
date: 2018-11-05
authors: schalmers
---

## Introductions

Waargonaut is a Haskell library for encoding/decoding/manipulating JSON. The design and development
of which has been driven by a dissatisfaction with the current status quo of JSON libraries in
Haskell. Part of the problem is choice, in that there isn't any. If you needed to handle JSON data
in Haskell, without rolling your own package, the _choice_ was to use
[aeson](https://hackage.haskell.org/package/aeson). 

It does need to be said that this was **not** a poor choice. As `aeson` is more than capable of
handling almost any JSON you care to mention. It can also be massaged into dealing with the edge
cases, generally. It presents a straightforward API, excellent documentation, and it's fast.

"So what's the problem", I hear you ask. The problem is _flexibility_.

* The use of typeclasses as the primary mechanism. Although convenient in that you have an
  overloaded name. Do not permit the flexibility often required of encoding/decoding tasks and
  you will find yourself having to undergo all sorts of contortions to achieve your goal.

* You must use the parser chosen by `aeson`. There are good reasons to only support a single parsing
  library. But nearly all of those reasons may be just as valid a reason to leave that choice to the
  final user. Such as how informative the error messages are. Or handling of tricky edge cases, such
  as handling idiosyncrasies of your input.

* There are ecosystem implications of having "only one good option". Swathes of packages are built
  upon a single point in design space and are thus constrained. This is a reasonable situation when
  considering packages such as `semigroups`, or `mtl`. Packages that may use the phrase, in the
  mathematical sense, "by definition". For a problem space as varied as encoding/decoding data this
  restriction may make handling your data more difficult than necessary.

To address this issue of flexibility: Waargonaut, where possible, will defer the choice to users. Or
if such a thing is not possible, the design is intended to be malleable enough that you are able to
extend the existing functionality. Alternatively there are enough of the internals exposed so you
may create your own solution to the problem.

### Proper Properties Providing Powerful Provisions

The `Json` data structure within Waargonaut allows it to keep track of all whitespace, as well as
the presence or lack of trailing commas in objects and arrays. 'Why bother?' you ask, why store all
of that extra information?

For the combination of a parser and printer, there are properties that when proven to hold, provide
immense guarantees about the accuracy and robustness of both functions. This is called the 'round
trip' property, which may be expressed in two ways, or directions, if you will. Each offers a
different level of assurance.

#### Hide no truths (`print . parse = id`)

Firstly, and to help explain why we have the ability to track all of the whitespace and comma
information. We will express the 'round-trip' property (using pseudo-code) as:

```
print . parse = id
```

This means you are able to parse JSON to a `Json` structure, change a value and then print the
`Json`, the only thing that will change is the value that you've updated. Nothing else. All of the
whitespace, indentation, trailing commas etc, are _all_ preserved. 

This may seem like a lot of unnecessary work if your use-case is simply a `JSON <-> DB` pipeline.
But remember, _flexibility_ is the goal here. There are uses beyond a quick visual debugging of your
data.

* A system that acts as a verification pipeline will want to be absolutely confident that their
  operations will not disrupt or alter data in an unwanted or unexpected fashion.
* A tooling developer that wants to build an analyser or command line update application won't have
  to patch over the preservation of this 'extraneous' information. 
* A process for migrating schemas that are codified as JSON documents can rely on this package to
  not alter anything unless explicitly instructed.
* As a library developer I won't have to try to shoe-horn such capability into the design
  after-the-fact.

Generally speaking, if you consider the robustness of a parser/printer that is able to satisfy this
property, then you're able to much more confidently rely on the integrity of its operations. To
reiterate, if you don't care about such information, this library won't force you to.

#### Tell no lies (`parse . print = id`)

When flipped around we have the following property, as pseudo-code:

```
parse . print = id
```

This property states that given _ANY_ `Json` (the main JSON data type in Waargonaut) structure. If
we `print` and then `parse` it, the result will precisely match the structure we started with.

This property is tested using the [Hedgehog](https://hackage.haskell.org/package/hedgehog)
property-based testing library. Which helps to generate randomised `Json` values and applying the
`Encode` / `Decode` functionality. Before checking the results and failing the test if this property
does not hold.

This particular property is the absolute minimum you would expect a parser/printer pair to be able
to satisfy. It shakes out surprising bugs in both the parser and the printer as any inconsistencies
are immediately identified.

#### More information

If you're interested in finding out more about this property then the [blog post by Tim
Humphries](https://teh.id.au/posts/2017/06/07/round-trip-property/) is an excellent starting point.

### Encoder/Decoder as values

Waargonaut defines the encoding and decoding process for any given type as a function. _From_, or
_possibly to_, that type:

```haskell
Decoder f a :: ParseFn -> JCurs -> DecodeResultT Count DecodeError f a
--
Encoder f a :: a -> f Json
```

These functions are values and may be passed around and manipulated in the same way. Because they
are not tied to a typeclass, you may have as many variations of encoders or decoders for a type as
you require.

```haskell
int :: Decoder f Int
list int :: Decoder f [Int]
--
int :: Encoder f Int
text :: Encoder f Text
```

To elaborate, if you have some type `Foo`, defining a typeclass for a `Decoder` implies that there is
one canonical way of decoding some JSON to a `Foo`. This frequently ends up not being true in the
general case. To give a concrete example, we will have a look at an optional value: `Maybe a`. Shall
we see if we can establish a single indisputable way to decode a `Maybe a` value?

This is almost immediately impossible if we ask the question: "What does it mean for this value to
be optional?". Consider the following scenarios:

```
null                   ~ Nothing
""                     ~ Nothing
""                     ~ Just ""
{"foo": null }         ~ Nothing
{"foo": null }         ~ Just ""
{"foo": null }         ~ Just Null
{"foo": "" }           ~ Nothing
{"foo": "" }           ~ Just ""
{"foo": "" }           ~ ("foo", Nothing)
{"foo": "" }           ~ ("foo", Just "")
{"foo": "" }           ~ A { _foo = Nothing }
{"foo": "" }           ~ A { _foo = Just "" }
{"foo": "" }           ~ Nothing
{"bar": null }         ~ Nothing
{"bar": null }         ~ B { _a = Nothing }
{"bar": {"foo": null}} ~ B { _a = Just (A { _foo = Nothing })}
{"bar": {"foo": null}} ~ B { _a = Just (A { _foo = Just Null })}
``` 

The same problem also exists in reverse when encoding a value. But such is the nature of decoding
and encoding data. In Waargonaut, with encoders and decoders as values, this problem changes to one
of the library providing a sufficiently expressive API, instead of presuming canonical truths.

#### But Waargonaut has typeclasses. What gives?

You are correct, dear questioning entity. Waargonaut does indeed contain two typeclasses, subtly
named `JsonEncode` and `JsonDecode`. These are included specifically for the `Waargonaut.Generic`
functionality. They are designed in such a way as to minimise their impact on the flexibility
provided by the library at large. In fact, Waargonaut doesn't even need them!

We will cover these in more detail later, but the primary differences are:

* The `Generic` functionality will only create the encoder or decoder function. They create a value,
  they are to execute the encode/decode process.
* The typeclasses require an additional type parameter that is then applied to the value they
  produce. Meaning that you must differentiate your instances and the type system will represent
  this.
* Unless you're constrained by another library, you may use Waargonaut without ever needing the
  typeclasses. If you don't use the `Generic` functionality, you won't use the typeclasses.

#### No More Orphan Instances

The flexibility provided by having encoders and decoders as values is that [orphan
instances](https://wiki.haskell.org/Orphan_instance) are a thing of the past. Also users do not have
to create newtypes every time they wish to encode or decode a value to suit their own needs. If you
have a specific requirement for decoding `Text` values, then having your own decoder is no problem.

Even when writing instances of `JsonEncode` or `JsonDecode`, the extra type parameter, intended for
users to use to tag their own custom instances with a type. Allowing you to have similar levels of
flexibility to the standard encoder/decoder functions, without orphan instances or newtypes.

### Zippin' about the JSON

Waargonaut provides a zipper based decoder, built upon a succinct data structure index of the
underlying JSON input. In this section we'll go over an example of how the `Decoder` works. How do
you access parts of the JSON? How do you decode specific values to the types you require?

Creating a `Decoder` starts with function that provides a cursor or "focus". Combined with the
aforementioned expressive API, you navigate the structure and pick off the pieces you require.

Taking the example from the [README](https://github.com/qfpl/waargonaut/blob/master/README.md) we
will use the following Haskell data structure:

```haskell
data Image = Image
  { _imageWidth    :: Int
  , _imageHeight   :: Int
  , _imageTitle    :: Text
  , _imageAnimated :: Bool
  , _imageIDs      :: [Int]
  }
```

Assuming we have the following JSON as input:

```json
{
  "Width":  800,
  "Height": 600,
  "Title":  "View from 15th Floor",
  "Animated" : false,
  "IDs": [116, 943, 234, 38793]
}
```

We will create our a `Decoder f Image` by using the `withCursor` function to give us the cursor into
our JSON input:

```haskell
imageDecoder :: Monad f => Decoder f Image
imageDecoder = withCursor $ \c -> ...
```

Now we have to move the cursor to the correct position, and use the `Decoder`s required for the
relevant types. There are plenty of helper functions in Waargonaut. And I hope enough moving parts
exposed so you can create any new ones that you may need.

Every `Decoder` must make an assumption about where the cursor will be positioned. As when you are
navigating the JSON input and you pass the cursor to a `Decoder`, you are stating that the JSON that
the `Decoder` expects can be found at that position. When writing a `Decoder` that is pulling values
of a JSON object, a convenient expectation is that the cursor will be located at the very top or
outer position of that object.

When you initially create the cursor for any JSON input using `mkCursor`, the cursor will always be
placed at this very outer position. This position is indicated by `**` in the following JSON:

```json
{**
  "Width":  800,
  "Height": 600,
  "Title":  "View from 15th Floor",
  ...
```

From here we need to move `down` into the object:

```haskell
imageDecoder :: Monad f => Decoder f Image
imageDecoder = withCursor $ \c -> do
  c' <- down c
  ...
```

We need to move `down` because the cursor is able to step over the entire object in one movement.

Now the cursor is positioned at `**`:

```json
{
  **"Width":  800,
    "Height": 600,
    "Title":  "View from 15th Floor",
    ...
```

Now it is a matter of moving between the keys, seeing if it is one that we're interested in, then
moving to the value to apply the right `Decoder`. That's quite the faff, so there are functions for
this.

The `fromKey` function instructs the cursor to search through the keys to find a match, then apply
the given `Decoder` to the value. Then using the normal abstractions we've come to know and love, we
can combine the result into our `Image` data type:

```haskell
imageDecoder :: Monad f => Decoder f Image
imageDecoder = withCursor $ \c -> do
  c' <- down c
  Image 
    <$> fromKey "Width" int c'
    <*> fromKey "Height" int c'
    <*> fromKey "Title" text c'
    <*> fromKey "Animated" bool c'
    <*> fromKey "IDs" (list int) c'
```

We can then apply our `Decoder` with a `JCurs` provided by the `mkCursor` function:

```haskell
parseFn :: ByteString -> Either DecodeError Json

>>> print $ runPureDecode imageDecoder parserFn (mkCursor input)
Right (Image {_imageWidth = 800, _imageHeight = 600, _imageTitle = "View from 15th Floor", _imageAnimated = False, _imageIDs = [116,943,234,38793]})
```

The movements can be as granular as you require, allowing for immense flexibility and accuracy in
the types of data you're able to decode using Waargonaut. For example, you may move along a JSON
array and pick off individual elements. For example:

Given this JSON input:

```json
['a',"Fred",2,3,5,6]
```

With this Haskell type:

```haskell
data Foo = F (Char, String, [Int])
```

We move `down` into the JSON array in the `Decoder` and select individual elements before consuming
the rest of the array into its own value:

```haskell
fooDecoder :: Monad f => Decoder f Foo
fooDecoder = withCursor $ \c -> F $ (,,)
  <$> (down c >>= focus char)
  <*> (down c >>= moveRight1 >>= focus string)
  <*> (down c >>= moveRightN 2 >>= rightwardSnoc [] int)
```

Or:

```haskell
fooDecoder :: Monad f => Decoder f Foo
fooDecoder = withCursor $ \c -> do
  hd <- down c
  fstElem <- focus char hd
  sndElem <- moveRight1 hd >>= focus string
  rest    <- moveRightN 2 hd >>= rightwardSnoc [] int
  pure $ F (fstElem, sndElem, rest)
```

We can then combine these to run our `Decoder`:

```haskell
>>> print $ runPureDecode fooDecoder parseFn (mkCursor "['a',\"Fred\",2,3,5,6]")
Right (F ('a',"Fred",[2,3,5,6]))
```

### Creation

Encoding a data type as JSON is the process of how to move from your type to JSON. We'll encode our
`Image` type from before as a JSON object:

```haskell
imageEncode :: Applicative f => Encoder f Image
imageEncode = mapLikeObj $ \img ->

    -- There is a general 'encode at key' function
    atKey "Width" int (_imageWidth img)

    -- There are also some helper functions ( intAt k v = atKey k int v )
  . intAt "Height" (_imageHeight img)

  . textAt "Title" (_imageTitle img)
  . boolAt "Animated" (_imageAnimated img)
  . listAt int "IDs" (_imageIDs img)
```

Similar to the process of running the `Decoder`, you pass the chosen `Encoder` to the run function:

```haskell
>>> print $ simplePureEncodeNoSpaces imageEncode someImg
"{\"Width\":800,\"Height\":600,\"Title\":\"View from 15th Floor\",\"Animated\":false,\"IDs\":[116,943,234,38793]}"
```

The `mapLikeObj` function expects a function that sets various key/value pairs onto a JSON object,
where the uniqueness of the guarantees is enforced. Note the composition of the functions above. The
other functions are to be composed together and are responsible for adding a single key/value pair
onto the object itself.

The `atKey` function:

```haskell
atKey "Width" int (_imageWidth img)
```

This will use the `Encoder f Int` to put the value from `_imageWidth img` at the key "Width" on the
JSON object. Its type signature looks a lot more complicated than that, but really that's all it is
doing.

```haskell
atKey
  :: ( At t
     , IxValue t ~ Json
     , Applicative f
     )
  => Index t
  -> Encoder f a
  -> a
  -> t
  -> f t
```

There are plenty of useful functions to help encode various structures:

```haskell
-- | Build an 'Encoder' using a 'Control.Lens.Prism''
prismE :: Prism' a b -> Encoder f a -> Encoder f b

-- | Encode a 'Maybe' value, using the provided 'Encoder''s to handle the different choices.
maybe :: Encoder f () -> Encoder f a -> Encoder f (Maybe a)

-- | Encode a 'Maybe a' to either 'Encoder a' or 'null'
maybeOrNull :: Applicative f => Encoder f a -> Encoder f (Maybe a)

-- | Encode some 'Traversable' of 'a' into a JSON array.
traversable :: (Applicative f, Traversable t) => Encoder f a -> Encoder f (t a)
```

### JSON RFC Interlude, or "Why is it called 'Map Like'?"

The RFC that Waargonaut complies with (https://tools.ietf.org/html/rfc8259) does not state that JSON
objects must have unique keys. As such Waargonaut does not enforce this either. Instead Waargonaut
provides functionality to handle both use cases. You may decode JSON objects that have duplicate
keys, and vice versa you may encode such objects. Commensurately, if you refuse to accept duplicate
keys then that is up to you too.

Again, the key is _flexibility_. Waargonaut aims to provide you with the tools you need to handle
your JSON data, your way. 

### What are those constraints?

Both the `Encoder` and the `Decoder` have a typeclass constraint of `Applicative` and `Monad`
respectively. This is because you may find yourself in a situation where writing a `Decoder` or
`Encoder` requires extra functionality. You may have special case logging requirements. You might
want to keep track of the length of a list as a failure case for encoding to prevent overloading a
downstream system.

These constraints are left open for users to leverage however they deem necessary. They may also be
specialised to `Identity` if nothing special is required. Waargonaut provides those functions as a
convenience when the constraints are redundant. The innards of the `Encoder` and `Decoder` machinery
are exposed, with some lifting and embedding capability provided.

### Succinctly Awesome

Under the hood of the Waargonaut `Decoder` machinery is the absolutely incredible work by John Ky
and Haskell Works on succinct data structures. This lets Waargonaut provide amazing performance. For
more information about this and succinct data structures in general, I recommend starting with the
[references for `hw-json`](https://github.com/haskell-works/hw-json#references).

## More to come

Waargonaut is still very new, but we believe it delivers on the goals of flexibility, precision, and
positive utility. If you want to give it a try, it is available on:

* [Hackage](https://hackage.haskell.org/package/waargonaut)
*  and the [GitHubs](https://github.com/qfpl/waargonaut)

More information may be found in the Haddock documentation of the library itself. With examples and
even some tests!

This has been a whirlwind tour of the main design ideas behind Waargonaut. In future posts we will
cover:

* The care and feeding of the `Generic` functionality. The `Tagged` type and the benefits of having
  your `Generic` functions producing functions.
* The property driven design that is the foundation of the data structures that make up our `Json`
  structure.

If there are specific things you are interested in knowing more about with respect to Waargonaut
then let us know!

We'll also touch on the [`servant-waargonaut`](https://github.com/qfpl/servant-waargonaut) package
that we have in the works.

Speaking of things in the works, there are still things to be done inside Waargonaut and we heartily
encourage suggestions, issues, and pull-requests!

### A request from us

To anyone working on or maintaining libraries that use JSON in Haskell. We encourage you to embrace
flexibility. Our request is to let it be a decision for your users. Design your library such that
you don't have to care.

Use a typeclass, a record with some functions on it, or "your preferred mechanism here" to allow
your users to choose how the JSON is handled. Leaving you free to concentrate on the functionality
that is important to your library rather than the specifics of any one JSON library.

Only if such a thing is possible of course, we're not your supervisor.
