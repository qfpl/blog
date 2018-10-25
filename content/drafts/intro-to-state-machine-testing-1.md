---
title: "Introduction to state machine testing: part 1"
date: 2018-06-11
authors: ajmccluskey
---

Testing can be hard. When working in a conventional imperative setting you end up with a soup of
mocks and stubs to hide the state you don't care about, and that gets old fast. When employing
functional programming however, referential transparency has your back. Functions always produce the
same outputs given the same inputs. We can choose, or better yet randomly generate, inputs for each
function, and check that the corresponding outputs match our expectations. Simple. That is until we
realise that we need to test more than each function in isolation. Yes, each function must work as
expected, but the system formed through composition of these functions must also be tested to ensure
that our system does "the right thing". This puts us at the boundaries of our application, where we
no longer talk about function calls, but HTTP endpoints and database connections. We're back in the
land of mutable state. Not only that --- we're often in the world of mutable state with concurrency.
What is a functional programmer to do?!

Fear not. We can test it; we have the technology.

## State machine testing

One solution to this problem --- one with a great power-to-weight ratio --- is state machine
testing. The short version is:

 - Model an application's state as a data type.
 - Model the inputs that can change the system's state as data types.
 - Specify how each model input can be executed against the system.
 - Specify how to update the model state given outputs from the system.
 - Write properties to test that the system behaviour and state match the model.
 
It gets better. There's an excellent property-based testing library called
[`hedgehog`](https://github.com/hedgehogqa/haskell-hedgehog) that includes facilities for state
machine testing. One issue with `hedgehog` is that it's state machine testing capabilities aren't
documented in detail, which can make it a little difficult to get started. I gave a [talk](/talks/)
on this topic at [YOW! Lambda Jam](http://lambdajam.yowconference.com.au/) recently, however the 25
minute talk slot made for a fairly fast paced talk. I'm now going to build upon that talk and
provide a slower-paced introduction over a series of blog posts.

## Parallel state machine testing: an example

Now that you hopefully have a high level understanding of the problem we're trying to solve, I'd
like to proceed with an example to whet your appetite for what's to come in this and future posts.

Lately I've been using state machine testing to test [WordPress](https://wordpress.org/). I'm doing
this for two reasons. Firstly, I'd like to demonstrate that these techniques can be employed to test
software that doesn't use functional programming. Secondly, I'd like to investigate how Haskell may
be used to test messy APIs designed with dynamic programming languages in mind. The first bug I
found during this testing is a good example of the power of state machine testing. After modelling a
part of WordPress' API and state, hedgehog was able to find a concurrency issue and provide a
minimal example to reproduce the issue.

Here's part of the output:

```
112 ┃     f cs s = forAll $ Gen.parallel (Range.linear 1 100) (Range.linear 1 10) s cs
              ┃     │ ━━━ Prefix ━━━
              ┃     │ Var 25 = CreatePost
              ┃     │            (fromList
              ┃     │               [ PostDateGmt :=> Identity 1900 (-01) (-01) 12 : 00 : 00
              ┃     │               , PostSlug :=> Identity (Slug "a")
              ┃     │               , PostStatus :=> Identity Publish
              ┃     │               , PostTitle :=> Identity (R (L (RCreate "a")))
              ┃     │               , PostContent :=> Identity (RP (L (PRCreate "a")))
              ┃     │               , PostAuthor :=> Identity (Author 1)
              ┃     │               , PostExcerpt :=> Identity (RP (L (PRCreate "a")))
              ┃     │               ])
              ┃     │ 
              ┃     │ ━━━ Branch 1 ━━━
              ┃     │ Var 26 = DeletePost (Var 25) Nothing
              ┃     │ Var 27 = DeletePost (Var 25) Nothing
              ┃     │ 
              ┃     │ ━━━ Branch 2 ━━━
              ┃     │ Var 28 = DeletePost (Var 25) (Just True)

...

no valid interleaving
```

So what is this telling us? The last line is telling us that there's "no valid interleaving". This
is because we ran the tests in parallel, and no matter how we interleave the inputs the system's
outputs always fails to match our expectations. If it's not clear what this means, we'll go into
detail in a later post.

In the other output, hedgehog has provided a minimal example to reproduce the issue:

- Create a post without any other parallel actions (the `Prefix`).
- Run two delete actions that send the post to the trash (`Nothing` argument) on one parallel branch
  (`Branch 1`).
- Run a delete action that actually deletes the post (`Just True` argument) on the second parallel
  branch (`Branch 2`).
  
In short --- hedgehog has run random sequences of web requests, in parallel, with random values for
the inputs, and found a concurrency bug. Not only that, it has then shrunk both the sequence of web
requests _and_ their inputs to provide a small (possibly minimal) example that still results in
failure. It gets even better: hedgehog provides the random seed and other relevant information that
produced the failure, so when we attempt a fix, we can re-run this exact test and ensure the fix has
worked.

If, like me, you find all of this terribly exciting and would like to know more --- stay tuned. The
rest of this post is a very brief overview of the prerequisites for state machine testing (state
machines and property based testing), but we'll start to get into the nuts and bolts of state
machine testing with hedgehog in the next post.

## State machines

Before we talk about how state machine _testing_ works, let's talk about state machines. If you've
seen state machines before, it's likely you've come across state machine diagrams like the one below
for a turnstile. You'll see it comprises:

 - A set of states: `{Locked, Unlocked}`.
 - A set of inputs that cause state transitions: `{Push, Coin}`
 - An initial state: `Locked`

<img src="../../../images/posts/state-machine-testing/turnstile.png" alt="turnstile state machine
from Wikipedia" />

If you squint a little, it seems that many common systems are state machines. Especially when one
considers that the set of states and inputs don't have to be finite (we're not limited to finite
state machines).

For example, many video games can be modelled as state machines. In the case of an RPG, the states
could comprise the product of the player's position in the world, inventory, health, and current
quest. The inputs could be controller inputs from the player, or more abstract actions such as "pick
up item". Finally, the initial state would be whatever initial values the game starts with.

We don't all get to work on games, so what about the humble web application? It's also a state
machine. Its states are all the possible values the application state can have; its inputs are HTTP
requests; and its initial state is whatever state it's in after starting up, but before receiving
its first request.

## Property based testing

Now that we understand state machines, and have established that many common applications can be
modelled using state machines, let's refresh our memory on property based testing. To begin, let's
consider the canonical example of reversing a list.

```haskell
-- Reverse is involutive
propReverse :: Property
propReverse =
  property $ do
    xs <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
    reverse (reverse xs) === xs
```

This function defines a property. It uses a generator --- `Gen.list (Range.linear 0 100) Gen.alpha`
--- to generate a random list of characters that is between 0 and 100 elements long. This random
input is then used to test the property that the result of reversing the list twice, is always equal
to the original list: `reverse (reverse xs) === xs`.

It's a small example, but this aptly captures the essence of property based testing. Generate random
inputs, then test that some properties hold for a function given each of those inputs. We'll see
plenty more examples of properties as we work through state machine testing. If you want to dive a
little deeper into property testing before continuing you can take a look at [the examples in the
Hedgehog repo](https://github.com/hedgehogqa/haskell-hedgehog/tree/master/hedgehog-example/src/Test/Example).

## The end... for now

That's it! Hopefully I've managed to get you excited about state machine testing, and at least point
to the concepts you'll want to be comfortable with before proceeding. Next time we'll start to look
at how state machine testing in hedgehog actually works.

