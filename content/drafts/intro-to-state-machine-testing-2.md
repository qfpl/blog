---
title: "Introduction to state machine testing: part 2"
date: 2018-11-13
authors: ajmcmiddlin
---

In my [first post](/posts/intro-to-state-machine-testing-1/) on state machine testing I gave a high
level view of what state machine testing is and some of the prerequisite knowledge. Now we're going
to start getting into the nuts and bolts of how state machine testing with
[`hedgehog`](https://github.com/hedgehogqa/haskell-hedgehog) works.

I found working through examples to be the most valuable way to learn this stuff, so that's what
we'll be doing in this post. There's a lot to take in, and you need to build up a reasonable mental
model of all the pieces before they start to click, so don't be disheartened if it takes a couple of
goes to get there.

We're going to use an app called [`leaderboard`](https://github.com/qfpl/leaderboard) as our test
subject. `leaderboard` is intended to be a simple web app for keeping track of table tennis scores.
So far it's mostly been a playground for trying out ideas and learning. I've simplified some of the
code for the demos in this post, so if you want to see everything, check out the [`itsm2`
tag](https://github.com/qfpl/leaderboard/tree/itsm2).

## A first property

The part of `leaderboard` that has had the most attention is the registration and authentication,
which is what we'll be focussing on with our tests. As it currently stands, the registration process
is pretty simple. A special endpoint allows the first, and only the first, successful registration
to happen without any other checks. This first user is unconditionally an administrator, which
allows them to register subsequent users.

Already we have a stateful property that we'd like to uphold: No matter what, the special endpoint
for the first player registration should only succeed once. Let's test it!

## State machine

The first thing we need to do is model enough of the registration process to be able to test it. We
don't have to model the whole application, or even all of the registration functionality. All that's
required is a model of that state that's relevant to the properties we're testing --- nothing more.

We'll start by drawing a state machine diagram for this functionality.

<img alt="register first state machine diagram" src="/images/posts/state-machine-testing/regfirst.svg" />

Here, `n` represents the count of registered players. So we're saying that when `n=0` and we hit the
`register-first` endpoint, we expect it to succeed and increment the player count to 1. Hitting the
`register-first` endpoint while the state is `n=1` doesn't result in a state change, and for the
purposes of this test we're in the terminal state. That's why `n=1` has the double circle around it.

If at this point you're thinking that this isn't a very interesting property, I totally agree with
you. However, we're keeping the properties as simple as we can for now so that we can focus on how
`hedgehog` works. As we get more comfortable with that, we'll ramp up the properties.

## State

We have some states now, so let's tell `hedgehog` about them. `hedgehog` requires that you model
states as a data type parameterised on a type constructor. The type constructor parameter isn't
important for now, so we'll include it in code examples but otherwise ignore it until we need it.

```haskell
newtype LeaderboardState (v :: * -> *) =
  LeaderboardState Integer
```

This is about as simple as it gets: our state is just a counter for the number of registrations.

## Inputs

State machines comprise states and inputs. We've got our states, so now we need inputs. For our
simple example we need two inputs: registering our first user, and getting a count of all registered
users to verify our model state agrees with the app's state.

Similar to our states, inputs are also parameterised on a type constructor, which we'll keep
ignoring.

```haskell
newtype RegFirst (v :: * -> *) =
    RegFirst RegisterPlayer
  deriving (Eq, Show)

data GetPlayerCount (v :: * -> *) =
    GetPlayerCount
  deriving (Eq, Show)
```

`RegFirst` requires a `RegisterPlayer` value, which contains all the data we need to register a new
player. `GetPlayerCount` is an input with no arguments --- as the name suggests it's just a getter.

## `HTraversable`

`hedgehog` requires every input type to have an instance of `HTraversable`. We're not going to go
into why just yet. Instead, we're going to follow the types to satisfy `hedgehog` and move on. We'll
take a closer look at it later.

Here's the class definition for `HTraversable`.

```haskell
class HTraversable t where
  htraverse :: Applicative f => (forall a. g a -> f (h a)) -> t g -> f (t h)
```

In the same way that `traverse` (from the `Traversable` class) maps between types of kind `*` and
wraps them in an `Applicative`, `htraverse` maps between type constructors of kind `* -> *` within
an `Applicative`. For our current inputs, `HTraversable` instances are only necessary to satisfy
`hedgehog`'s requirements, and don't really serve much purpose beyond that. For completeness, here
are the instances.

```haskell
instance HTraversable RegFirst where
  htraverse _ (RegFirst rp) = pure (RegFirst rp)

instance HTraversable GetPlayerCount where
  htraverse _ _ = pure GetPlayerCount
```

Looking back at our inputs, they're parameterised on some type constructor that didn't appear in our
values. It's therefore not surprising that we don't do any mapping of type constructors in our
`HTraversable` instances. Instead, we just wrap them up in the `Applicative`.

## `Command`s

Now that we have our state and our inputs, we need to tell `hedgehog` what to do with them.
Remembering back to the last post, this is where we're at in our high level process.

 1. ~~Model an application's state as a data type.~~
 2. ~~Model the inputs that can change the system's state as data types.~~
 3. Specify how each model input can be executed against the system.
 4. Specify how to update the model state given outputs from the system.
 5. Write properties to test that the system behaviour and state match the model.
 
Steps 3 to 5 are handled by `Command`s in `hedgehog`. Let's look at the data type.

```haskell
data Command n m (state :: (* -> *) -> *) =
  forall input output.
  (HTraversable input, Show (input Symbolic), Typeable output) =>
  Command {
      commandGen ::
        state Symbolic -> Maybe (n (input Symbolic))
    , commandExecute ::
        input Concrete -> m output
    , commandCallbacks ::
        [Callback input output state]
    }
```

`Command` has 3 type variables. `n` is going to be an instance of `MonadGen` in which we can
generate random values. `m` is some monad that we can work in when interacting with the application
under test. `state` is our model state.

There are also two existential types that aren't part of the `Command` type, `input` and `output`.
These are the types of the abstract input and the output produced by the application under test.
Using existentials here allows us to provide a homogenous collection of commands for `hedgehog` to
choose from when generating sequences of inputs. If you're not familiar with existential types then
don't sweat the details --- just know that all `Command`s used in a property must use the same `n`,
`m`, and `state`, but can differ on the `input` and `output`.

The `Command` record contains three fields. `commandGen` tells `hedgehog` how to generate the input
associated with this command. Given a value of the state it will optionally generate an input. The
reason it's optional is that we may be in a state where it doesn't make sense to run a particular
command. The property we're in the process of defining is an example. As we'll soon see, we use two
different `Command`s for testing the register first functionality: one for when we expect it to
succeed, and another for when we expect failure. Once we've registered our first user, we no longer
want to generate a command that expects success, so we shall return `Nothing`.

`commandExecute` tells `hedgehog` how to execute our abstract input against the application under
test and produce a value of our `output` type.

Finally, `commandCallbacks` is used to provide a list of preconditions, state updates, and
assertions to execute when running each `Command`. Let's look at those in more detail.

## Callbacks

```haskell
data Callback input output state =
    Require (state Symbolic -> input Symbolic -> Bool)
  | Update (forall v. Ord1 v => state v -> input v -> Var output v -> state v)
  | Ensure (state Concrete -> state Concrete -> input Concrete -> output -> Test ())
```

As we can see, the `Callback` type has 3 constructors: `Require` for preconditions, `Update` for
state updates, and `Ensure` for assertions.

`Require` is a predicate on the current state and input that checks if it still makes sense to run a
command. This is important when shrinking. If `hedgehog` finds a test failure and shrinks inputs to
find a minimal example, it also shrinks the list of commands. If the list of commands changes, then
the state is likely to change, which means that the checks performed in the generator using an old
state may no longer be valid.

`Update` is used to update the state we're tracking in our tests. Given the old state, the input,
and the output, it produces a new state.

Finally, `Ensure` is used to make assertions once a command has run. Given the old and new states,
the input, and the output, it specifies a `Test ()`. That is to say, it tests our expectations about
how the state relates to the output from the application.

## Register first `Command`s

We've got enough pieces to write our first state machine property now: we've defined the model
state, we've defined our inputs, and now we can define the `Command`s that tell `hedgehog` what to
do with them.

### `cRegisterFirst`

Here's our first `Command`. It tries to register the first user and expects it to succeed.

```haskell
cRegisterFirst
  :: ( MonadGen n
     , MonadIO m
     , MonadTest m
     )
  => ClientEnv
  -> Command n m RegFirstState
cRegisterFirst env =
  let
    gen (RegFirstState n) =
      if n == 0
      then Just . fmap RegFirst $ genRegPlayer
      else Nothing
    execute (RegFirst rp) =
       fmap TestRsp $ evalEither =<< successClient env (registerFirst rp)
  in
    Command gen execute [
      Require $ \(RegFirstState n) _input -> n == 0
    , Update $ \_state _input _out ->
        RegFirstState 1
    ]
```

Firstly, let's look at `gen`, which is responsible for producing an abstract input to the
command. Given that this command is going to register our first user and expect it to succeed, we
need to make sure we haven't already registered a user. To do that, we check our state. If the
player count is 0, we produce an input by generating registration information. If the player count
_isn't_ one, then we don't generate an input and this command won't be used.

`execute` takes our abstract input and runs it against the app. It does this using `registerFirst`,
which is a function produced by
[`servant-client`](https://hackage.haskell.org/package/servant-client-0.11). `registerFirst` hits
the relevant HTTP endpoint for registering our first user and takes care of marshalling values
between Haskell data types and the representations used for transmission. `successClient` is a
helper that runs an action in `servant-client`'s monad and returns a value of type `MonadIO m => m
(Either a ServantError)`. `evalEither` is a `hedgehog` function that fails the test when given an
`Either` value constructed using `Left`, or passes on the value from a `Right` constructor. Finally,
we wrap up the result in our `TestRsp` `newtype` which we use to instance things we can't otherwise
instance without creating orphans.

Last, but not least, we specify our `Callback`s. We can specify as many or as few callbacks as we'd
like, as implied by the fact that `hedgehog` expects them to be in a list. This `Command` only makes
use of `Require` and `Update` callbacks. The `Require` ensures that we only run this command when
our player count is 0. The `Update` sets the model state's player count to 1 after the command has
been executed. We could add an assertion that checks the new state has a player count of 1, but at
that point you're mostly testing that `hedgehog` works as expected, so we haven't.

### `cRegisterFirstForbidden`

As I mentioned earlier, it pays to have multiple commands for the same input whenever your
expectations differ. We just saw in our first command that we used the `successClient` helper and
`evalEither` to fail the test if registering the first user failed. Now we need to specify a similar
command that expects failure whenever our first user has already been registered.

```haskell
cRegisterFirstForbidden
  :: ( MonadGen n
     , MonadIO m
     , MonadTest m
     )
  => ClientEnv
  -> Command n m RegFirstState
cRegisterFirstForbidden env =
  let
    gen (RegFirstState n) =
      if n == 0
      then Nothing
      else Just . fmap RegFirst $ genRegPlayer
    execute (RegFirst rp) =
      evalEither =<< failureClient env (registerFirst rp)
  in
    Command gen execute [
      Require $ \(RegFirstState n) _input -> (n > 0)
    , Ensure $ \_sOld _sNew _input se ->
        case se of
          FailureResponse{..} -> responseStatus === forbidden403
          _                   -> failure
    ]
```

This is very similar to our first command. The differences are:

 - `gen` has flipped such that we only generate this command when the number of players _isn't_ 0.
 - `execute` uses `failureClient` instead of `successClient`, such that the tests fail when the
   request _doesn't_ produce an error.
 - Our `Require` callback is flipped such that the number of players must be greater than 0.
 - An `Ensure` callback has been added to check that the error response is specifically a `403
   Forbidden`.
 
### `cGetPlayerCount`

Our last command is used to retrieve the number of players and ensure that it agrees with our state.
It looks like this.

```haskell
cGetPlayerCount
  :: ( MonadGen n
     , MonadIO m
     , MonadTest m
     )
  => ClientEnv
  -> Command n m RegFirstState
cGetPlayerCount env =
  let
    gen _state =
      Just (pure GetPlayerCount)
    exe _i =
      evalEither =<< successClient env (unPlayerCount <$> getPlayerCount)
  in
    Command gen exe [
      Ensure $ \(RegFirstState n) _sNew _i c ->
        n === fromIntegral c
    ]
```

There's nothing very surprising here. We always generate an input, given we don't care what the
state is, and we have no callbacks other than an `Ensure` to check that our state matches the app's
count of players.

## Putting it together

We have all our pieces, so now we need to put them together and feed them to `hedgehog`. 

```haskell
propRegFirst
  :: ClientEnv
  -> IO ()
  -> TestTree
propRegFirst env reset =
  testProperty "register-first" . property $ do
  let
    commands =
      ($ env) <$> [cRegisterFirst, cGetPlayerCount, cRegisterFirstForbidden]
    initialState =
      RegFirstState 0
  actions <- forAll $
    Gen.sequential (Range.linear 1 100) initialState commands
  evalIO reset
  executeSequential initialState actions
```

`propRegFirst` takes a client environment, `env`, that is used to run `servant-client` actions, as
well as an IO action, `reset`, that we use to reset the app state between test runs. If we didn't reset
the state, each test would begin with the model state's initial value, and an application state that
contained whatever was left over from previous runs. Not a good place to start when you're trying to
ensure your model and application state always agree.

`commands` lists the commands that `hedgehog` may select from when generating tests. The `ClientEnv`
required to execute actions with `servant-client` is passed to each command, as they each need to
run requests using `servant-client`. Given this list of possible commands, we can ask `hedgehog` to
generate a random sequence of between 1 and 100 of them for each test. We use `Gen.sequential`
because this property is going to run each command in sequence --- that is, we won't attempt to run
any of these commands in parallel. That's a topic for another post.

Once our sequence of commands is generated, we run our `reset` action using `evalIO`, which lifts
our `IO` action into our test monad. Importantly, we're running this _after_ we're done generating
the sequence of commands. It's a rather subtle gotcha with `hedgehog`'s state machine testing, but
if we were to run the reset action before we'd finished generating all of our property inputs, the
reset action would only be run once before any tests had run, and not before each test run and
shrink. The rule here is that if you want something to run before each test, do it after all
generators have run (after all your calls to `forAll`), but before execution.

Finally, we call `executeSequential` to run our property. Given our implementation is correct, this
is what we see when we run this property.

```
Migrated successfully
registration
  register-first: OK (6.37s)
    OK

All 1 tests passed (6.37s)
```

Our DB migration on the temporary test database ran correctly, and our `register-first` property
passed. Success!

Details on how the test setup works are beyond the scope of this post and my goal of explaining how
state machine testing works. If looking at the code isn't enough, feel free to [get in touch](/contact) and
I can look at writing that up in another post.

## That's all folks!

We might not have written the most useful or impressive state machine test, but it's a start. We
also covered a large chunk of how state machine testing in `hedgehog` works: We found a state
machine in our application, wrote data structures for its states and inputs, then turned it all into
a working state machine test.

There were some things we skipped too. Like what was the deal with those mysterious `v`s,
`Symbolic`s, and `Concrete`s peppered through our types? What was the point of those `HTraversable`
instances? Stay tuned for part 3 and all will be revealed! Probably. I mean, I think that's what
we'll cover next. I haven't written it yet. Stay tuned though. Definitely do that.

