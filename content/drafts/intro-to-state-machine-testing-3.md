---
title: "Introduction to state machine testing: part 3"
date: 2018-11-27
authors: ajmcmiddlin
---

This is part 3 in a series. If you haven't seen the first two posts, you might want to take a look
at those first.

[Part 1: Why and prerequisites](/posts/intro-to-state-machine-testing-1)  
[Part 2: The basics and a first property](/posts/intro-to-state-machine-testing-2)

In part 2, we looked at the basics of how state machine testing in hedgehog worked. We defined
states and inputs, looked at how `hedgehog` uses the `Command` and `Callback` types to execute
tests, and defined our first property. In this post we're going to revisit some of this material and
explain the parts we initially glossed over. Specifically, we're going to look at why hedgehog
required our state and input types to be parameterised on a type constructor, what the `Var`,
`Symbolic` and `Concrete` types are all about, and why we need `HTraversable` instances for our
inputs. It might not come as a surprise, but they're all closely related.

## Another property

Our last post used the [leaderboard](https://github.com/qfpl/leaderboard) application as a test
subject, and we'll be continuing to use it in this post. This time we're not only going to register
our first user, we're going to register additional users and make sure that our count agrees with
the application.

Once again, let's start with a state diagram.

<img alt="Register state diagram" src="/images/posts/state-machine-testing/register.svg" />

If it's not clear what's happening here, let me explain. As with the property we wrote in part 2,
`n` here stands for the number of successfully registered users, and we start at `n=0`. Again, like
before, we may register our first user (`register-first` input) and move to `n=1`. We can then move
to `n>0` without input. Whenever our user count is greater than 0, `register-first` is a no-op ---
at least with regards to this state machine. However, an admin user may register a new user with the
`register` input, which increments the user count. At this point, the number of registered users is
still greater than 0 and we can move back to our `n>0` state without input.

## State and inputs revisited

Here's what our state and input types looked like for our first property.

```haskell
newtype RegFirstState (v :: * -> *) =
  RegFirstState Integer
  deriving (Eq, Show)

newtype RegFirst (v :: * -> *) =
    RegFirst RegisterPlayer
  deriving (Eq, Show)

data GetPlayerCount (v :: * -> *) =
    GetPlayerCount
  deriving (Eq, Show)
```

We know we want to register additional users, which isn't the same as registering our first user, so
we're going to need a new input for that.

```haskell
data Register (v :: * -> *) =
  Register RegisterPlayer (Var TestRsp v)
  deriving (Eq, Show)
```

`Register` comprises two values: the registration information for the new player, and a
`Var TestRsp v`. `TestRsp` is a newtype around the response type returned by our application's
`register` endpoint. It comprises a player ID and an authentication token. Importantly, we need the
authentication token of an adminstrator to successfully register a new player, and that's why our
`Register` input requires a `TestRsp`. As for the `Var` constructor and `v` parameter, we'll get to
those in just a moment.

Given we need a valid auth token for an admin if we want to register subsequent users, and we can
only get those from the application after registering a user, we're going to have to store some in
our state. It also seems appropriate to mention that `leaderboard` rejects registrations that try to
use an email for a user who has already been registerd. That means we need to generate unique
emails. To do that, we'll also need to keep track of registered emails in our state. So what's our
new state look like?

```haskell
data RegisterState (v :: * -> *) =
  RegisterState
  { rsPlayerEmails :: S.Set Text
  , rsAdmins       :: S.Set (Var TestRsp v)
  } deriving (Eq, Show)
```

This is probably close to what you expected given we need to keep track of player emails and admin
tokens now. Just like our new input, our new state also makes use of that `v` parameter and `Var`
type. OK. It's time...

## We need to talk about `v`

Firstly, it's important to understand how `hedgehog` runs our tests in a little more detail. The
hand wavey description of the process is something like this:

 1. Generate a random sequence of commands to run given an initial state.  
     a. Filter out any commands that don't produce an input generator given the current state.
     b. Filter out any commands whose `Require` callback returns `False` given the current state.
     c. Use the `Update` callback of each chosen command to update the state such that subsequent
        commands are filtered using the updated state.
 2. Execute the sequence of commands.  
     For each command:
     a. Run the input against the system under test.
     b. Update the model state given the output from the system under test.
     c. Check any post conditions (`Ensure` callbacks).
 3. If a test fails, shrink the list of commands (which will in turn shrink their inputs) and return
     to step 2.
     a. This includes removing commands that can no longer be run because their input included
        output from a command that is no longer being used.

I'd like to point out that `hedgehog` generates _all_ of the commands that are run before the first
command is ever executed against the application. I'd also like to point out that, while it's
generating this complete set of commands, it is updating state and deciding which commands may be
run based on this state. In case you're getting nervous, `hedgehgog` hasn't broken causality ---
it's just got two different versions of state. This is where the `v` parameter and `Var` type come
in.

Here's the `Var` type.

```haskell
data Var a (v :: * -> *) =
  Var (v a)
```

`Var` is used to store values that we get back from the application in our state. `hedgehog` needs
to keep track of the link between outputs that come from the application, and inputs that make use
of those outputs. Not only that, it needs to do this while generating and shrinking, which means it
hasn't run any commands and therefore has no concrete values. Instead, `hedgehog` uses symbolic
variables as placeholders for the outputs it expects to get back from the application. It therefore
makes sense that the two type constructors that `hedgehog` uses to inhabit the `v` parameter in
`Var` are `Symbolic` and `Concrete`.

```haskell
data Symbolic a where
  Symbolic :: Typeable a => Name -> Symbolic a

newtype Concrete a where
  Concrete :: a -> Concrete a
```

`Symbolic` and `Concrete` both have kind `* -> *`, which means they may inhabit `v`. We can also see
that the `a` in `Symbolic a` is a phantom type that is never inhabited. It just keeps track of the
fact that this placeholder should be replaced by a value of type `a`. Finally, `Concrete` is just a
`newtype` around an `a` such that it has the same kind as `Symbolic` and can replace `Symbolic`s
whenever we get concrete values back from the application.


When an input requires some previous output from our application, it gets wrapped up in a `Var` as
we saw in our `Register` input. While `hedgehog` is generating or shrinking, and doesn't have
concrete values, this will be a `Var a Symbolic`. Once commands are being executed, our state will
be rebuilt using values of type `Var a Concrete`. Despite being a placeholder for future values,
it's useful to note that `Var a Symbolic` has `Eq` and `Ord` instances. This allows us to do things
like store our symbolic representations in ordered containers and check that they're still present
in our state as part of `Require` callbacks.

Our hand wavey explanation of what `hedgehog` does included another interesting tid bit: our state
gets built up twice. When we generate commands, we build up a symbolic representation of the state
that allows subsequent commands to determine whether it makes sense for them to be generated or run
given the current state. Once commands are being run against the application, the state gets rebuilt
from the beginning using the concrete values returned by the application. This is why our state type
must be parameterised on a type constructor --- so we can mark whether it contains
`Var a Symbolic`s, or `Var a Concrete`s.

## `HTraversable` revisited

Inputs work a little differently to state. Inputs are generated along with commands, which means
they start life with a symbolic representation of any values we expect to get back from our
application. However, unlike our state, which we can build up again from an initial value, we can't
rebuild our inputs. Once they're generated as part of each command, they are part of the tree of
generated values and don't get modified. Instead, inputs get updated on demand such that their
symbolic variables are replaced with concrete values before the input is executed. To capture this
ability to replace `Var a Symbolic`s with `Var a Concrete`s, `hedgehog` uses the `HTraversable`
class. This is why every input _must_ have an instance of `HTraversable` --- `hedgehog` needs to be
able to replace symbolic placeholders with their concrete values once they've been returned by the
application. The fact that state gets rebuilt and inputs don't is also the reason that, while our
state must be parameterised on the type constructor `v`, it doesn't need to have an `HTraversable`
instance.

Now that we know why the `HTraversable` class exists and why inputs must have instances for it,
let's look at its definition again.

```haskell
class HTraversable t where
  htraverse :: Applicative f => (forall a. g a -> f (h a)) -> ht g -> f (ht h)
```

In the same way that `traverse` (from the `Traversable` class) maps between types of kind `*`
underneath an `Applicative`, `htraverse` maps type constructors of kind `* -> *` under an
`Applicative`. This might be made clearer if we put the definition of `htraverse` and `traverse`
(from the `Traversable` class) next to each other.

```haskell
traverse  :: Applicative f => (            a -> f    b)  ->  t a -> f ( t b)
htraverse :: Applicative f => (forall a. g a -> f (h a)) -> ht g -> f (ht h)
```

`traverse` takes a function that maps `a`s to `b`s with some `Applicative` effect, and an instance
of `Traversable` (`t`) containing `a`s. `t` has kind `* -> *` here as it's a regular type
constructor. One intuition for this function is that it gets applied to each of the `a`s in `t`,
while the `Applicative` is pulled outside of the `t`. For example, if we `traverse` over a list of
things where the mapping is partial, we end up with the following.

```haskell
(a -> Maybe b) -> [a] -> Maybe [b]
```

As you can maybe guess from the type signature, if any of the mappings produces a `Nothing`, the
whole result is `Nothing`.

`htraverse` is very similar, except `ht`, our instance of `HTraversable`, is a higher kinded type of
kind `(* -> *) -> *`. So, given a function that maps between type _constructors_ `g` and `h`,
`htraverse` should perform this mapping everywhere the type constructor `g` appears in its
structure. Not only that, but the use of `Rank2Types` means that the mapping function knows nothing
about `a` and is therefore unable to modify it in any way.

To make things clearer, let's write out some types and do some substitutions.

```haskell
Symbolic :: * -> *
Concrete :: * -> *

htraverse ::
  Applicative f
  => (forall a. g a -> f (h a))
  -> ht g
  -> f (ht h)
```

If we let `g ~ Symbolic` and `h ~ concrete`, we get:

```haskell
htraverse ::
  Applicative f
  => (forall a. Symbolic a -> f (Concrete a))
  -> ht Symbolic
  -> f (ht Concrete)
```

`ht` is an instance of `HTraversable`, and each of our inputs must be an instance of `HTraversable`,
so let's substitute in our new `Register` input. We'll write it's `HTraversable` instance in a moment.

```haskell
htraverse ::
  Applicative f
  => (forall a. Symbolic a -> f (Concrete a))
  -> Register Symbolic
  -> f (Register Concrete)
```

Finally, `hedgehog` maintains a mapping from symbolic `Var`s to their concrete values as each input
is executed. The mapping function that `hedgehog` uses with `htraverse` is a lookup in that
environment, which is partial and might fail. The type of the lookup function is something like
`forall a. Symbolic a -> Either EnvironmentError (Concrete a)`. So, when `hedgehog` updates our
`Register` input with concrete values, `htraverse` is used with the following concrete type.

```haskell
htraverse ::
  (forall a. Symbolic a -> Either EnvironmentError (Concrete a))
  -> Register Symbolic
  -> Either EnvironmentError (Register Concrete)
```

To summarise: given a function that maps a symbolic variable to a concrete value with the
possibility of failure, `htraverse` is used to replace all symbolic variables in an input with their
concrete values from the environment, or fail the entire computation if any of the lookups fail.

Now that we know the types, and we have an intuition for what we're trying to achieve with an
`HTraverse` instance, let's write the instance for `Register`.

```haskell
instance HTraversable Register where
  htraverse f (Register rp (Var rsp)) =
    Register rp . Var <$> f rsp
```

If it's not clear what we're doing here, it helps to write out some types. `htraverse` returns
something of type `f (ht h)`; where `f` is some applicative, `ht` is our instance of `HTraversable`,
and `h` is the type constructor we've mapped to. In our case, we're looking to return a value of
type `f (Register h)` given we're writing a `HTraversable` instance for `Register`.

```haskell
rsp :: g TestRsp
f :: forall a. g a -> f (h a)
f rsp :: f (h TestRsp)

Var :: h a -> Var a h
Register rp :: Var TestRsp h -> Register h
Register rp . Var :: h TestRsp -> Register h

(Register rp . Var <$>) :: f (h TestRsp) -> f (Register h)
Register rp . Var <$> f rsp :: f (Register h)
```

## Breather

Let's take stock of where we're at. So far in this post we've:

 - Introduced a new property --- registering users should increment the count of users.
 - Specified an input to register users with an admin token.
 - Updated our state to keep track of player emails and admin tokens so we can successfully register
   new users.
 - Talked about `Symbolic` vs `Concrete` state.
 - Looked at what `HTraversable` does and written a non-trivial instance.
 
Now that we have a deeper understanding of how `hedgehog` runs state tests and how it handles state,
I'd like to look at `Command` and `Callback` again to fill in the bits we skipped in the last post.
After that, we'll write our new `Command` for registering users and then update our property to use
our new command.

## `Command` and `Callback` revisited

Here's the type of `Command` again.

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

We can see now that this reflects our understanding of `hedgehog`'s process and use of state.
`commandGen` generates inputs before anything has been executed, and therefore only has a `Symbolic`
view of the model state. Furthermore, it can take `Symbolic` representations of expected application
outputs and use them as part of its input, as evidenced by the `input Symbolic` in `commandGen`'s
return type.

`commandExecute` is actually running commands, and has access to values returned by our application.
Given commands are executed sequentially in the order they were generated, our state should include
actual return values from the application for any previously executed commands. Therefore, any
inputs may be transformed from those containing `Symbolic` values to `Concrete` ones via their
`HTraversable` instance.

Next, let's look at the `Callback` type again.

```haskell
data Callback input output state =
    Require (state Symbolic -> input Symbolic -> Bool)
  | Update (forall v. Ord1 v => state v -> input v -> Var output v -> state v)
  | Ensure (state Concrete -> state Concrete -> input Concrete -> output -> Test ())
```

Once again, this reflects the understanding we've been building up in this post. `Require` is a
precondition that is checked during generation and shrinking, but before anything has been executed
against the application. It therefore deals with a `Symbolic` state and input. `Update` is used to
build up both a `Symbolic` state and a `Concrete` state. Therefore, it must be polymorphic in `v`.
Finally, `Ensure` is a post condition that is checked during execution, so it gets a `Concrete` view
of all the state that has been built up, up to and including the state after its command has run. It
also has access to the `Concrete` input that was used.

## The `cRegister` `Command`

We've covered all the required concepts, and now we can finally write our new command.

```haskell
cRegister
  :: ( MonadGen n
     , MonadIO m
     , MonadTest m
     )
  => ClientEnv
  -> Command n m RegisterState
cRegister env =
  let
    gen (RegisterState ps as) =
      if null as
      then Nothing
      else (Register <$> genRegPlayerUniqueEmail ps <*>) <$> genAdminRsp as
    execute (Register rp rsp) =
      fmap TestRsp $ evalEither =<< successClient env (register (clientToken rsp) rp)
  in
    Command gen execute [
      Require $ \(RegisterState ps as) (Register rp p) ->
           S.notMember (_lbrEmail rp) ps
        && S.member p as
    , Update $ \(RegisterState ps as) (Register rp _rqToken) rsp ->
        let
          newPs = S.insert (_lbrEmail rp) ps
          newAs = bool as (S.insert rsp as) (_lbrIsAdmin rp == Just True)
        in
          RegisterState newPs newAs
    , Ensure $ \(RegisterState psOld asOld) (RegisterState psNew asNew)
          (Register LeaderboardRegistration{..} _t) rsp -> do
        assert $ S.member _lbrEmail psNew
        assert $ S.notMember _lbrEmail psOld
        length psNew === length psOld + 1
        let vRsp = Var (Concrete rsp)
        if _lbrIsAdmin == Just True
          then do
            assert (S.member vRsp asNew)
            assert (S.notMember vRsp asOld)
            length asNew === length asOld + 1
          else
            success
    ]
```

`cRegister` is a big function. Let's tackle it in pieces.

```haskell
cRegister
  :: ( MonadGen n
     , MonadIO m
     , MonadTest m
     )
  => ClientEnv
  -> Command n m RegisterState
```

Our type signature isn't very surprising, but it's worth pointing out that it uses our new
`RegisterState` type.

```haskell
cRegister env =
  let
    gen (RegisterState ps as) =
      if null as
      then Nothing
      else (Register <$> genRegPlayerUniqueEmail ps <*>) <$> genAdminRsp as
```

`gen` isn't very exciting either. If we don't have any admin users (`as`) then we can't generate a
`Register` input. If we do, we use some helpers to generate registration information containing a
unique email address and select an admin user whose token we'll use.

```haskell
execute (Register rp rsp) =
  fmap TestRsp $ evalEither =<< successClient env (register (clientToken rsp) rp)
```

`execute` just hits our registration endpoint via the `register` client function, expects success, and
then wraps the response type in our `newtype`.

```haskell
Require $ \(RegisterState ps as) (Register rp p) ->
     S.notMember (_lbrEmail rp) ps
  && S.member p as
```

The `Require` callback checks that our player doesn't already exist in the state, and that the admin
user whose token we need is still present in the state. When `hedgehog` encounters a failure and
starts shrinking things, our player emails will start to converge given the way shrinking works, so
we need to make sure they're still unique. Likewise, if the command that registered the admin user
used by this command is removed as part of shrinking, we can no longer run this command because the
admin user won't exist.

```haskell
Update $ \(RegisterState ps as) (Register rp _rqToken) rsp ->
  let
    newPs = S.insert (_lbrEmail rp) ps
    newAs = bool as (S.insert rsp as) (_lbrIsAdmin rp == Just True)
  in
    RegisterState newPs newAs
```

State updates are also unsurprising: we must insert our new player's email into the state to ensure
that we don't generate an email already in use, and we must add our new user's token to the set of
admins if the user is to be an admin.

```haskell
Ensure $ \(RegisterState psOld asOld) (RegisterState psNew asNew)
    (Register LeaderboardRegistration{..} _t) rsp -> do
  assert $ S.member _lbrEmail psNew
  assert $ S.notMember _lbrEmail psOld
  length psNew === length psOld + 1
  let vRsp = Var (Concrete rsp)
  if _lbrIsAdmin == Just True
    then do
      assert (S.member vRsp asNew)
      assert (S.notMember vRsp asOld)
      length asNew === length asOld + 1
    else
      success
```

Finally, our post conditions. We check that our new set of player emails contains the one we
registered, and that our previous state didn't contain that email. We also check that the number of
player emails in our state has increased by one with this command. Finally, if our new player is to
be an admin, we do similar checks with our set of admins. These are really just sanity checks to
help catch bugs in our test code early rather than having them manifest in strange failures later.
It's pretty easy to mess up a generator, `Require`, or `Update` and have `hedgehog` do some very
unexpected things. The `cGetPlayerCount` command we defined in our last post is sticking around, and
that'll take care of checking that our count agrees with the application's, which is the property
we're actually testing.

## Updated property

Once again, we're going to start with code from our first property.

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

There are only a couple of changes we need to make here other than renaming some things. Firstly, we
need to add our new command to the list of commands. Secondly, we need to change the `initialState`
to be of type `RegisterState`.

```haskell
propRegister
  :: ClientEnv
  -> IO ()
  -> TestTree
propRegister env reset =
  testProperty "register-all" . property $ do
  let
    commands = ($ env) <$>
      [ cRegister
      , cRegisterFirst
      , cRegisterFirstForbidden
      , cGetPlayerCount
      ]
    initialState =
      RegisterState S.empty S.empty
  actions <- forAll $
    Gen.sequential (Range.linear 1 100) initialState commands
  evalIO reset
  executeSequential initialState actions
```

## One more things

This post has gotten quite long already, but there's one last thing we need to do. If we were to try
and compile our code as it stands, we'd get some type errors. All of the commands we defined in our
last post, which are still being used in this new property, used `RegFirstState` as their state
type. `cRegister` uses `RegisterState` though. We need to go and update each of those commands to
use our new state before proceeding.

I won't bore you with the details, but I will say that it's annoying busy work. It also triggers an
unhappy thought. As our properties and commands grow more complex, so will our state. We'll likely
want to reuse existing commands in new properties too, which means we're likely to have to update
all of our existing commands every time we update our state. That sucks. If only there were a nice
way to deal with that _wink wink_.

## Until next time

That's it for this post, and very likely for 2018. Things are very busy at Data61, and we're rapidly
approaching the new year. I still have more to say on this topic though, sol I'll be back in 2019 to
share it.
