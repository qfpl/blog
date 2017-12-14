---
title: JSaddle - ArrayBuffers and Notifications
date: 2017-12-14
authors: schalmers
---

This post will demonstrate a couple of techniques for using the
[GHCJS](https://github.com/ghcjs/ghcjs) helper libraries:
[jsaddle](https://hackage.haskell.org/package/jsaddle) and
[jsaddle-dom](https://hackage.haskell.org/package/jsaddle-dom) to:

* Create an
  [ArrayBuffer](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/ArrayBuffer)
  for use with WebGL
* Leverage the [Notification
  API](https://developer.mozilla.org/en-US/docs/Web/API/Notifications_API) available in some
  browsers.

### ArrayBuffers

During some experiments with [reflex-dom-canvas](https://github.com/qfpl/reflex-dom-canvas), a
library for interacting with the [Canvas
API](https://developer.mozilla.org/en-US/docs/Web/API/Canvas_API). I started having runtime
exceptions regarding a vertex index being out of bounds. Initially I thought it was due to an
incorrect memory plan:

```haskell
  let
    size      = 2        -- 2 components per iteration
    dataType  = Gl.FLOAT -- the data is 32bit floats
    normalise = False    -- don't normalize the data
    stride    = 0        -- 0 for tightly packed array, or (size * sizeof(type)) to get the next position/per iteration
    offset    = 0        -- start at the beginning of the buffer

  -- Tell the attribute how to get data out of positionBuffer (ARRAY_BUFFER)
  Gl.vertexAttribPointerF (fromIntegral _rPosAttrLoc) size dataType normalise stride offset
```

After trying a few iterations of different ``stride`` values, I inspected the list of vertices, in
case something hadn't survived the trip from Haskell -> JavaScript. It wasn't the first point of
investigation because it seemed so innocuous:

```haskell
positions
  :: [Double]
positions =
  [ 0.0, 0.0
  , 0.0, 0.5
  , 0.7, 0.0
  ]
```

Using the ``toJSVal`` function from ``jsaddle`` to take our ``[Double]`` and turn it into a
``JSVal`` for the API to consume:

```haskell
toJSVal positions
```

Upon closer inspection, I was performing the conversion to a JavaScript value incorrectly. The list
of vertices was being inlined as arguments to the ``bufferData`` function:

What I expected:

```javascript
bufferData(ARRAY_BUFFER, vertexList, STATIC_DRAW);
```

What was appearing in JavaScript:

```javascript
bufferData(ARRAY_BUFFER, 0.0, 0.0, 0.0, 0.5, 0.7, 0.0, STATIC_DRAW);
```

This is because ``jsaddle`` provides a the ``MakeArgs`` typeclass for turning a list into the
arguments of a function. So my list of vertices was being inlined as individual function inputs, and
I didn't understand it well enough to be able to solve my problem, yet.

### From [Double] to ArrayBuffer

In the first attempt, the ``ArrayBuffer`` was handled directly. Creating a ``Float32Array`` that was
backed by that buffer, populating it with data, before accessing the underlying buffer again to
use in the WebGL code. Fuelled by a need to make something appear on the screen, and my general
ignorance of the workings of the ``jsaddle`` library, this attempt ended up being very low level and
manual.

The second attempt more closely mirrors the approach used in many JavaScript WebGL tutorials. It is
also a marked improvement over the first attempt, courtesy of mistakes made, lessons learned, and
questions asked.

#### First Attempt

__Step 1) Instantiate a fixed size ``ArrayBuffer`` object and create a ``Float32Array`` using that buffer.__

```javascript
// JavaScript

var buff = new ArrayBuffer(positions.length * 4);
var f32Arr = new Float32Array(buff);
```

```haskell
-- Haskell

-- The 'length' function returns an 'Int', but we need to give jsaddle a 'Double', so we use
-- 'fromIntegral' to handle that transformation for us. This needed a type annotation to 
-- help Haskell along as these types end up very general. You can't pick a typeclass when 
-- all you know about a type is that it is an instance of 'Num'.
let
  buffSize :: Double
  buffSize = fromIntegral (length positions * 4)

buff <- new (jsg "ArrayBuffer") buffSize
f32Arr <- new (jsg "Float32Array") buff
```

The ``new`` function from ``jsaddle`` works similarly to its JavaScript counterpart, requiring a
constructor object and a list of arguments to pass to the constructor function. The ``jsg`` function
acquires a top level JavaScript reference, a simple intuition for it may be anything it exists on the
``window`` object may be accessed using ``jsg``. You can read more about these functions [here
(``new``)](https://hackage.haskell.org/package/jsaddle-0.9.4.0/docs/Language-Javascript-JSaddle-Object.html#v:new),
and [here
(``jsg``)](https://hackage.haskell.org/package/jsaddle-0.9.4.0/docs/Language-Javascript-JSaddle-Object.html#v:jsg).

__Step 2) Populate the ``Float32Array`` with the position information.__

```javascript
// JavaScript

for ( var i = 0; i < positions.length; i++ ) {
  f32Arr[i] = positions[i];
}
```

```haskell
-- Haskell

itraverse_ (\ix pos -> (f32Arr <## ix) pos ) positions
```

The ``itraverse_`` function is an indexed traversal, providing the index of the current element as
input to the traversal function:

```haskell
itraverse_ :: (Applicative f, FoldableWithIndex i t) => (i -> a -> f b) -> t a -> f ()
```

We can use that with a ``jsaddle`` function, ``(<##)`` ,[documented
here](https://hackage.haskell.org/package/jsaddle-0.9.4.0/docs/Language-Javascript-JSaddle-Object.html#v:-60--35--35-).
This function sets a property on an object at the given index. For a bit more flavour, we're able to
take advantage of the fact that Haskell functions only take one argument
[[1](https://stackoverflow.com/questions/37040814/why-haskell-takes-one-argument)]
[[2](http://blog.tmorris.net/posts/haskell-functions-take-one-argument/)], to simplify our traversal
function:

```haskell
-- Starting here with our original function, lets call it 'f' for now...
f :: Int -> Double -> JSM ()
f ix pos = (f32Arr <## ix) pos

-- We don't need to explicitly include the 'pos' argument
f :: Int -> Double -> JSM ()
f ix pos = (f32Arr <## ix) pos
-- is the same as
f :: Int -> Double -> JSM ()
f ix = f32Arr <## ix

-- because... We can also drop the 'ix' argument, because this function:
(<##)  :: (MakeObject this, ToJSVal val) => this -> Int -> val -> JSM ()
-- Specialised to our Float32Array
(<##)  :: Float32Array -> Int -> Double -> JSM ()
-- Then partially applied to our Float32Array:
f :: Int -> Double -> JSM ()
f = (f32Arr <##)

-- Consequently our thoroughly "code golf'd" 'itraverse_' becomes
itraverse_ (f32Arr <##) positions
```

__Step 3) Retrieve the underlying buffer to be used in the rendering process.__

The underlying buffer of a ``Float32Array`` is accessed as a property of the array object itself.

```javascript
// JavaScript

return f32Arr.buffer;
```

Haskell needs a bit more information as the property access functions return a ``JSVal``. Conversions are up to you, because this lets you choose where you want to be on the runtime safety scale:

```haskell
-- Haskell

-- Access the "buffer" property on our f32Arr object
f32Buff <- f32Arr ! "buffer"
-- Convert our JSVal to the ArrayBuffer type we require by giving
-- the constructor to the ``castTo`` function from 'jsaddle'
castTo ArrayBuffer f32Buff
```

The ``castTo`` function from ``jsaddle`` will return a ``Maybe a`` of your desired cast, to avoid runtime exceptions wherever possible. There is another version ``unsafeCastTo`` that will simply crash if the cast was not possible.

#### First attempt results:

All in all, quite a bit of heavy lifting going on...

```javascript
// JavaScript
function buildBuffer(positions) {
  var buff = new ArrayBuffer(positions.length * 4);
  var f32Arr = new Float32Array(buff);
  for ( var i = 0; i < positions.length; i++ ) {
    f32Arr[i] = positions[i];
  }
  return f32Arr.buffer;
}
```

```haskell
-- Haskell
buildBuffer :: [Double] -> JSM (Maybe ArrayBuffer)
buildBuffer positions = do
  let
    buffSize :: Double
    buffSize = fromIntegral (length positions * 4)

  buff <- new (jsg "ArrayBuffer") buffSize
  f32Arr <- new (jsg "Float32Array") buff
  itraverse_ (f32Arr <##) positions
  f32Arr ! "buffer" >>= castTo ArrayBuffer
```

The different steps translate quite easily to Haskell, and we have the added benefit of the types
and all the various plumbing functions that Haskell provides.

#### Second Attempt

Many WebGL tutorials demonstrate the following technique for creating the ``Float32Array``. Also,
given that the array is backed by an ``ArrayBuffer`` by design, we can simply pull that off the
newly minted array:

```javascript
// JavaScript
function buildBuffer(positions) {
  var f32Arr = new Float32Array(positions);
  return f32Arr.buffer;
}
```

Where I was coming unstuck with my usage of the ``jsaddle`` functions was my understanding of the
[``MakeArgs``
TypeClass](https://hackage.haskell.org/package/jsaddle-0.9.4.0/docs/Language-Javascript-JSaddle-Classes-Internal.html#t:MakeArgs).
Its purpose is to allow you to construct the list of arguments for a function. I needed to pass a
*list of inputs* as a *single* argument to the function. Perhaps obviously, the solution was to
simply place my list in a list. Thus...

```haskell
buildBuffer :: [Double] -> JSM (Maybe ArrayBuffer)
buildBuffer positions = do
  f32Arr <- new (jsg "Float32Array") [positions]
  f32Arr ! "buffer" >>= castTo ArrayBuffer
```

Or if you don't like intermediate variables:

```haskell
buildBuffer :: [Double] -> JSM (Maybe ArrayBuffer)
buildBuffer positions = new (jsg "Float32Array") [positions]
  >>= ( ! "buffer" ) 
  >>= castTo ArrayBuffer
```

Either way, this is a much more concise way of building an ``ArrayBuffer``, and knowing more about
the ``MakeArgs`` typeclass is extremely useful when it comes to creating things like callbacks and
integrating with other JavaScript APIs.

### [Notification API](https://developer.mozilla.org/en-US/docs/Web/API/Notifications_API)

Now, lets annoy, I mean communicate with, some users using desktop notifications! We'll use some of
the functions we introduced in the first section: ``new``, ``jsg``. To access the ``Notification``
object on the window, check our permissions, and try to send a simple notification to the user.
We'll work through the example from the [MDN API documentation
page](https://developer.mozilla.org/en-US/docs/Web/API/Notifications_API/Using_the_Notifications_API). 

#### Accessing the API Object

Earlier we used the ``jsg`` function to acquire a top level JavaScript reference, in that case it
was a constructor: ``Float32Array``. We'll use that function again, but this time we need access to
the object because we need to run some of it's functions. You can think of the ``jsg`` function as a
roughly equivalent to a property accessor for the ``window`` object.

```haskell
notify <- jsg "Notification"
```

Now we can check what our permissions are with respect to the Notifications API and decide what to
do. To do that we need to access a property on the ``Notification`` object.

```haskell
permStr <- notify ^. js "permission"
```

This will give us a ``JSVal`` that is the current permission setting for the ``Notification`` API.
But we have a ``JSVal`` and it could be anything! According to the documentation though, this
property should be a stringly value from a list of three options:

* "denied"
* "default"
* "granted"

We __*could*__ use the ``valToText`` function from ``jsaddle`` to try to change this to a
``JSString``, change that to a ``Text`` value with ``strToText``, and finally ``unpack`` this to a
``String`` value and decide what to do:

```haskell
permStr <- valToText =<< notify ^. js "permission"
case fromJSString permStr where
  "denied" -> ...
  "granted" -> ...
  "default" -> ...
  _ -> ...
```

But we would need to do that every time we checked the permissions, that's not very nice! We're
using Haskell after all, so we will build a data type to represent our permission levels, then tell
``jsaddle`` how to translate from a ``JSVal``. First, the new type:

```haskell
data NotifyPerm
  = Default
  | Denied
  | Granted
  deriving (Show, Eq)
```

Then we need to tell ``jsaddle`` how to translate a ``JSVal`` into our type. We do this by creating
an instance of the ``FromJSVal`` class. In JavaScript land, the permission value is just a string,
so we can use that to solve half the problem when trying to create an instance of our type:

```haskell
instance FromJSVal NotifyPerm where

  fromJSVal :: JSVal -> JSM (Maybe NotifyPerm)
  fromJSVal v = do

    -- Will give us the JSString value of whatever this 'JSVal' is.
    permStr <- valToStr v

    -- Unpack the 'JSString' to a boring Haskell 'String' so we can use a 'case':
    pure $ case unpack $ strToText permS of

      -- Pattern match on the string values
      "default" -> Just Default
      "denied"  -> Just Denied
      "granted" -> Just Granted

      -- Ignore everything that doesn't meet our requirements.
      _         -> Nothing
```

We are able to use ``Generic`` to derive a ``FromJSVal`` instance automatically, using the
``DeriveGeneric`` and ``DeriveAnyClass`` extensions. But this technique won't work the way you want.
By just looking at the ``NotifyPerm`` type, can you see why?

Now that we're able to use a more robust type, we can decide what to do when we know what
permissions we have for creating notifications:

```haskell
permVal <- notify ^. js "permission"
notifyPerm <- fromJSVal permVal
-- Using 'traverse' here lets us write the 'handleNotify' function without 
-- worrying about the 'Nothing' case, which makes our life easier.
traverse (handleNotify notify message) notifyPerm
```

Now we can write our ``handleNotify`` function in a ``let`` or ``where`` binding to keep things neat
and tidy. Or not, referential transparency is lovely like that. We'll need the reference to the
``Notification`` object, and the message, assumed to be a ``Text`` value. Because we don't have to
worry about the ``Maybe``, we can pattern match on our permission, making everything even easier,
again.

```haskell
where
  -- No permission, just return '()' and do nothing.
  handleNotify _ _ Denied = pure ()
  
  -- Permission already granted, create our Notification
  handleNotify nObj msg Granted =
    -- We use 'void' here because we don't need the result of this call to 'new'
    void $ new nObj (ValString msg)
    
  -- The gnarly case, the API will ask the user for permission to show notifications
  -- We must provide a callback function to act on the answer
  handleNotify nObj msg Default =
    -- Call the 'requestPermission' function on the Notification object
    void $ nObj ^. jsf "requestPermission"
      -- This is our callback function, we will go over this in more detail next
      [ fun $ \_ _ [newPerm] -> do
          pV <- fromJSVal newPerm 
          traverse_ (\p -> when ( p == Granted ) $ newNotify nObj ) pV
      ]
```

Lets go over building the callback function, which in ``jsaddle`` is defined as:
[JSCallAsFunction](https://hackage.haskell.org/package/jsaddle-0.9.4.0/docs/Language-Javascript-JSaddle-Object.html#t:JSCallAsFunction).
Using the [``fun``
function](https://hackage.haskell.org/package/jsaddle-0.9.4.0/docs/Language-Javascript-JSaddle-Object.html#v:fun)
as short hand for using a normal Haskell function.

```haskell
type JSCallAsFunction
 =  JSVal    -- The function object
 -> JSVal	 -- "this" object
 -> [JSVal]	 -- The function arguments
 -> JSM ()	 -- Must return unit as the function may run on a different thread

  -- We can discard the first two arguments, using '_', we don't use them. Then we pattern match on the list of arguments
  [ fun $ \_ _ [newPerm] -> do
        -- Take the 'JSVal' and convert it to a 'NotifyPerm'
        pV <- fromJSVal newPerm
        -- Use a traverse to run our function on the 'NotifyPerm' value, if we have one.
        traverse_ (\p -> when ( p == Granted ) $ newNotify nObj ) pV
  ]
```

The purpose of placing the entire call to ``fun`` inside a list is that we want the ``MakeArgs``
typeclass to pass in this function as a single input to the ``requestPermission`` function. Now we
have all the moving parts in place to start using the Notification API in the Browser via GHCJS.
Yay! A version of this integration is
[here](https://github.com/mankyKitty/android-reflex-hello/blob/master/frontend/src/Notify.hs). It
won't work on Android at the moment. Chrome version 62 or higher will require a ``https`` website
before it will enable notifications, but Firefox should work.

#### Conclusion

The ``jsaddle`` and ``jsaddle-dom`` packages are invaluable when working with GHCJS. However they
cover an enormous surface area simply because of the size of the APIs they represent, which may be a
bit daunting. Hopefully I've provided enough information that you've more confidence to delve into
creating your own GHCJS/Reflex applications, and leverage existing APIs without feeling like you
need to reinvent the wheel. Integrate the parts you need, or make some APIs more functional and
easier for the next person to use. 
