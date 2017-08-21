---
title: Integrating Purescript Flare UI Component into Webpack/NG2/TS Project
date: 2017-08-21
authors: schalmers
---

Picking up from where we left off, we should have a [angular-starter](https://github.com/AngularClass/angular-starter) project with some integrated Purescript code. To improve on this, we're going to now create a small UI component using Purescript and display it in the Javascript application. This UI will take some input, mess with it a bit, and display the result. 

After that we'll take some state from the Javascript application and pass it to our Purescript UI component. Letting the Purescript component do the work, updating the ``localState`` as it goes. Removing the need for any Angular code in managing the UI or the state. 

### Things I have assumed...

- A *nix-like terminal environment
- A working knowledge of Javascript
- A basic, or working knowledge of Purescript, and a desire for more
- A functioning install of Purescript 0.11.5, ``purs`` available on the $PATH
- A functioning install of NodeJS with ``bower`` and ``npm`` on the $PATH
- Have worked through the tutorial for [Integrating Purescript to a Webpack Project](purescript-wp2-ng2-ts)

### Adding a bit of Flare

[Flare](http://david-peter.de/articles/flare/) is a special purpose UI library that sacrifices some flexibility for expressiveness and simplicity. It's this simplicity that makes it a good candidate for our demonstration. As an example, the following Flare code:
```haskell
lift2 (*) (int "a" 6) (int "b" 7)
```
Will produce a UI with two ``number`` type text fields, labelled "a" and "b" respectively, alongside a display of the result of multiplying the input value of "a" by "b". See the Flare website for more examples.

Install the Flare package and then we'll start with a self-contained UI component.
```shell
$ bower install --save purescript-flare
```
Now create a new module ``Flarey.purs``, next to our ``Basic.purs``:
```haskell
module Flarey where
```
Import some Flare components, and some other bits we'll need...
```haskell
import Flare (UI, string_, runFlare)
import Control.Monad.Eff (Eff)
import Signal.Channel (CHANNEL)
import DOM (DOM)
import Prelude (Unit, pure, (<>))
```
Create a small UI component, comprising of two text input fields that combine their inputs:
```haskell
combineTextUI :: forall e. UI e String
combineTextUI = string_ "" <> pure " " <> string_ ""
```
This function describes the individual UI component, however it doesn't actually run it, we need to pass it to the [**runFlare**](https://pursuit.purescript.org/packages/purescript-flare/3.1.0/docs/Flare#v:runFlare) function for that. That function requires a HTML Element ID value so it knows where the UI input component is to be placed, as well as a Element ID for where the output will be placed.

Lets add these IDs as input to our function from the Javascript side so we don't have to hard code anything:
```haskell
combineTextEle :: forall e. String -> String -> Eff ( dom :: DOM, channel :: CHANNEL | e ) Unit
combineTextEle inputEleId outputEleId = runFlare inputEleId outputEleId combineTextUI
```
Now to add this to the ``home.component``... First add the required elements to the HTML. Open ``home.component.html`` and just above the heading for 'Local State', add the following:
```html
<h4>Purescript Flare</h4>
<div>
  <div id="home-flare-in"></div>
  <div id="home-flare-out"></div>
</div>
```
Then in our ``home.component.ts`` import our ``Flarey`` module and wire everything up. This time however we don't want to have to wait for a submit to happen before our UI component is available on the page. So we'll put our call the create our Flare UI component in the ``ngOnInit`` function.

Import The Module, ES6 Style:
```javascript
import * as Flarey from '../purescript/Flarey';
```
Then add the instantiation function to ``ngOnInit``:
```javascript
Flarey.combineTextEle("home-flare-in")("home-flare-out")();
```

### Purescript functions only take one argument.

Note that each input to the ``combineTextEle`` function is given as a separate function call. This is intentional and is because every Purescript function only ever takes one argument. Returning either a result or a function that needs more input.

In the previous tutorial, our ``doubler`` function took one String argument and returned a String result, without any side effects. 
```haskell
-- Purescript
String -> String
```
```javascript
// Javascript
Basic.doubler
```
So we're able to call it as we would any normal single argument Javascript function.
```haskell
-- Purescript
String
```
```javascript
// Javascript
Basic.doubler("foo");
```
This time our function signature is:
```haskell
-- Purescript 
forall e. String -> String -> Eff ( dom :: DOM, channel :: CHANNEL | e ) Unit
```
```javascript
// Javascript
Flarey.combineTextEle
```
Pass in our first argument and that returns the following function:
```haskell
-- Purescript 
forall e. String -> Eff ( dom :: DOM, channel :: CHANNEL | e ) Unit
```
```javascript
// Javascript
Flarey.combineTextEle("home-flare-in")
```
Then the second, returning the following function:
```haskell
-- Purescript 
forall e. Eff ( dom :: DOM, channel :: CHANNEL | e ) Unit
```
```javascript
// Javascript
Flarey.combineTextEle("home-flare-in")("home-flare-out")
```
This is an effect causing function, that must be evaluated to produce the desired effect. Often this is handled by the ``main`` function of a Purescript application. In Javascript, it is simply a function that returns null and has some external effect. In our case, making the necessary DOM adjustments and activating some event handlers.
```haskell
-- Purescript 
Unit & *spooky sounds*
```
```javascript
// Javascript
Flarey.combineTextEle("home-flare-in")("home-flare-out")()
```
For this standalone component that is all you need to do. If you don't have it running already, kick off the development server and you should see another two text fields on the home page the display your input in the container you specified. Weeeeeeee!

### Communicating With JavaScript

So far our interactions have been either a pure function (``String -> String``), or they have been interactive but self-contained, in the case of Flare UI. A more useful example is having a Flare UI component that handles some state from the Javascript application.

The steps we need to take are:
- Update our Flare UI component so that it accepts a function to run over the input
- Update the JavaScript code to pass in a function that will update our ``AppState``

Helpfully, Flare comes with a couple of functions that let us run effectful functions over the output of a Flare UI Component instead of applying the output to the DOM.

To start with, we need [**runFlareWith**](https://pursuit.purescript.org/packages/purescript-flare/3.1.0/docs/Flare#v:runFlareWith).

This requires a function that will be passed the result of the Flare UI component, allowing for effectful actions to take place. This function will be passed in from the Javascript code, so although we will give it a Purescript type, that type will be a lie. As the Javascript is under no obligation to obey this type, nor can we actually enforce it. However by giving it a type on the Purescript side, at no point are we under any illusions and the typechecker will prevent us from misusing it.

The Purescript type of this function is:
```haskell
type InputFn = forall eff. String -> Eff eff Unit
```

Normally, you might write such a type signature like this:
```haskell
type InFn eff = String -> Eff ( dom :: DOM | eff) Unit
```
Because you want to indicate in the type of the function that it may cause DOM changes to occur. You're also able to trace back all of the other effects that are in play when this function is called because the type requires that they be passed in through the ``eff`` type variable.

However any function from Javascript will have no such constraints and we cannot guarantee anything about what it may or may not do. So we assume the worst and declare that this function may perform any possible effect: ``forall eff.``. 

We cannot force Javascript to comply with our types, but we can at least provide some level of confidence within our Purescript code that we're handling this callback correctly, by giving it a type signature that describes the very lack of control we must be cautious of.

Another change is that we are no longer relying on Flare to display this result in the DOM. So we do not need the "output" element ID. We will replace it with our ``InputFn`` and our new ``combineTextEle`` function should look something like:
```haskell
combineTextEle :: forall eff. String -> InputFn -> Eff ( dom :: DOM, channel :: CHANNEL | eff ) Unit
combineTextEle elemId fn = runFlareWith elemId fn combineTextUI
```
Update the import for Flare to import ``runFlareWith`` instead of ``runFlare``.

In ``home.component.ts`` update our ``combineTextEle`` function call to match the new arguments. As we are dealing with a foreign function interface between Javascript and Purescript, we must accept that Purescript cannot help beyond the boundaries. Nothing can. Such is the nature of an FFI.

Our goal here is to create a Flare UI component that will operate on some state that we give to it. Update the ``localState`` property to have a new key:
```javascript
public localState = {
  ...,
  flareValue: ''
};
```
Now define our 'effectful function' that we will pass to Purescript. Recall from earlier that every Purescript function only takes one argument, and that ``Eff`` functions are 'thunks' that are evaluated to run their respective effects. Knowing that, we have to to create this:
```haskell
:: forall eff. String -> Eff eff Unit
```
In our ``ngOnInit`` function, lets define this function to update our ``localState`` with the given input:
```javascript
var inputFn = (str) => this.localState.flareValue = str;
```
This isn't quite right though, as we have to construct our 'effectful' function as the final return value. Otherwise when this function is run by Purescript it won't work as expected. In this case we only have to build an ``Eff`` thunk, which takes no arguments and returns no output (``Unit``).

We do this by wrapping our function in another function:
```javascript
var inputFn = (str) => () => this.localState.flareValue = str;
```
Notice how this mirrors the FFI requirements for Purescript:
```haskell
-- Purescript
 String -> Eff eff Unit
```
```javascript
// Javascript
 (str) => () => ...

```
We can now replace our second argument to the ``Flarey.combineTextEle`` with this ``inputFn`` variable:
```javascript
Flarey.combineTextEle( "home-flare-input" )( inputFn )();
```
With your app running, you should see a new element present in the display for ``localState``:
```javascript
this.localState = {
  "value": "",
  "flareValue": " "
}
```
If you type something into the fields, you should then see their combined output displayed as you type:
```javascript
this.localState = {
  "value": "",
  "flareValue": "Turbo Snail"
}
```

If things are going according to plan then you have successfully integrated a well typed Purescript UI component into an existing Javascript Angular2 application. It may not seem like much, but now you can start to replace entire pieces of your DOM altering code with statically typed UI components. More importantly you can start to do this right now, without having to rebuild your entire application!

