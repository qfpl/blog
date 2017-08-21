---
title: Basic Integration of Purescript into a Webpack/NG2/TS Project
date: 2017-08-21
authors: schalmers
---

New tools and languages can be wonderful things. But all too often the excitement around these new things is focused around building an entirely new X, or 'I had some spare time so I rewrote Y'. Where we often need them most is in an existing project, sometimes one that is starting to struggle under its own weight or showing its age.

This post hopes to demonstrate that Purescript is one new tool that you can integrate with an existing project without blowing out your complexity budget. This enables you to start subsuming your legacy code without the horrendous cost of a total rebuild. Additionally this process enables the team to be brought up to speed on Purescript on demand, without requiring an all or nothing approach.

So, with some assumptions, lets begin...

### Some things I have assumed

- A *nix-like terminal environment
- A working knowledge of Javascript
- A basic, or working knowledge of Purescript, and a desire for more
- A functioning install of Purescript 0.11.5, ``purs`` available on the $PATH
- A functioning install of NodeJS with ``bower`` and ``npm`` on the $PATH
- A fresh clone of [angular-starter](https://github.com/AngularClass/angular-starter)

### Preparing the build pipeline
Prepare the angular-starter dependencies by running:

```bash
$ npm install
```

This will install all of the base dependencies required by the standard project. To include Purescript in a Webpack project, you're going to need a loader plugin that will tell Webpack what to do with Purescript files. The simplest option is the [purs-loader](https://github.com/ethul/purs-loader "purs-loader") plugin. We'll also use the [purescript-psa](https://github.com/natefaubion/purescript-psa "purescript-psa") npm package to help with displaying the compiler output from Purescript. Lets install both of those. In your project root:
```bash
$ npm install --save-dev purescript-psa purs-loader
```

Once that is done and assuming everything has completed successfully, you should be able to run ``$ npm start``. This will build the fresh application and open it in a browser window. Click around a bit and familiarise yourself with what is going on, because we'll be breaking it soon.

First off, we have to tell Webpack what to do with the ``.purs`` files that it encounters. Add the following configuration to the 'rules' array in the 'module' property within the common Webpack configuration in ``config/webpack.common.js``:
```javascript
{ 
  test: /\.purs$/,
  use: [
    {
      loader: 'purs-loader',
      options: {
        psc: 'psa',
        output: 'dist', // *** This must match your JS output directory
        src: [
          // This ensures that our library code is included
          'bower_components/purescript-*/src/**/*.purs',
          // This is our source folder where we will keep our Purescript code
          'src/app/purescript/**/*.purs'
        ]
      }
    }
  ]
}
```
The above provides a rule to Webpack that when the test for a Purescript file extension is successful, it will run the 'purs-loader' with the given configuration. The destination for the compiled Purescript files defaults to ``output``, this configuration setting must be changed to be the same destination as the rest of the JavaScript in the project. In our case, we will changing the value of the ``output`` setting to be ``dist``. It also tells ``purs-loader`` to use the ``purescript-psa`` package as the communication gateway with the compiler, making for nicer error messages.

We need to make some other adjustments to our Webpack configuration to ensure that the Purescript code is included in the final application. First add the Purescript file extension to the 'extensions' property:
```javascript
extensions: ['.purs', ...]
```
Finally, we have to ensure that our Purescript modules that are installed via Bower are included in the list of modules that Webpack includes in the build process. If we don't do this then none of the library code we download to use will be included and our code won't be very useful.
```javascript
modules: [
  ...,
  'bower_components'
]
```
Without changing anything else, run ``$ npm start`` to ensure we haven't broken anything. If everything comes up as it did before then we should be ready to start including some Purescript code!

### Adding some Purescript

Lets add something that will do some "work" with the input value before it is placed on the ``AppState``. This will demonstrate how to build a Purescript module that can be used by Javascript like it were any normal package.

Start up the development server for this project so we can start to see any feedback from the compiler or build process.
```bash
$ npm start
```
This should build everything and start up a webserver, additionally it will start watchers that will rebuild the project whenever files change. This includes compiling our Purescript code! It will happily purr away in the background whilst we work.

**NB:** With the way this project is configured, any code that is not actually in use by the application will be discarded. So if you write some Purescript code and you don't see any output regarding it from Webpack, it's likely that is because it's not being used/called by the Javascript code. Thus it will be pruned by the Webpack tree-shaking/dead-code elimination process.

Navigate to the ``src/app`` folder and create the ``purescript`` directory.
```bash
$ cd src/app
$ mkdir purescript
```
Or your operational equivalent...

Then create our first basic module, lets call it ``Basic``.
```bash
$ cd purescript
$ touch Basic.purs
```
Open this file in your favourite editor. 

Start with the Purescript module declaration. This line is required in every Purescript file and provides not only the name of the module, but also lists what this module exports. The requirement being that anything it exports must be in scope, and the module name must match the file name.

To start with, we'll put the following at the top of our file:
```haskell
module Basic where
```
We're not listing anything to export yet, so Purescript will export everything we define. We'll fix that later.

Our function will be simple: duplicate the input string. It will take a ``String`` and return a ``String``. So if we're given ``"Foo"`` then we'll return ``"FooFoo"``.

To do that we'll use a function from the Purescript ``Prelude`` to combine our input. Move down a couple of lines and then add the following:
```haskell
doubler :: String -> String
doubler inp = inp <> inp
```
The operator, that ``<>`` thing in the middle, is an ["infix function"](https://github.com/purescript/documentation/blob/master/language/Syntax.md#binary-operators) defined in the ``Prelude``. It's from the ``Semigroup`` typeclass, and when used with Strings will concatenate the given inputs.
```haskell
"fuzz" <> "fuzz" = "fuzzfuzz"
"foo" <> "bar" = "foobar"
```
Now we have some bookkeeping to do. to ensure that both the ``String`` type and the ``<>`` function are in scope. So lets add a ``Prelude`` import towards the top of our file to take care of this.
```haskell
import Prelude
```
We also need to make sure that the Purescript Prelude module is actually installed:
```bash
$ bower install --save purescript-prelude
```
Now change the module declaration at the top of the file so that we're only exporting this one function. To do that we add a list of function names before the ``where`` keyword. Surrounded by parentheses and separated by commas: So our ``module`` line changes from:
```haskell
module Basic where
```
To:
```haskell
module Basic (doubler) where
```

### Importing to JavaScript

Including this module in our Javascript code is the same as using any other module, with a few gotchas that we'll touch on later. For now, open up the ``home.component.ts`` file and look for the ``submitState`` function.

This function takes the input from the text field on the 'Home' page, pushes it to the ``appState`` and then clears the value. We're going to intrude on this process and pass the input through our function, before setting the value on the ``appState``.

First, import the function from our Purescript module. Purescript compiles to CommonJS modules, so importing into Javascript land is quite easy. This project provides ES6 features, so we're able to use the new import syntax:
```javascript
import * Basic from '../purescript/Basic';
```
This is the same as:
```javascript
const Basic = require('../purescript/Basic');
```
You can do selective imports as well, if the functions are exported from your Purescript module:
```javascript
import { doubler } from '../purescript/Basic';
```
Using the first import example, we have our Purescript module imported as 'Basic', so we're able to use it to modify our input. Go to the ``submitState`` function and change the following line:
```javascript
this.appState.set('value', value);
```
To be:
```javascript
this.appState.set('value', Basic.doubler(value));
```

### Kicking the tyres

If your development server isn't running, you can start it by typing ``$ npm start`` at the root of the project. 

On the 'Home' page of the web-app, you should be able to type something into the 'Local State' text field and click the ``Submit Value`` button. As you type you should see the ``localState`` being updated with the latest value. Typing "Turbo" should take this:
```javascript
this.localState = {
  "value": ""
}
```
... to this
```javascript
this.localState = {
  "value": "Turbo"
}
```
Clicking ``Submit Value`` should then change both our ``localState`` and our ``appState`` to the following values:
```javascript
this.localState = {
  "value": ""
}
this.appState.state = {
  "value": "TurboTurbo"
}
```
If you see the duplicated value on the ``appState`` then congratulations, you have successfully integrated your Purescript module with the larger Javascript application.

This was a pure function, simple inputs and outputs with no side-effects. However you can still notice the benefits that this might provide if you have a particularly gnarly piece of business logic, or you would like to lean on the abstractions available in Purescript.

Perhaps more importantly, this technique and what will be covered in the next article provide the basis for gradually subsuming the Javascript in a project with Purescript. Components and functionality can be replaced piece by piece without requiring an 'all or nothing' type approach.

### Where to from here?

Next time we'll build a standalone UI component using [Flare](https://github.com/sharkdp/purescript-flare), and then demonstrate how to pass data from the Flare component back into our free wheeling javascript world.
