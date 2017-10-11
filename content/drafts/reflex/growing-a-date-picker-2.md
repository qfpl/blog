---
title: Growing a Date Picker in Reflex - Part 2
date: 2017-10-11
authors: schalmers
project: reflex
---

Continuing on from the [previous post]() about our shiny new date picker, there has been a lot going on and the various pieces are really starting to come together. In this post we discuss a large scale (re?)factoring of the various components into self-contained pieces.

This post is more of a 'developer diary' style and may not be as straight forward as post number one.

#### Refactoring

Spurred on by [this comment](https://www.reddit.com/r/haskell/comments/74mnnk/growing_a_date_picker_in_reflex_part_1/do3g6nx/), I started to factor out the larger pieces of the date picker into their own functions, and eventually their own modules. Initially this was to see if I could make the 'day' display widget a standalone item. This soon snowballed however.

#### Modules!

There are now three primary components that are available for individual use:
- ``Controls``: This is the 'Previous' and 'Next' month buttons, along with the text input. Providing the ``Event``s for the clicks and text input.
- ``DaySelect``: This is the widget that will display a list of days and provide an ``Event`` when one is clicked.
- ``Core``: This is the core functionality of the date picker widget. Providing two ``Dynamic``s for the ``Day`` and ``[Day]`` based on the current inputs.

Overall it was quite an enjoyable experience, moving all of these things into their own little boxes. There is still work to be done, especially on the ``DaySelect`` component as there are a couple of pieces of functionality that are required.

The ability to style the day that has been selected is a required feature. But one that should not be too difficult to implement. Although there quite a few ways of handling that. Initial thoughts lean towards changing the styling ``Dynamic`` from:
```haskell
Dynamic t (Map Text Text)
```
To something like:
```haskell
Dynamic t (Day -> Day -> Map Text Text)
```

This in turn could be combined with a function:
```haskell
Map Text Text -> Day -> Day -> Map Text Text
```
That would let you make adjustments to the default styling for the ``Day`` based on, for example, comparison to another ``Day`` value. So if the ``Day`` values match, in the case of trying to render the ``Day`` that has been clicked, you can adjust the styling to suit. The construction of that function would be left to the end user, with some default implementations provided for people that don't care to mess with it.

This feature hasn't been implemented yet, as there is more discovery to happen and there might already be Reflex idioms/functions that handle this sort of thing.

There is also the need to be able to select a range of days. Either by clicking start and and end days, or clicking and dragging. That's a bit more advanced and would require some other moving parts.

### Styling additions

In order to make some of the styling possible for the various component layouts, there is a need to wrap groups of elements in a ``div`` or similar parent element. Whilst "make it work" was the priority, this was handled by hard-coding in the wrapping element. But that isn't a viable solution for end users as their needs will always be a bit different and may require messing with how the elements are contained.

Easy to assume they'll be better CSS too, so best not bind them to my attempts at a flexible layout.

To that end, I generalised the technique I had used in earlier stages by having a ``newtype`` that contained a wrapping function:
```haskell
newtype Wrap a t m = Wrap
  { wrapEl :: forall e. MonadWidget t m => m e -> m e
  }
```
This type contains a ``Phantom Type`` that let you specify type level information about what sort of element you will be wrapping. As well as ensuring that the ``t`` and ``m`` line up. It is constructed using a function that is effectively the identity of the element that is passed in. But gives a chance to wrap the inner element in something else.

To use it, specify a type to provide information about what you're intending to use it for:
```haskell
data ControlW
```
This is a void type as the value is not relevant, it's a type level restriction to ensure we don't try to wrap things with the wrong wrapper.

For our example we'll just use a ``div`` with a class of our choosing:
```haskell
import Reflex.Dom (MonadWidget, divClass)

controlWrap :: MonadWidget t m => Wrap ControlW t m
controlWrap = Wrap (divClass "my-controls")
```

Then, when you want to wrap the element:
```haskell
myEl :: MonadWidget t m => m (Event t ())
myEl = ...

wrappedEl :: MonadWidget t m => Wrap ControlW t m -> m (Event t ())
wrappedEl myWrap = wrapEl myWrap $ myEl
```

This lets you specify specific wrappers for given elements:
```haskell
-- Separate the wrappers for different elements clearly
dayList :: MonadWidget t m => ... -> Wrap DayW t m -> Wrap DayListW t m -> ...
mkDatePickerControls :: MonadWidget t m => ... -> Wrap ControlW t m -> ...
```
Allowing the type system to help you use the correct wrapper.

This is something that is just used in this project, and it's trivial enough that you can make your own should you feel so inclined. But there are possibilities for creating higher order functions with this style that would allow for safer definitions of frameworks like [Bootstrap](https://getbootstrap.com/). Stating clearly in the type that a function named ``cFluid`` has a type of ``:: Wrap ContainerFluid t m`` and you don't have to rely on the name only to use the right wrapper in the right spot.

There are type level functions that would make that even nicer but that is beyond the scope of this post.

#### Pretty!

There is now some CSS included with the date picker and the layout of the controls and the list of days in the month now looks like something worthwhile. There is no inclusion of indicating which ``Day`` has been selected yet. it's a work in progress and there many yaks lining up for this particular barber.

### Oh no, a bug!

Overall the process of breaking out the different pieces into their various modules was quite painless, the type system told me about the things I'd forgotten, and the semantics of FRP and how the ``Event``s and ``Dynamic``s fit together ensured that nothing untoward happened. Mostly...

One issue I encountered, and it was entirely my fault, was wanting to find a cleaner way to express the construction of an ``Event t Day`` from the ``Dynamic t Day`` and ``Event t ()``. What I had was:
```haskell
let ePreviousMonth = prevMonth <$> current dDayValue <@ ePrevMonthClicked
```
Which creates an ``Event`` that contains a value of the previous month, based on the current value of the ``Dynamic``, tagged at the time of the ``Event t ()`` from the previous month button being clicked.

This is a common enough expression in FRP, however I wondered if there was a simpler way to express this, given how common it seemed to me. What I found was a function:
```haskell
tagPromptlyDyn :: Dynamic a -> Event b -> Event a
-- Vs the 'tag' operator and 'current'
current :: Dynamic a -> Behavior a
(<@) :: Behavior a -> Event b -> Event a
```
So my expression became:
```haskell
let ePreviousMonth = prevMonth <$> tagPromptlyDyn dDayValue ePrevMonthClicked
```
Saved the file, compiled the code, reloaded the page, and nothing worked. Damn...

The issue is that whilst ``tagPromptlyDyn d e`` resembles ``tag (current d) e``, it has different functionality with respect to acquiring the value of ``d``. Explained in detail [here](https://github.com/reflex-frp/reflex/blob/fe21a501f7db4a3dbf5f7727c37bbe33fafee9ac/src/Reflex/Dynamic.hs#L239). I had created the exact scenario that the comment is referring to. As this particular ``Event`` is used to update the value of the ``Dynamic`` that I am using the ``current`` value of, in order to compute the new value. This creates a dependency cycle, thus the widget did not function.

Changing it back to use ``current d <@ e`` made everything happy once again. ``tagPromptlyDyn`` does have its uses, but in this case the functionality was incorrect for what I was trying to achieve.

## Just keep swimming...

The date picker is progressing well and it's nice to see people interested in it so I hope I can make something useful. Next up is more dynamic and flexible styling capabilities for the list of days. As well is cleaning up the code and providing more useful comments and documentation on how one might actually use it.

As well as embedding a copy of the date picker in this post if I can work it out...
