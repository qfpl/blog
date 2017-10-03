---
title: Growing a Date Picker in Reflex - Part 1
date: 2017-09-29
authors: schalmers
project: reflex
---

[Reflex]: (https://github.com/reflex-frp/reflex)
[Reflex-DOM]: (https://github.com/reflex-frp/reflex-dom)
[FRP]: (https://gist.github.com/staltz/868e7e9bc2a7b8c1f754)
[ReflexIntro]: (https://qfpl.io/posts/reflex/basics/introduction/)
[ManningFRP]: (https://www.manning.com/books/functional-reactive-programming)

## Stay a while, and listen.

In the interests of learning more about [FRP] and [Reflex], I decided to try to build a component
that appears in many a user facing application, the date picker. This series will be a tale of that
process. The goal is to document as much as I can about designing, implementing, and debugging the
component. As well as the lessons learned along the way, so expect to see me making lots of
mistakes.

This also seemed like a suitably meaty challenge to try to level up my [Reflex] skills, and if I am
successful then the Reflex ecosystem hopefully gains a sufficiently generic date picker widget. Or
I'll do everything wrong, then someone will be able to look at what I've done and have a list of
what you shouldn't do. Win win, really.

#### Some assumptions...

I assume an understanding of [Reflex] and [FRP]. If you don't know [FRP] or [Reflex] yet, or you
find yourself a bit lost at times, I must recommend the [Reflex Introduction][ReflexIntro] series by
Dave Laing, and if you're keen the [Functional Reactive Programming][ManningFRP] book by Stephen
Blackheath and Anthony Jones is a wonderful resource.

### Why a date picker?

In a previous role, my colleagues and I would use the phrase, "Show me your date picker", as a
subjective and slightly silly metric for evaluating JavaScript frameworks. We required a date picker
widget in every application and it began to be an interesting indicator of the maturity and
reliability of the framework in question. Since a date picker is quite a complex beast at the best
of times as it requires complex styling, state management, and more configurable options than one
cares to mention.

### Basic Design

The following are the goals or loose specification for a '0.1' release:

- Only handle selecting a date, not a time.
- Display a text input field for manual date entry.
- Accept a configurable format for both the date and the list of days in the relevant month.

- Display buttons to move to the next/previous month.
- Display a list of days for the month that correctly matches the latest valid input date.
- Allow flexible styling for the input, day display, and buttons.

- If a day is clicked, then the text input is updated to match.
- If the next month is selected and we're at the end or start of the year then rollover correctly.
- If the text input is not valid, don't break the UI and don't allow bad data to propagate.
- If the text input is valid then update the list of days if needed

The plan was to start with an existing Reflex ``textInput`` and expand from there.

### Always start with the data structures

**Plan (A)** is to imitate the style of the existing [Reflex-DOM] input widgets, to start I created
two records:

- One to hold all the required configuration to build and run our date input
- Another to be handed to the user so they could manage the date ``Dynamic`` and various related ``Event``s.

As we are using the ``textInput`` internally, some of it's requirements flowed through to the
``DateInputConfig`` structure. Plus some extra information required to handle the ``Day`` value.

```haskell
data DateInputConfig t = DateInputConfig
  { _dateInputConfig_initialValue   :: Day         -- ^ Starting value
  , _dateInputConfig_dateFormat     :: DateFormat  -- ^ Formatter to be used to check any text input
  , _dateInputConfig_dayFormat      :: DayFormat   -- ^ Formatter for displaying the days in the month
  , _dateInputConfig_timelocale     :: TimeLocale  -- ^ This is required for formatting / parsing
  , _dateInputConfig_setValue       :: Event t Day -- ^ Fires on selecting or inputting a valid date
  , _dateInputConfig_textInputAttrs :: Dynamic t (Map Text Text)
  ...
```
Similar to the ``TextInputConfig`` structure:
```haskell
data TextInputConfig t = TextInputConfig
  { _textInputConfig_inputType    :: Text
  , _textInputConfig_initialValue :: Text
  , _textInputConfig_setValue     :: Event t Text
  , _textInputConfig_attributes   :: Dynamic t (Map Text Text)
  }
```

Then consider what we need to provide to the user of this widget to provide sufficient utility.
Since we're just ripping off the text input widget for now, this isn't anything terribly
interesting.

```haskell
-- The Modified Julian `Day` is a standard count of 
-- days, with zero being the day 1858-11-17
data DateInput t = DateInput
  { _dateInput_value       :: Dynamic t Day -- ^ Our date picker value

  -- Text input box for date selection, Events and 
  -- HTML Element from the underlying widget
  , _dateInput_rawInput    :: Event t Text
  , _dateInput_keypress    :: Event t Word
  , _dateInput_keydown     :: Event t Word
  , _dateInput_keyup       :: Event t Word
  , _dateInput_hasFocus    :: Dynamic t Bool
  , _dateInput_textElement :: Input.HTMLInputElement
  }
```

### Consuming text input

First up is displaying the text input and parsing the input, only using the new date if the input
successfully parsed using the format we were given. This proved to be straightforward enough using
the basic [Reflex] tools.

First we need a ``textInput`` on to provide us with something to work with:
```haskell
tI <- textInput $ def
  & textInputConfig_initialValue .~ dateCfg ^. dateInputConfig_initialValue . to fmtDate
  & textInputConfig_attributes .~ dateCfg ^. dateInputConfig_textInputAttrs
  & textInputConfig_setValue .~ (fmtDate <$> updated dDayValue)
```
><small>
>``^.`` and ``.~`` are from [Control.Lens](https://hackage.haskell.org/package/lens), in case you hadn't
>seen them before. In the simplist terms, they are getters and setters, respectively, to simplify
>updating the default ``TextInputConfig`` record.
> </small>

The ``fmtDate`` will format the given ``Day`` using the provided format from the ``DateInputConfig``
before setting it as the current value on the text input. We also pass on the attributes to the text
input field in case there is extra styling or related shenanigans that the user would like to
leverage. There is the possibility you can do something silly with that, but for now lets [pretend
that won't happen](https://xkcd.com/908/).

We also provide an ``Event`` that we will fire when we have a new ``Day`` value and we want to
update the contents of the text field. In true [Reflex] fashion, we haven't defined ``dDayValue``
yet, but that will be ``Dynamic t Day`` we use to build the list of days, calculate the next and
previous month values, and finally provide to the user.

The ``textInput`` will provide us with, amongst other things, a ``Dynamic t Text`` that will contain
the input values from the user. We need to parse this value over time and _if_ it is valid then we
update our ``Dynamic t Day``. 

We use ``updated`` from [Reflex] to retrieve the ``Event t Text`` from our ``Dynamic t Text``: 
```haskell
let eDateTextInput = updated $ tI ^. textInput_value
```

Then we need to run our parsing function over ``Text`` value each time the ``Event`` fires.
Thankfully the ``Event`` is an instance of ``Functor``, so that is straight forward enough:

```haskell
parseDay <$> eDateTextInput
```

Except that will give us an ``Event t (Maybe Day)`` and the ``Nothing`` values aren't terribly
interesting to us yet. So we could do this:

```haskell
(fromMaybe someDayValue . parseDay) <$> eDateTextInput
```

This will provide us clear out the ``Maybe`` to just a ``Day`` value whenever the text input is
updated even if it isn't valid.

But there are a couple of things wrong with this... For starters, how do we select the correct value to put
in as the default? We could tag the ``current`` value of the ``Dynamic t Day`` at the time of
this event, but that doesn't make much sense because we'd be performing unnecessary updates with an
identical value. We could use the initial value from the ``DateInputConfig`` but that value is stale
from the moment the user selects/inputs any other value.

Regardless of the choice of default, by using this solution we would be spamming updates on **_every_**
update to the text input. Potentially performing a DOS attack against our own widget.

So we want to run our function but filter for events where we have a valid value. Turns out that
[Reflex] has a function for this exact situation, ``fmapMaybe``. The ``Event t Day`` now looks like
this:
```haskell
fmapMaybe parseDay eDateTextInput
```
This creates an ``Event t Day`` that will only fire when the ``textInput`` contains text that
successfully parses using the provided date format, perfect.

### Creating our Dynamic

We have the ``Event t Day`` from the ``textInput``, but we also need to include any update
``Event``s from outside our little world. So, just like the ``TextInput``, we included a ``Event t
Day`` that on the ``DateInputConfig``. This is expected to be fired external to our widget with an
update to the value of our widget:

```haskell
dateCfg ^. dateInputConfig_setValue
```

Now that we have an ``Event`` that will fire on valid ``Day`` updates, and we can accept any updates
from outside our widget, we can construct the ``Dynamic t Day`` that will be given to the user.
Using our given initial value, plus the two ``Event``s described above:

```haskell
dDayValue <- holdDyn (dateCfg ^. dateInputConfig_initialValue) $ leftmost
  [ dateCfg ^. dateInputConfig_setValue
  , fmapMaybe parseDay eDateTextInput
  ]
```

Now we can build the return structure for our ``dateInput`` so that we can build a mock page to
ensure all the right values are flowing through:
```haskell
return $ DateInput
  dDayValue
  (tI ^. textInput_input)
  (tI ^. textInput_keypress)
  (tI ^. textInput_keydown)
  (tI ^. textInput_keyup)
  (tI ^. textInput_hasFocus)
  (_textInput_element tI)
```

### Something is not quite right...

During testing, the page would quickly become unusable even though the various inputs appeared to be
flowing through correctly. Invalid dates were being filtered out and valid ones were triggering the
expected updates. If you have a quick scroll back through the ``Dynamic`` and ``Event`` values that
were constructed and how they were used, can you see where I went wrong?

The issue was that I had built an ``Event`` loop that would trigger itself and lead to an infinite
loop. Whoops. Let's have a look at that...

Starting with the ``Event`` from the text input:
```haskell
let eDateTextInput = updated $ tI ^. textInput_value
```
This ``Event`` is attached to the ``Dynamic`` of the text input, so it will fire for every update to
the value of the ``TextInput``.

The next piece is the update to our ``Dynamic t Day``, specifically one of its update ``Event``s. 
```haskell
dDayValue <- ...
  [ fmapMaybe parseDay eDateTextInput
  ...
```

The final piece is the ``Event`` we set on our ``TextInputConfig`` to update its value when we
have a new valid ``Day`` value:
```haskell
textInputConfig_setValue .~ (fmtDate <$> updated dDayValue)
```
With the above update ``Event`` and a little bit of inlining we can start to see the problem:
```haskell
dDayValue <- ...
  [ fmapMaybe parseDay ( updated $ tI ^. textInput_value )
  ...
```
Note that the text field will be updated by the ``Event`` of ``dDayValue`` being updated. 

* Text field updated
* Fires Event to parse value
* Valid value updates Dynamic
* Dynamic fires Event on update to set value on the Text field with new value
* Text field updated
* ...ad nauseam.

I was trying to set the value of the text field, that was triggered by an ``Event`` of parsing a
valid ``Day`` input, from the ``Event`` that was fired because text was entered into the text field
that fires events when it is updated... [Oh dear](http://gunshowcomic.com/648).

### Untie the knot 

The fix for this is to untie the updates of the text field from the updates of our ``Dynamic t
Day``. Since, perhaps obviously to some of you, the text field doesn't need to be notified when it
has a valid ``Day`` value entered. Since that value is, by definition, in the text field.

If we refer back to our specification, such as it is, the only times when we will need to format a
``Day`` value into our text field are:

- Next / Previous month button is clicked
- A day is clicked from the list of days in that month
- The provided ``Event t Day`` from our ``DateInputConfig`` is triggered

The only times we need to update our ``Dynamic t Day`` are all of the above, plus:

- We parse a valid ``Day`` input from the ``textInput``

We haven't written the next/previous month buttons, or the list of days, but lets pretend for a
moment and create the ``Event`` to satisfy our non-broken requirements above:

```haskell
let eDateUpdate = leftmost
      [ ePrevMonth -- ^ Event containing the previous month, clipped to valid day in that month
      , eNextMonth -- ^ Event containing the next month, clipped to valid day in that month
      , eDaySelect -- ^ Event containing the Day that the user clicked on the UI
      , dateInpCfg ^. dateInputConfig_setValue -- ^ Externally triggered Event
      ]
```

The respective update ``Event``s for our ``textInput`` and the ``Dynamic t Day`` are now as follows:
```haskell
tI <- textInput $ def
  ...
  & textInputConfig_setValue .~ ( fmtDt <$> eDateUpdate )

dDayValue <- holdDyn initialVal $ leftmost
  [ eDateUpdate
  , fmapMaybe parseDate eDateTextInput
  ]
```
These changes de-couple the updates of the ``textInput`` from the changes to the ``Dynamic t Day``,
without allowing it to fall out of sync if there are other relevant update ``Event``s. Additionally
the ``Dynamic t Day`` doesn't miss out on any updates or valid changes to the ``textInput``. More
testing indicated no page slow down and no more loops, hooray.

## What's next?


Now that we have the core update structure built, we can move on to some fun things like adding in
the next/previous month functionality. As well as the clickable list of days in the month, and some
suitably ``Dynamic`` styling. _Terrible CSS Warning_

Also we'll write some tests for our widget so we can make sure that everything works as desired and
to see just how we test and verify our [Reflex] widgets.
