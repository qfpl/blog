---
title: Functional Programming Cheat Sheet
date: 2017-08-10
authors: tmorris
---

What follows is translation from traditional programming constructs to how we might express the same, or similar, program using a Functional Programming approach. It is a loose translation, in no particular programming language syntax, then demonstrated in FP using the Haskell programming language.

In some of the examples, there will be reference to a *function*. This can be directly translated to what is used in languages such as Java and C# as an interface. For example, this Java interface…

```java
interface Integer2String {
  String apply(Integer i);
}
```

…used at a call site with the value `n`…

```java
i2s.apply(n);
```

…can be thought of as a function called `i2s` applied to a value `n`, using Haskell syntax:

```haskell
i2s n
```

##### Direct links

* [liftA2 on []]
* [Option/Maybe monad]
* [liftA2 on reader]
* [sequence on Maybe with list]
* [sequence on reader with list]
* [sequence on list with list]
* [join on reader]
* [kleisli composition on Maybe]
* [kleisli composition on reader]
* [kleisli composition on State]
* [sequence on reader with Maybe]
* [choice on Maybe]
* [Do effect on condition]
* [Do effect on not-condition]

----

### liftA2 on []

Given a function `func`, and two lists `list1` and `list2`, loops first through
`list1`, then `list2` and apply a function to the element at that position.

```csharp
given(func, list1, list2) {
  var r = List.empty
  for(int i = 0; i < list1.length; i++) {
    for(int j = 0; j < list2.length; j++) {
      r += list1[i].func(list2[j])
    }
  }
  return r
}
```

This can be written using `liftA2` which works for many values, including lists. The first argument to `liftA2` is the function to apply to the list elements. The subsequent arguments are the two lists.

```haskell
liftA2 func list1 list2
```

The `liftA2` function applies to "one loop within one more loop". You can correlate the number `2` with the number of loops. There are other `liftA*` functions for embedding more inner loops.

This general pattern is called *Applicative Functor* and is described in the paper, [Applicative Programming With Effects](http://www.staff.city.ac.uk/~ross/papers/Applicative.html). 

----

### Option/Maybe monad

The `Maybe` data type, sometimes called the `Option` data type, can be thought of as a list with a maximum length of one. It is a type-safe substitution for what is often represented as `null` in other programming languages.

Given two possibly `null` values, `x` and `y`, then if either value is `null`, return `null`, otherwise apply a function (such as `+`) to the two values, knowing that they are not `null`:

```csharp
given(x, y) {
  if (x == null) {
    return null
  } else if(y == null) {
    return null
  } else {
    return x + y
  }
}
```

This can be written using the `>>=` function, which is overloaded to work on many values, of which `Maybe` is one instance:

```haskell
x >>= \p ->
y >>= \q ->
pure (p + q)
```

This particular code can also be written using `liftA2`:

```
liftA2 (+) x y
```

However, when the second `null` check depends on the value of the first one, then using the `>>=` function is required.

For example, given one value `x`, and a function `test`, which takes `x` as argument, for the second `null` check:

```csharp
given(x, test) {
  if (x == null) {
    return null
  } else {
    val y = test(x)
    if(y == null) {
      return null
    } else {
      return x + y
    }
  }
}
```

This can be written using `>>=` but not `liftA2`.

```haskell
x >>= \a ->
test x >>= \b ->
pure (a + b)
```

----

### liftA2 on reader

Given two functions `p` and `q`, apply them to the same value `x`, then apply those two results to another function such as `+`.

```csharp
given(x) {
  return p(x) + q(x)
}
```

Since `liftA2` is overloaded to work on different structures as long as they all satisfy a similar pattern. We have already seen that lists and `Maybe` satisfy this pattern. However, functions that accept a single argument also satisfy this pattern. A function that accepts a single argument can also be used to simulate a function that accepts two arguments, by first accepting that one argument, and returning a function that then accepts one argument and takes it to a value. Since in this case we are describing (the simulation of) a *`two-argument function`* (called `+`), then we specifically want the `2` in `liftA2`. In common nomenclature, this specific instance is often called the *reader* instance.

```haskell
liftA2 (+) p q
```

----

### sequence on Maybe with list

Given a list of possibly `null` values, return either a list of definitely-not `null` values, or `null` itself.

```csharp
// >>> given([notnull 1,null,notnull 3])
// null
// >>> given([notnull 1,notnull 2,notnull 3])
// notnull [1,2,3]
given(list) {
  var r = List.empty
  for(int i = 0; i < list.length; i++) {
    if(list[i] == null) 
      return null
    else 
      r.add(list[i])
  }
  return r
}
```

This can be written using the `sequence` function, which is overloaded on many values, including `Maybe`. Since `Maybe` values have either 0 values or 1 value inside, it can be used as a type-safe `null` value. Therefore the `sequence` function will accept a list of `Maybe` values, and return a `Maybe` list of values.

```haskell
sequence list

sequence [Just 1,Nothing,Just 3] == Nothing
sequence [Just 1,Just 2,Just 3] == Just [1,2,3]
```

----

### sequence on reader with list

Given a list of functions (or *instances of an interface*), and a value (`x`), collect the results of having applied each function to that value.

```csharp
// >>> given([(+1),(*2),(/3)], 99)
// [100,198,33]
given(list, x) {
  var r = List.empty
  for(int i = 0; i < list.length; i++) {
    r.add(list[i].apply(x))
  }
  return r
}
```

Since the `sequence` function is overloaded, as we have also seen it work on `Maybe` values and, it can also work on values that are functions accepting one argument, since that meets the same pattern as `Maybe` as far as `sequence` is concerned.

```haskell
sequence list x

sequence [(+1),(*2),(/3)] 99 == [100,198,33]
```

----

### sequence on list with list

Given a list of lists, produce a list of lists, where each element of each input list appears (in order), with each element of the other lists. This is also known as *the cartesian product*.

```csharp
// >>> given([[1,2],[3,4,5],[6,7]])
// [[1,3,6],[1,3,7],[1,4,6],[1,4,7],[1,5,6],[1,5,7],[2,3,6],[2,3,7],[2,4,6],[2,4,7],[2,5,6],[2,5,7]]
given(list) {
  var r = List.empty;
  for(int i = 0; i < list.length; i++) {
    var s = List.empty
    for(int j = 0; j < list[i].length; j++) {
      s.add(list[i][j])
    }
    r.append(s)
  }
  return r
}
```

The `sequence` function is also overloaded on `[]` values, not just `Maybe` and functions that accept an argument. Actually, it's overloaded on many other things. These things are called *applicative functors* and `sequence` works on all of them. 


```haskell
sequence list

sequence [[1,2], [3,4]] == [[1,3],[1,4],[2,3],[2,4]]
```

----

### join on reader

Given a function that accepts two (or more) arguments such as `+`, apply that function to the same argument.

```csharp
given(x) {
  return x + x
}
```

The `join` function is overloaded to work on many values, including functions that take one argument. With one call to `join`, that argument will be passed to a function, then when another function is returned, that same value is passed in again, to return a value. To continue passing this argument, the `join` function can be *composed* with itself as many times as necessary.

```haskell
join (+) x
```

----

### kleisli composition on Maybe

Given a value (`x`), one function that might return `null` (`func1`), another function that might return `null` (`func2`), and a third function that might return `null` (`func3`), pass `x` to `func3` and only if it does not return `null`, pass that return result into `func2` and if that is not `null`, pass it to `func1`. If any of the functions return `null` then return `null`.

```csharp
given(x) {
  func1result = func1(x)
  if(func1result == null) {
    return null
  } else {
    func2result = func2(func1result)
    if(func2result == null) {
      return null
    } else {
      func3(func2result)
    }
  }
}
```

The kleisli composition operator, called `<=<` is overloaded to work on many values, of which `Maybe` is one instance.

```haskell
(func3 <=< func2 <=< func1) x
```

----

### kleisli composition on reader

Given a value (`x`), a function (`func1`) that requires access to a variable (`s`) to compute its result, another function (`func2`) that also requires access to `s`, pass `x` to `func2` and the return result of that to `func1`.

```csharp
global const var s // accessible by func1 and func2

given(x, func1, func2) {
  return func1(func2(x))
}
```

We can use the *reader* with kleisli composition.

```haskell
(func1 <=< func2) x s
```

----

### kleisli composition on State

The `State` data structure is structurally equal to a function that passes a value in, potentially modifies it to a new value, and returns another, orthogonal value in association with that state changing. It can be used to to simulate updating variables, while maintaining the principles of Functional Programming.

Given a value (`x`), a function (`func1`) that requires access to, and may modify, a variable (`s`) to compute its result, another function (`func2`) that also requires access/modify to `s`, pass `x` to `func2` and the return result of that to `func1`.

```csharp
global var s // accessible/writable by func1 and func2

given(x, func1, func2) {
  return func1(func2(x))
}
```

We can use the `State` data structure to simulate the variable `s`.

```haskell
evalState ((func1 <=< func2) x) s
```

----

### sequence on reader with Maybe

Given a function (or *instance of an interface*), called `func`, which may be `null`, and a value (`x`), apply the function to the value. If the function is `null`, return `null`.

```csharp
given(func, x) {
  if(func == null) {
    return null
  } else {
    return func.apply(x)
  }
}
```

The possibility for `null` can be simulated using the `Maybe` data type. Since `Maybe` can be treated as a list with a maximum length of one, we can also *sequence* this, and we can do it with many values that meet a certain pattern. A function that accepts one argument meets this pattern, so we can use `sequence` for this case.

```haskell
sequence func x
```

----

### choice on Maybe

Given two values (`x` and `y`), then if `x` is not `null`, return it, otherwise try `y`.

```csharp
given(x, y) {
  if(x == null) {
    return y
  } else {
    return x
  }
}
```

In some programming languages, this expression might be written using a ternary operator:

```csharp
given(x, y) {
  return x == null ? y : x
}
```

The C# programming language has a built-in operator (`??`) for this, called the elvis operator.

```csharp
given(x, y) {
  return x ?? y
}
```

The choice operator in Haskell is called `<|>` and is overloaded to work on many different values, of which `Maybe` is one instance.

```haskell
x <|> y
```

----

### Do effect on condition

Given some boolean condition, if it is true, do something. If it is false, do nothing.

```csharp
given(condition) {
  if(condition)
    doSomething()
}
```

Since Haskell values are always pure, including effects, they can be treated as values. That is, they can be passed to functions as arguments.

```haskell
when condition doSomething
```

### Do effect on not-condition

Given some boolean condition, if it is false, do something. If it is true, do nothing.

```csharp
given(condition) {
  if(!condition)
    doSomething()
}
```

The `when` and `unless` functions can handled performing some effect based on a condition. Since the condition is negated, we want the `unless` function.

```haskell
unless condition doSomething
```

----

Can you come up with any more patterns that translate in a helpful way? [Let us know](https://www.reddit.com/r/haskell/comments/6sz3td/fp_cheat_sheet/) if you do.
