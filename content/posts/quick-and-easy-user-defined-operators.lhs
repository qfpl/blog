---
title: Quick and easy user-defined operators with Plated
date: 2017-11-17
authors: ielliott
---

All [infix](https://en.wikipedia.org/wiki/Infix_notation) operators have
[precedence](https://en.wikipedia.org/wiki/Order_of_operations)
and
[associativity](https://en.wikipedia.org/wiki/Operator_associativity).
A language that supports user-defined operators should also give the
user a way to control these attributes for their custom operators.
Modern languages do this in a variety of ways. In Swift, all operators
are associated with a
[precedence group](https://developer.apple.com/documentation/swift/operator_declarations).
User-defined operators in F# get their precedence and associativity from the
[combination of characters](https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/operator-overloading)
that make up the operator. In Haskell-like languages, the user explicitly states the
precedence and associativity using special syntax. For example,
`infixl 5 +` says "the `+` operator is left-associative and has
precedence level 5".

I like Haskell-style operators out of all these options &mdash; I think
they're a more elegant solution to the problem. However, elegance
comes at the cost of implementation difficulty.

Haskell-style infix operators are generally implemented like this:

* Define a set of characters that are allowed in operators.
* Add syntax for declaring operator precedence and associativity.
* Parse all operators right-recursively.

    The grammar might look something like this:
    ```
    operator_char ::=
      '~' | '!' | '@' | '#' | '$' | '%' | '^' | '&' | '*' |
      '?' | '>' | '<' | '.' | '|' | '-' | '+' | '='
    operator ::= operator_char+
    
    fixity ::= 'infixr' | 'infixl'
    infix_decl ::= fixity [0-9]+ operator
    declaration ::= infix_decl | ...
    
    non_operator_expr ::= '(' expr ')' | ...
    expr ::= non_operator_expr (operator non_operator_expr)*
    ```

* Collect the operator precedences and associativities from the parsed
   output
* Re-write the parsed expressions according to this information

    This is done in two parts:

    * Associativity correction
    * Precedence correction

Re-association
--------------

Consider the input `2 - 3 + 4`. We implicitly read this as `[2 - 3] + 4`,
because `-` and `+` have the same precedence and are left-associative.
According to our grammar this expression will always be parsed as
`2 - [3 + 4]`, which has a completely different meaning. Changing the
position of the brackets accomplished by changing which operator is at
the top of the tree. `(-)` is at the top of the tree in the expression
`2 - [3 + 4]`, but `(+)` is at the top in `[2 - 3] + 4`. Notice that the
order of the leaves &mdash; `2, 3, 4` &mdash; stays the same regardless
of how the tree is transformed.

The re-association algorithm for a tree of height two looks like this:

```
Left associative operators:

  OP1(prec=X, assoc=Left)
  /             \
 /               \
A      OP2(prec=Y, assoc=Left)
       /                 \
      /                   \
     B                     C


if X == Y, becomes


            OP2(prec=Y, assoc=Left)
            /                \
           /                  \
  OP1(prec=X, assoc=Left)      C
  /                 \
 /                   \
A                     B
```

```
Right associative operators

            OP1(prec=X, assoc=Right)
            /                \
           /                  \
  OP2(prec=Y, assoc=Right)     C
  /                 \
 /                   \
A                     B


if X == Y, becomes


  OP2(prec=Y, assoc=Right)
  /             \
 /               \
A      OP1(prec=X, assoc=Right)
       /                 \
      /                   \
     B                     C
```

If a parent and child node have different precedences or associativities,
then they do not need to be re-associated.

Precedence Correction
---------------------

Consider the input `2 * 3 + 4`. This is read as `[2 * 3] + 4`, because
`*` has higher precedence than `+`, but it will be parsed as `2 * [3 + 4]`.

The precedence-correction algorithm looks like this:

```
  OP1(prec=X, assoc=?)
  /             \
 /               \
A      OP2(prec=Y, assoc=??)
       /                 \
      /                   \
     B                     C


if X > Y, becomes


            OP2(prec=Y, assoc=??)
            /                \
           /                  \
  OP1(prec=X, assoc=?)         C
  /                 \
 /                   \
A                     B


and



            OP1(prec=X, assoc=?)
            /                \
           /                  \
  OP2(prec=Y, assoc=??)        C
  /                 \
 /                   \
A                     B


if X > Y, becomes


  OP2(prec=Y, assoc=??)
  /             \
 /               \
A      OP1(prec=X, assoc=?)
       /                 \
      /                   \
     B                     C
```

Putting it together
-------------------

This seems fairly straightforward when thinking about trees of height 2,
but how do you generalise it to trees of any height?

This is where `Plated` comes in. To make your datatype an instance of
[Plated](https://hackage.haskell.org/package/lens/docs/Control-Lens-Plated.html#t:Plated),
you write a
[Traversal](https://hackage.haskell.org/package/lens/docs/Control-Lens-Type.html#t:Traversal)
that operates over its immediate self-similar children.
[rewrite](https://hackage.haskell.org/package/lens/docs/Control-Lens-Plated.html#v:rewrite)
then allows you to write transformations on trees as deep or as shallow
as you wish, and will recursively apply those transformations from the
bottom of a tree upwards until it can no longer be transformed.

Good abstractions reduce boilerplate and help you focus on what's
important. The "recursive bottom-up tree rewriting" algorithm has already
been written for us. Using `Plated`, we need only consider the
simplest case for re-ordering the tree, and then it scales for free.

<br>

Let's write some code.

\begin{code}
module Operators where

import Control.Applicative ((<|>), liftA2)
import Control.Lens.Plated (Plated(..), rewrite)
import Data.Maybe (fromMaybe)
\end{code}

<br>

Our syntax tree will consist of binary operators and numbers. The `Parens`
node is for explicit parenthesisation.
\begin{code}
data Expr
  = Parens Expr
  | BinOp String Expr Expr
  | Number Int
  deriving (Eq, Ord, Show)
\end{code}

<br>

The wonderful `Plated` instance.
\begin{code}
instance Plated Expr where
  plate f (Parens a) = Parens <$> f a
  plate f (BinOp n a b) = BinOp n <$> f a <*> f b
  plate _ a = pure a
\end{code}

<br>

Operators have an associativity and a precedence.
\begin{code}
data Associativity
  = AssocL
  | AssocR
  deriving (Eq, Ord, Show)

data OperatorInfo
  = OperatorInfo
  { opAssoc :: Associativity
  , opPrecedence :: Int
  }
\end{code}

<br>

An `OpTable` is a map from operator names to their `OperatorInfo`.
\begin{code}
type OpTable = [(String, OperatorInfo)]
\end{code}

<br>

Re-association. This code crashes when an operator is missing from the
table.

* If the input node is an operator, and:

    * It is left-associative:
        1. Inspect its right child
        2. If that node is also left-associative, and has equal
           precedence to the input node:
             * Re-order the tree to be left-associative
 
    * It is right-associative
        1. Inspect its left child
        2. If that node is also right-associative, and has equal
           precedence to the input node:
             * Re-order the tree to be right-associative
* Otherwise, do nothing

\begin{code}
associativity :: OpTable -> Expr -> Maybe Expr
associativity table (BinOp name l r)
  | Just entry <- lookup name table =
      case opAssoc entry of
        AssocL
          | BinOp name' l' r' <- r
          , Just entry' <- lookup name' table
          , AssocL <- opAssoc entry'
          , opPrecedence entry == opPrecedence entry' ->
              Just $ BinOp name' (BinOp name l l') r'
        AssocR
          | BinOp name' l' r' <- l
          , Just entry' <- lookup name' table
          , AssocR <- opAssoc entry'
          , opPrecedence entry == opPrecedence entry' ->
              Just $ BinOp name' l' (BinOp name r' r)
        _ -> Nothing
associativity _ _ = Nothing
\end{code}

<br>

Precedence correction. This code also crashes when operators are missing
from the operator table.

This is broken down into two phases- making sure the left branch is
precedence-correct with respect to the input node, and then doing the same
for the left branch.

For each branch, if that branch contains an operator with a lower
precedence than the input node, re-order the tree so the lower-precedence
operator is at the top.
\begin{code}
precedence :: OpTable -> Expr -> Maybe Expr
precedence table e@(BinOp name _ _)
  | Just entry <- lookup name table =
      checkR entry $ fromMaybe e (checkL entry e)
  where
    checkL entry (BinOp name l c) =
      case l of
        BinOp name' a b 
          | Just entry' <- lookup name' table
          , opPrecedence entry' < opPrecedence entry ->
              Just $ BinOp name' a (BinOp name b c)
        _ -> Nothing
    checkL _ _ = Nothing

    checkR entry (BinOp name a r) =
      case r of
        BinOp name' b c
          | Just entry' <- lookup name' table
          , opPrecedence entry' < opPrecedence entry ->
              Just $ BinOp name' (BinOp name a b) c
        _ -> Nothing
    checkR _ _ = Nothing
precedence _ _ = Nothing
\end{code}

<br>

`precedence` and `associativity` have type `Expr -> Maybe Expr` because
eventually the transformations will no longer be applicable. We can use
`liftA2 (<|>)` to combine the two rewrite rules, and `rewrite` will run
until both always produce `Nothing`
\begin{code}
reorder :: OpTable -> Expr -> Expr
reorder table =
  rewrite (liftA2 (<|>) (precedence table) (associativity table))
\end{code}

<br>

Let's try it on the expression `5 - 4 + 3 * 2 + 1`. It will be parsed as
`5 - [4 + [3 * [2 + 1]]]`, but after re-ordering should become
`[[5 - 4] + [3 * 2]] + 1`.
```
ghci> let o = [("+", Operator AssocL 5), ("-", Operator AssocL 5), ("*", Operator AssocL 6)]
ghci> let input = BinOp "-" (Number 5) (BinOp "+" (Number 4) (BinOp "*" (Number 3) (BinOp "+" (Number 2) (Number 1))))
ghci> reorder o input
BinOp "+" (BinOp "+" (BinOp "-" (Number 5) (Number 4)) (BinOp "*" (Number 3) (Number 2))) (Number 1)
```

We can also use `Parens` to explicitly parenthesise the expression. If we
input `5 - (4 + (3 * (2 + 1)))`, it will not be re-ordered at all.
```
ghci> let input = BinOp "-" (Number 5) (Parens $ BinOp "+" (Number 4) (Parens $ BinOp "*" (Number 3) (Parens $ BinOp "+" (Number 2) (Number 1))))
ghci> reorder o input
BinOp "-" (Number 5) (Parens (BinOp "+" (Number 4) (Parens (BinOp "*" (Number 3) (Parens (BinOp "+" (Number 2) (Number 1)))))))
ghci> reorder o input == input
True
```

Exercise
--------

If you're interested in playing around with this a bit more, here's an
exercise for you. Earlier I said that `precedence` and `associativity`
will crash if they encounter operators that are missing from the operator
table.

*Extend `precedence`, `associativity` and `reorder` to gracefully handle
undefined operators by returning an `Either`.*

Hint: You might want to use a different combinator from `Control.Lens.Plated`
