**All of these guidelines are current ideas and are pretty much subject to change depending on both this project's progress
and Ciao's development. Suggestions are, of course, welcome!**

## Prelude for Ciao

In the [Dev](https://github.com/imdea-software/hs-to-ciao/tree/dev) branch, 
under the [hs-to-ciao/lib](https://github.com/imdea-software/hs-to-ciao/tree/dev/lib) folder, is a file called 
[ciao_prelude.pl](https://github.com/imdea-software/hs-to-ciao/tree/dev/lib/ciao_prelude.pl) 
which holds some standard predicate/function
definitions that are used throughout the examples on which I'm working. Eventually, I think it should be
great to translate the whole or most of the Haskell Prelude, since they are functions used in most programs
and in order to make any kind of analysis over a minimally useful Haskell program, we're going to need
those functions defined in Ciao. This is a starting point for that.

The translation of the Prelude could happen either manually, and translate the equivalent
of every function to Ciao by hand, or automatically and then supervise and correct the output
of the translation for the functions. I think we should take the second approach once the translation
is at a sufficiently advanced state, and translate manually some of the functions as needed for
testing and checking the development of the translation program/algorithm itself.

## Lambdas

We have the following function:
```haskell
filterAndFoldInts :: (Int -> Bool) -> (Int -> Int -> Int) -> Int -> [Int] -> Int
filterAndFoldInts filt f base = (foldl f base) . (filter filt)
```
which filters the elements of a list with the predicate `filt`, 
and then performs a `foldl` over said filtered list using the operation `f` with base case `base`.

Providing `filterAndFoldInts` with `filt`, `f`, and `base` yields not a value, but a function that takes one parameter
(the list) and returns the resulting value. That is, a lambda:

```haskell
(\x -> ( (foldl f base) . (filter filt) ) x) :: [Int] -> Int
```

For doing that in Ciao, I plan on using the upcoming syntax for Ciao's lambda abstractions:
```prolog
filterAndFoldInts(Filt, F, Base) = {''(X) := ~compose(~foldl(F, Base), ~filter(Filt), X)}
```
(assuming that `compose`, `foldl` and `filter` are already existing predicates)

## Currying

By default, all Haskell functions represented in GHC's Core via [`CoreSyn`](https://downloads.haskell.org/ghc/latest/docs/html/libraries/ghc-8.8.1/CoreSyn.html) are curried. We probably *could* bring them into
Ciao directly that way, but for easier readability and (maybe) ease of analysis, I'm going to try and inspect the 
type signature of the functions represented in Core and translate them as uncurried as possible, 
but not further. For example, the previous `filterAndFoldInts` example could be taken one step further and
instead of taking three arguments and returning a lambda that takes one argument, make it so that it
directly takes in four arguments, and returns a value. That would most likely work fine even for partial application
in Ciao, but I think it's better if we don't do that.

## Built-in types

For now, the examples I will be using for the early stages of the translation pretty much only involve
the types `Int`, `Bool`, and lists.

### Int

`Int`s are going to be naïvely translated to the same number they hold in Haskell,
which *could* or *could not* break Ciao programs for sufficiently large positive or negative `Int`s.

### Lists

Lists are going to be constructed using the built-in functor `./2` which holds correspondence with
Haskell's list constructor, `(:)`, so there shouldn't be any problems.

### Bool

Since Ciao doesn't have built-in support for them 
(predicates only succeed or fail, but don't return `true` or `false`),
and as a result of an email discussion with Manuel H. and José,
I'm going to use the following Ciao predicate:
```prolog
:- meta_predicate bool(goal,?).
bool(X,T) :- (X -> T=true ; T=false).
```
So for every function that in Haskell would return a value of type `Bool`, in Ciao we're going to tweak it into the following fashion:
Instead of having the predicate `greater/2`:
```prolog
greater(X,Y) := X>Y.
```
which succeeds if X is a number strictly greater than Y and fails otherwise, we're going to have the predicate `greater/3` in functional syntax:
```prolog
greater(X,Y) := ~bool(X>Y).
```
which in the parameter hidden by the functional syntax (the result parameter) stores the atom `true` if the goal inside 
the `~bool(...)` call succeeds, or the atom `false` if the goal fails.
