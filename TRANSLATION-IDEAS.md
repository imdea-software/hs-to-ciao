**All of these guidelines are current ideas and are pretty much subject to change depending on both this project's progress
and Ciao's development. Suggestions are, of course, welcome!**

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

By default, all Haskell functions represented in GHC's Core via `CoreSyn` are curried. We probably *could* bring them into
Ciao directly that way, but for easier readability and (maybe) ease of analysis, I'm going to try and inspect the 
type signature of the functions represented in Core and translate them as uncurried as possible, 
but not further. For example, the previous `filterAndFoldInts` example could be taken one step further and
instead of taking three arguments and returning a lambda that takes one argument, make it so that it
directly takes in four arguments, and returns a value. That would most likely work fine even for partial application
in Ciao, but I think it's better if we don't do that.
