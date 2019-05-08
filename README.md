# hs-to-ciao
Translate Haskell code to CiaoPP for code analysis, implemented as a [ghc-plugin](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/extending_ghc.html#compiler-plugins).

# Install

- using cabal: Run `cabal install` inside the `hs-to-ciao` directory.
- using stack: Run `stack install` inside the `hs-to-ciao` directory.

Both generate the HsToCiaoPP compiler plugin.

# Run

Add the `-fplugin=HsToCiaoPP` to ghc. E.g., to translate `tests/Merge.hs` run 


```
# if installed with cabal 
ghc -fplugin=HsToCiaoPP tests/Repeat.hs


# if installed with stack 
stack exec -- ghc -fplugin=HsToCiaoPP tests/Repeat.hs
```

The cioa translation of the Haskell file will be written in `out/merhe.pl`.



