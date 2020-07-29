<p align="center">
  <img width="500" height="200" src="hs-to-ciao-logo.svg">
</p>

[![Build Status](https://travis-ci.com/imdea-software/hs-to-ciao.svg?branch=master)](https://travis-ci.com/imdea-software/hs-to-ciao)

# hs-to-ciao
Translate [Haskell](https://www.haskell.org/) code to [Ciao](http://ciao-lang.org/) for automatic resource analysis, implemented as a [ghc-plugin](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/extending_ghc.html#compiler-plugins).

# Installation
The easiest way to install the plugin is by using [Stack](https://docs.haskellstack.org/en/stable/README/).
In most Unix-like systems (e.g. macOS and Linux based), running the following command should be enough:

```
curl -sSL https://get.haskellstack.org/ | sh
```

After installing it, clone this repository somewhere, and simply run `stack build` inside the repository folder.

The plugin is tested for GHC versions `8.6.5`, `8.8.1`, `8.8.2`, `8.8.3` and `8.10.1`. Older or newer versions _might not_ work.
Also, an installation through [Cabal](https://www.haskell.org/cabal/) hasn't been actively tested.

If you just want to translate Haskell into Ciao, having installed the plugin is enough.
However, if you intend on using the resource analysis capabilities,
you will need a working Ciao installation, 
as well as the `ciaopp` and `ciaopp_extra` bundles. 
Since these bundles are provided _on demand_,
you should ask the [CLIP Lab](https://cliplab.org/Software/index.html) staff for access and support,
since they are the ones responsible for Ciao's development.

# Usage

Add the `-fplugin=HsToCiaoPP` option when compiling a Haskell source from GHC.
For example, let's say you want to translate `examples/ListReverse.hs`. Then, run:


```
stack exec -- ghc -fplugin=HsToCiaoPP examples/ListReverse.hs
```

You will be prompted to select between several kinds of resource analysis (right now, only Big-O analysis is supported).
After choosing one, both the Ciao translation of the original Haskell source and the results of the analysis
will be written in the `out/` folder (in this example, `out/listreverse.pl` and `out/listreverse_big-o.txt`).

# Detailed explanation of the plugin

The details of the project are explained thoroughly in [my](https://davidmazarro.com/) bachelor's thesis, which you can read [here](http://oa.upm.es/63139/1/TFG_DAVID_MUNUERA_MAZARRO.pdf).



