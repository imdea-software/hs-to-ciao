IN THE FUTURE:

- [ ] Figure out what kinds of analysis we can perform over our target manual examples

16/1
  - [ ] keep implementing the translate function
    - [ ] add local variable tracking for function body id translation; if it's a local id, translate it into uppercase, otherwise lowercase
    - [x] figure out about defining data types in Ciao
    - [ ] try to fix the other two manual examples 
    - [x] make functions returning booleans in Haskell a special case

19/12

- TODO
  - [ ] keep implementing the translate function
    - [ ] add local variable tracking for function body id translation; if it's a local id, translate it into uppercase, otherwise lowercase
    - [ ] figure out about defining data types in Ciao
    - [x] translate the application of data constructors without the ~ (https://downloads.haskell.org/ghc/latest/docs/html/libraries/ghc-8.8.1/Id.html#v:isDataConWorkId)
    - [ ] try to fix the other two manual examples 
    - [ ] make functions returning booleans in Haskell a special case

12/12

- TODO
  - [ ] keep implementing the translate function
    - [x] make it so that the foldl Core translates into the example term I constructed
      - [x] fix compilation errors in case-splitting function, and try it
      - [x] write the translate Ciao function body function
    - (it translates, but the ambiguity between functor/variable still has to be solved for the Ciao code to work properly)
    - [ ] make functions returning booleans in Haskell a special case

5/12

- TODO
  - [x] write the final report in relation to the internship for my uni
  - [ ] keep implementing the translate function
    - [ ] make it so that the foldl Core translates into the example term I constructed
      - [x] keep writing the case-splitting function
    - [ ] make functions returning booleans in Haskell a special case


28/11
- TODO
  - [ ] keep implementing the translate function
    - [ ] make it so that the foldl Core translates into the example term I constructed
      - [x] use the arity function to print out the metapredicate clause in Ciao
      - [ ] in the first case, return as result base
      - [ ] also translate the second case correctly
    - [ ] make booleans a special case
  - [x] check out if the manual examples work with CiaoPP analysis
    - according to the Ciao people, there should't be any problems

21/11

- TODO
  - [ ] keep implementing the translate function
    - [ ] make it so that the foldl Core translates into the example term I constructed
    - [x] encode the other two manual examples within CiaoSyn
    - [ ] make booleans a special case
  - [x] ask/figure if the empty list is a functor in Ciao
    - it is :)

7/11

- TODO
  - [ ] keep implementing the translate function
    - [ ] make booleans a special case
    - [x] take a look at the point-free example (lambdas in Ciao? ~~or add the needed argument?~~)
      - I've been discussing lambdas with José from the Ciao team, and they're going to make a few changes/additions;
      so I think I'll try to use lambdas and the type signatura inspection to find out how many arguments are missing.
      This could all be subject to change though.
      - [x] how is a function's type signature encoded within CiaoSyn?
  - [x] define the CiaoPred syntax so it covers the manual examples
  - [x] created document [TRANSLATION-IDEAS.md](https://github.com/imdea-software/hs-to-ciao/blob/master/TRANSLATION-IDEAS.md) at the topmost folder of the project going over the assumptions/decisions
  that are being taken for the translation process; this way both the Ciao team and Niki can tell what's going on and see if   everything is in check

31/10

- TODO
  - [x] start reading Graham's
  - [x] try to start the implementation
  - [x] more concrete pseudocode
    - point-free style will not be as immediate to translate as I thought; I need to ask you a few things
    
24/10

- TODO
  - [x] keep reading LYAH/Graham
  - [x] migrate previous manual translations to Ciao's functional syntax
  - [x] discussed with the Ciao team, and refined the manual translation model; we should be getting better, more accurate and direct translations. We now have non ad-hoc currying, and I think we're closer to supporting types
  - [x] check if the manual translation for map-reduce is correct
    - [x] fix my Ciao/CiaoPP installation
  - [ ] more concrete pseudocode
    - [x] get cleaner print bindings (mess with the current code)
      - ↑ I didn't have to change anything in the end, and I think it's clean enough

10/10

- TODO
  - [x] keep reading LYAH
  - [x] write the report for my university
  - [x] check out the Programming with Higher-Order Logic book
  - [ ] try to install CiaoPP
    - [x] try to read CiaoPP manual
      - ↑ met with Maximiliano last week and he explained the basics of code analysis
  - [ ] manual translation for map-reduce
  - [ ] print bindings in a more explicit manner (mess with the current code)

3/10

- TODO
  - [x] one more example of manual translation
    - [x] start translation pseudocode that works (at least) for these two examples
  - [x] keep reading LYAH
  - [x] read the paper on higher order programming Prolog
  - [ ] try to read CiaoPP manual

26/9

- TODO
  - [ ] schedule a meeting with the Ciao team for seeing what kind of resource analysis we want (and so they teach me how to use CiaoPP)
    - I didn't get a reply :(
  - [x] implement function to output Core bindings into a file
  - [x] keep reading LYAH
  - [ ] one more example of manual translation
    - ↑ had a bit of trouble with this one
    - [ ] start translation pseudocode that works (at least) for these two examples
  - [x] update diagram with a user (which will be asking the analysis queries) and -fplugin=HsToCiaoPP 
  
19/9

- TODO
  - [x] keep reading LYAH (λ more applicatives, ~~monoids and monads!~~ λ)
      - applicatives turned out to be harder than expected to grasp in certain ways
  - [x] vector graphic for the "bigger picture"
  - [x] read about Core GHC
      - take a look at specialisations and try to understand how Core desugars
        typeclases
  - [x] plan for the example, manual translation
      - the plan is: code basic plain Haskell example (list reverse or similar),
        generate Core code, see how to translate into Prolog
  
12/9

- TODO
  - [X] keep reading LYAH
  - read about Core GHC 
    - [X] (actually watched Simon Peyton Jones' talks)
  - [ ] come up with an example and manual translation
  - [X] sketch the bigger picture of where we're trying to get (resource analysis of Haskell source)

5/9

- TODO
  - [x] create a .pdf with the goals and challenges of with work 
  - [x] start book readings 
  - [ ] fix the prolog grammar (on paper and on the implementation)
  - [x] started reading a bit about Core

- DONE 
  - [x] summer  
