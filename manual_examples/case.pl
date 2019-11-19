:- module(_,_,[functional, hiord]).
:- use_module('~/hs-to-ciao/lib/ciao_prelude.pl').

% case in Ciao via function composition

foo(X) := X+3.

bar(X) := X=4 ? hey1
        | X=5 ? hey2
        | X=6 ? hey3.

foobar(X) := ~compose(bar, foo, X).
