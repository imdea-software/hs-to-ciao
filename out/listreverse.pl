:- module(_,_,[assertions, regtypes, functional, hiord]).
:- use_module('~/hs-to-ciao/lib/ciao_prelude.pl').

:- meta_predicate reverse_(?,?).
reverse_([]) := [].
reverse_([X | Xs]) := ~append(~reverse_(Xs), [X]).


