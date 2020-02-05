:- module(_,_,[functional, hiord]).
:- use_module('~/hs-to-ciao/lib/ciao_prelude.pl').

:- meta_predicate foldl(pred(3),?,?,?).
foldl(Ds_d1gk, B, []) := B.
foldl(Ds_d1gk, B, [X | Xs]) := ~Ds_d1gk(X, ~foldl(Ds_d1gk, B, Xs)).

:- meta_predicate compose(pred(2),pred(2),?,?).
compose(F, G, X) := ~F(~G(X)).

:- meta_predicate filter(pred(2),?,?).
filter(Ds_d1g8, []) := [].
filter(Ds_d1g8, [X | Xs]) := (~Ds_d1g8(X)=true ? [X | ~filter(Ds_d1g8, Xs)]
| ~Ds_d1g8(X)=false ? ~filter(Ds_d1g8, Xs)).

:- meta_predicate filterAndFoldInts(pred(2),pred(3),?,?,?).
filterAndFoldInts(Filt, F, Base, X) := ~compose(foldl(F, Base), filter(Filt), X).


