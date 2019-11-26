:- module(_,_,[functional, hiord]).
:- use_module('~/hs-to-ciao/lib/ciao_prelude.pl').

:- meta_predicate sum(?,?,?).
sum(X,Y) := +(X,Y).

:- meta_predicate greater(?,?,?).
greater(X,Y) := ~bool(X>Y).

:- meta_predicate filterAndFoldInts(pred(2),pred(3),?,?,?).
filterAndFoldInts(FILT, F, BASE, LIST) := ~compose(foldl(F, BASE), filter(FILT), LIST).