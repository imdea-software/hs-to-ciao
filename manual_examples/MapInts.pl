:- module(_,_,[functional, hiord]).
:- use_module(library(lists)).

plus(X,Y) := X+Y.

mapInts(DS_D11N, []) := [].
mapInts(DS_D11N, .(X,XS)) := .(~DS_D11N(X), ~mapInts(DS_D11N, XS)).