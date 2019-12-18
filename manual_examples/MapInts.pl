:- module(_,_,[functional, hiord]).
:- use_module(library(lists)).

plus(X,Y) := X+Y.

mapInts(Ds_d11n, []) := [].
mapInts(Ds_d11n, .(X,Xs)) := .(~Ds_d11n(X), ~mapInts(Ds_d11n, Xs)).
