:- module(_,_,[assertions, regtypes, functional, hiord]).
:- use_module('~/hs-to-ciao/lib/ciao_prelude.pl').

:- regtype myType/1.
myType(white).
myType(red).
myType(black).

% Implementation of the Eq typeclass for myType;
% the first argument is the type parameter
equalPoly(myType, white, white) := true.
equalPoly(myType, red, red) := true.
equalPoly(myType, black, black) := true.
equalPoly(myType, _, _) := false.

% Implementation of the Ord typeclass for myType
lessEqualPoly(myType, white, red) := true.
lessEqualPoly(myType, white, black) := true.
lessEqualPoly(myType, red, black) := true.
lessEqualPoly(myType, X, Y) := ~equalPoly(myType, X, Y).

:- entry maxPoly/4 : atm * myType * myType * var.
maxPoly(myType, X, Y) := (~lessEqualPoly(myType, Y, X)=true ? X
                         | ~lessEqualPoly(myType, Y, X)=false ? Y).

:- entry max_/3 : myType * myType * var.
max_(X, Y) := ~maxPoly(myType, X, Y).