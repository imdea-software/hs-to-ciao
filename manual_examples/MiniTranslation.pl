plus3(X, Y) :- Y is X+3.

mapInts(_,[],[]).
mapInts(F,[X|Xs],[Y|Ys]) :-
	Goal =.. [F,X,Y],
	call(Goal),
	Goal2 =.. [mapInts,F,Xs,Ys],
	call(Goal2).