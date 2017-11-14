:- module(blatt5).

% Aufgabe 3 (Binärbäume)
inorder(T) :- inorder(T,L-[]) , print(L) , !.
inorder(nil,L-L) :- !.
inorder(t(V,LT,RT),L-T) :- inorder(LT,L-[V|T1]), inorder(RT,T1-T).


postorder(T) :- postorder(T,L-[]) , print(L) , !.
postorder(nil,L-L) :- !.
postorder(t(V,LT,RT),L-T) :- postorder(LT,L-T1), postorder(RT,T1-[V|T]).


preorder(T) :- preorder(T,L-[]) , print(L), !.
preorder(nil,L-L) :- !.
preorder(t(V,LT,RT),[V|T]-T1) :- preorder(LT,T-T2), preorder(RT,T2-T1).


% Aufgabe 4 (Sieb des Eratosthenes)
primes(UpTo,ListOfPrimes) :- range_1(UpTo,[1|LoPP]) , removeMultiplesFromList(LoPP,ListOfPrimes) , !.

removeMultiplesFromList([],[]) :- !.
removeMultiplesFromList([H|T],[H|T2]) :- removeMultiples(H,T,T1), removeMultiplesFromList(T1,T2).

removeMultiples(_,[],[]) :- !.
removeMultiples(E,[H|T],L) :- 0 is H mod E, removeMultiples(E,T,L) , !.
removeMultiples(E,[H|T],[H|T1]) :- removeMultiples(E,T,T1).


% s. blatt3.pl
range_1(0,[]) :- !.
range_1(1,[]) :- !.
range_1(N,[1|L]) :- range_2(N,[1|L]) , !.
range_2(N,[A,B|T1]) :- B is A+1 , N > B , range_2(N,[B|T1]) , !.
range_2(_,[_]). 