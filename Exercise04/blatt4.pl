:- module(blatt4).

% Aufgabe 1 (Intuition)
example0(L) :- s([a(q,1),a(i,4),a(i,5),a(f,4)],a(i,7),L).
example1(L) :- t([1,2,3,4],L).

s([],_,[]).
s([a(H,_)|T] , a(H,X) , [a(H,X)|T]). % Ersetze das zweite Element im Kopf der Liste mit X aus dem zweiten a-Tupel
s([a(H,V)|T] , a(I,L) , [a(H,V)|R]) :-	H \= I ,
										s(T, a(I,L) ,R). % Solange das erste Element vom a-Tupel noch nicht übereinstimmt, suche weiter in der Liste.


t(L,NL) :- t(L,[],NL). 				% Rufe neues Prädikat mit 3 Parametern auf
t([],L,L).
t([H|T],A,NL) :- t(T,[H|A],NL).		% Hänge den Kopf ans Ende der List (Effektiv wird die Reihenfolge der Elemente umgekehrt)


% Aufgabe 2 (Autamaton)
start(1).
transition(1,d,2).
transition(2,a,2).
transition(2,b,2).
transition(2,d,4).
transition(2,c,3).
transition(2,e,5).
transition(3,d,6).
transition(6,c,5).
fin(4).
fin(5).

accept([H|T]) :- start(A) , transition(A,H,B) , accept(T,B) , !.
accept([],Z) :- fin(Z).
accept([H|T],A) :- transition(A,H,B) , accept(T,B).

% Aufgabe 3 (Kompression)
%(a)
compress([],[]) :- !.
compress([H|T],X) :- compress(H,T,X) , !.

compress(H,[],[H]) :- !.
compress(H,[H|T],X) :- compress(H,T,X) , !.
compress(E,[H|T],[E|Es]) :- E \= H , compress(H,T,Es).

%(b)
encode([],[]) :- !.
encode([H|T],X) :- encode(H,T,X).

encode(H,[],[[1,H]]) :- !.
encode(H,L,X) :- encodeHead(H,L,X).

encodeHead(H,[],[[1,H]]) :- !.
encodeHead(H,[E|T],[[1,H]|T1]) :- 	H \= E ,
									encode(E,T,T1) , !.
encodeHead(H,[H|T],[[N,H]|T1]) :- encodeHead(H,T,[[N1,H]|T1]) ,
								  N is N1+1.


decode([],[]) :- !.
decode(DL,L) :- decode0(DL,L-[]).

decode0([],[]-[]) :- !.
decode0([[N,H]|T],L-L1) :- 	expand(N,H,L-L2) ,
							decode0(T,L2-L1).

expand(1,H,[H|T]-T) :- !.
expand(N,H,[H|L]-T) :- N1 is N-1 ,
					   expand(N1,H,L-T).
