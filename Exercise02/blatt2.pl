:- module(blatt2).

% is_a_list(L), testet, ob L eine Prolog-Liste ist
is_a_list([]) :- !.
is_a_list([_|T]) :- is_a_list(T) , !.

% infix(I, L), testet, ob I ein Infix der Liste L ist
infix(I,L) :- append(I,_,L) , !.
infix(I,[_|T]) :- infix(I,T) , !.

% suffix(S, L), testet, ob S ein Suffix der Liste L ist
suffix(S,L) :- append(_,S,L) , !.

% prefix(P, L), testet, ob P ein Präfix der Liste L ist
prefix(P,L) :- append(P,_,L) , !.

% element_of(E, L), testet, ob E ein Element der Liste L ist
element_of(E,[E|_]) :- !.
element_of(E,[_|T]) :- element_of(E,T) , !.

% del_element(E, L, R), löscht E aus der Liste L und gibt die neue Liste R zurück
% del_element löscht alle Elemente E aus der Liste
del_element(_,[],[]) :- !.
del_element(E,[E|T],R) :- del_element(E,T,R) , !. 
del_element(E,[H|T],[H|T1]) :- E \= H , del_element(E,T,T1) , !.

% Schreiben Sie ein Prädikat flatten/2, welches eine Liste von Listen in eine flache Liste überführt.
flatten([],[]).
flatten([H|T],[H|T1]) :- \+ is_a_list(H) ,
                          flatten(T,T1).
flatten([H|T],L) :- flatten(H,H1) ,
                    flatten(T,L1) ,
                    append(H1,L1,L).


% flatten_DL(List L, List R) - flatten_DL(+,-) will give a flattened version of the List L in R
flatten_DL(L,R) :- flatten_DL_(L,R-[]).

flatten_DL_([],L-L).
flatten_DL_([H|T],[H|T2]-T3) :- \+ is_a_list(H) ,
                                flatten_DL_(T,T2-T3).
flatten_DL_([H|T],L-L2) :- flatten_DL_(H,L-L1) ,
                           flatten_DL_(T,L1-L2).


% Implementieren Sie die folgenden Fakten in Prolog:
% 1. Siegfried liebt Krimhild und mag Gunther.
% 2. Krimhild liebt Siegfried und hasst Brunhild.
% 3. Gunther liebt Brunhild und mag Krimhild und Hagen.
% 4. Brunhild hasst Siegfried, Gunther und Krimhild.
% 5. Hagen hasst Siegfried und alle, die Siegfried lieben.
% 6. Brunhild mag alle, die Siegfried hassen.
% 7. Alberich hasst alle, mit Ausnahme von sich selbst.
loves('Siegfried','Krimhild').
loves('Krimhild','Siegfried').
loves('Gunther','Brunhild').

likes('Gunther','Krimhild').
likes('Gunther','Hagen').
likes('Brunhild',X) :- hates(X,'Siegfried').

hates('Brunhild','Siegfried').
hates('Brunhild','Gunther').
hates('Brunhild','Krimhild').
hates('Krimhild','Brunhild').
hates('Hagen','Siegfried').
hates('Hagen',X) :- loves(X,'Siegfried').
hates('Alberich',X) :- X \= 'Alberich'.

who_likes_brunhild(L) :- findall(X,likes(X,'Brunhild'),L).
who_hates_siegfried(L) :- findall(X,hates(X,'Siegfried'),L).
form_pairs(L) :- findall([X,Y], ( loves(X,Y) , X \= Y) ,L).

% Schreiben Sie ein Prädikat insert at/4, welches ein Element an einer gegebenen Position in eine Liste einfügt.
insert_at(E,L,N,R) :- length(L,LengthL) , N > LengthL , append(L,[E],R).
insert_at(E,L,N,R) :- insert_at_(E,L,N,R).

insert_at_(H,T,1,[H|T]) :- !.
insert_at_(E,[H|T],N,[H|T1]) :- N > 1 , N1 is N-1 , insert_at_(E,T,N1,T1).
