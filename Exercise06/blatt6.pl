:- module(blatt6).

% Aufgabe 3 (Double-Append)
dapp([],[],R,R).
dapp([],[Y|Ys],Z,[Y|R]) :- dapp([],Ys,Z,R).
dapp([X|Xs],Y,Z,[X|R]) :- dapp(Xs,Y,Z,R).



% Aufgabe 4 (Simplizieren von aussagenlogischen Termen)
% Literale sind p oder NOT(p), wobei p eine atomare Aussage ist
is_literal(A) :- is_atomic_expr(A) , !.
is_literal(not(A)) :- is_atomic_expr(A) , !.

% Atomare Aussage sind Aussagen ohne Junktoren
is_atomic_expr(Term) :- var(Term) , ! , fail.
is_atomic_expr(and(_,_)) :- ! , fail.
is_atomic_expr(or(_,_)) :- ! , fail.
is_atomic_expr(Term) :- atom(Term) , !.


simplify_expr(not(not(Term)),Simplified) :- simplify_expr(Term,Simplified) , !. % not(not(A)) = A
simplify_expr(not(and(A,B)),or(AS,BS)) :- simplify_expr(not(A),AS) ,
                                          simplify_expr(not(B),BS) , !.         % De Morgan: not(and(A,B)) = or(not(A),not(B))
simplify_expr(not(or(A,B)),and(AS,BS)) :- simplify_expr(not(A),AS) ,
                                          simplify_expr(not(B),BS) , !.         % De Morgan: not(or(A,B)) = and(not(A),not(B))
simplify_expr(not(A),not(Simplified)) :- simplify_expr(A,Simplified) , !.       % Look further into the Term
simplify_expr(and(A,B),and(AS,BS)) :- simplify_expr(A,AS) ,
                                      simplify_expr(B,BS) , !.                  % Look further into the Term
simplify_expr(or(A,B),or(AS,BS)) :- simplify_expr(A,AS) ,
                                    simplify_expr(B,BS) , !.                    % Look further into the Term
simplify_expr(Term,Term) :- is_literal(Term) , !.                               % Lowest level reached


% Eine Klausel ist die Disjunktion von Literalen
is_clause(Term) :-  simplify_expr(Term,Simplified) ,
                    is_clause_(Simplified).
is_clause_(L) :- is_literal(L) , !.
is_clause_(or(A,B)) :-  is_clause_(A) ,
                        is_clause_(B) , !.

% Horn Klauseln sind Disjunktionen, die maximal ein positives Literal besitzen
is_horn_clause(Term) :- simplify_expr(Term,Simplified) ,
                        count_pos_lit_in_disjunction(Simplified,N) , N < 2.
% Anfragen sind Disjunktionen, die kein positives Literal besitzen
is_denial(Term) :-  simplify_expr(Term,Simplified) ,
                    count_pos_lit_in_disjunction(Simplified,0).

count_pos_lit_in_disjunction(not(L),0) :- is_literal(L) , !.
count_pos_lit_in_disjunction(L,1) :- is_literal(L) , !.
count_pos_lit_in_disjunction(not(A),N) :- count_pos_lit_in_disjunction(A,N) , !.
count_pos_lit_in_disjunction(or(A,B),N) :-  count_pos_lit_in_disjunction(A,N1) ,
                                            count_pos_lit_in_disjunction(B,N2) ,
                                            N is N1+N2 , !.
