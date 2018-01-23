:- module(sat_solver,[solvername/1, to_cnf/2, solve/1]).

solvername('nihas101 SAT Solver').

% to_cnf(+F,-CNF_L)
% F - The formula to transform into CNF
% CNF_L - The resulting CNF as a list of lists
to_cnf(F,CNF_L) :-  eliminate_imp_move_neg(F,F1) ,
                    move_disjunctions(F1,CNF) ,
                    cnf_to_list(CNF,CNF_L) , !.

% to_cnf_simplify(+F,-CNF_L)
% F - The formula to transform into CNF
% CNF_L - The resulting CNF as a list of lists
% In Addition this simplifies the CNF and removes/simplifies clauses that are always true
to_cnf_simplify(F,CNF_L) :- eliminate_imp_move_neg(F,F1) ,
                            move_disjunctions(F1,CNF) ,
                            cnf_to_list(CNF,CNF_L1) ,
                            simplify_cnf_clauses(CNF_L1,CNF_L2) ,
                            delete_redundant_clauses(CNF_L2,CNF_L) , !.

% eliminate_imp_move_neg(+F,-F1)
% F - The formula to eliminate implications and negations from
% F1 - The resulting formula after eliminating implications
eliminate_imp_move_neg(lit(X),lit(X)) :- !.
eliminate_imp_move_neg(not(lit(X)),not(lit(X))) :- !.
eliminate_imp_move_neg(implies(A,B),or(AA,BB)) :- eliminate_imp_move_neg(not(A),AA) ,     % 1. Eliminate implications
                                                  eliminate_imp_move_neg(B,BB) , !.
eliminate_imp_move_neg(not(implies(A,B)),and(AA,BB)) :-  eliminate_imp_move_neg(A,AA) ,     % 1. Eliminate implications with not
                                                        eliminate_imp_move_neg(not(B),BB) , !.
eliminate_imp_move_neg(not(and(A,B)),or(AA,BB)) :-  eliminate_imp_move_neg(not(A),AA) ,   % 2. Move negations to atoms
                                                    eliminate_imp_move_neg(not(B),BB) , !.
eliminate_imp_move_neg(not(not(A)),D) :- eliminate_imp_move_neg(A,D) , !.
eliminate_imp_move_neg(or(A,B),or(AA,BB)) :-  eliminate_imp_move_neg(A,AA) ,
                                              eliminate_imp_move_neg(B,BB) , !.
eliminate_imp_move_neg(and(A,B),and(AA,BB)) :-  eliminate_imp_move_neg(A,AA) ,
                                                eliminate_imp_move_neg(B,BB) , !.
eliminate_imp_move_neg(not(A),AA) :- eliminate_imp_move_neg(A,AA).

% move_disjunctions(F,CNF)
% F - The formula for which to apply the law of distribution
% CNF - The resulting CNF
move_disjunctions(lit(X),lit(X)) :- !.
move_disjunctions(not(lit(X)),not(lit(X))) :- !.
move_disjunctions(or(A,and(B,C)),and(AA,BB)) :- move_disjunctions(or(A,B),AA) ,   % Move disjunctions to the literals
                                                move_disjunctions(or(A,C),BB) , !.
move_disjunctions(or(and(B,C),A),and(BB,AA)) :- move_disjunctions(or(A,B),AA) ,   % Commutativity
                                                move_disjunctions(or(A,C),BB) , !.
move_disjunctions(and(A,B),and(AA,BB)) :- move_disjunctions(A,AA) ,
                                          move_disjunctions(B,BB) , !.
move_disjunctions(or(A,B),CNF) :- move_disjunctions(A,AA) ,
                                  move_disjunctions(B,BB) ,
                                  ((AA=and(_,_) ; BB=and(_,_))
                                    -> move_disjunctions(or(AA,BB),CNF)
                                    ; CNF=or(AA,BB)) , !.

% cnf_to_list(+CNF,-CNF_L)
% CNF - The CNF to turn into a list
% CNF_L - The CNF as a list of lists of DNFs
cnf_to_list(CNF,CNF_L) :- cnf_to_list_(CNF,CNF_L-[]).

cnf_to_list_(lit(X),[[X]|T]-T) :- !.
cnf_to_list_(not(lit(X)),[[not(X)]|T]-T) :- !.
cnf_to_list_(and(A,B),[H|T]-T1) :-  dnf_to_list(A,H-[]) ,
                                    cnf_to_list_(B,T-T1) , !.
cnf_to_list_(and(A,B),CNF_L-T1) :-  cnf_to_list_(A,CNF_L-T) ,
                                    cnf_to_list_(B,T-T1) , !.
cnf_to_list_(or(A,B),[DNF_L|T]-T) :- dnf_to_list(or(A,B),DNF_L-[]).

% dnf_to_list(+DNF,-DNF_L)
% DNF - The DNF to turn into a list
% DNF_L - The DNF as list
dnf_to_list(lit(X),[X|T]-T) :- !.
dnf_to_list(not(lit(X)),[not(X)|T]-T) :- !.
dnf_to_list(or(A,B),DNF_L-T1) :-  dnf_to_list(A,DNF_L-T) ,
                                  dnf_to_list(B,T-T1).

% simplify_cnf_list(+CNF_L,-CNF_LR)
% CNF_L - The CNF as list of lists of DNFs to simplify
% CNF_LR - The simplified CNF
simplify_cnf_clauses([],[]) :- !.
simplify_cnf_clauses([H|T],[H1|T1]) :-  simplify_dnf_clauses(H,H1) ,
                                        simplify_cnf_clauses(T,T1).

% simplify_dnf_list(+DNF_L,-DNF_LR)
% DNF_L - The DNF as list to simplify
% DNF_LR - The simplified DNF
simplify_dnf_clauses([],[]) :- !.
simplify_dnf_clauses([H|T],DNF_L) :-  member_(H,T) ,
                                      simplify_dnf_clauses(T,DNF_L) , !.
simplify_dnf_clauses([H|T],[H|T1]) :- simplify_dnf_clauses(T,T1).

% delete_redundant_clauses(+CNF_L,-CNF_LR)
% CNF_L - The CNF as list of lists to delete redundant clauses from
% CNF_LR - The CNF as list of lists without redundant clauses
delete_redundant_clauses([],[]) :- !.
delete_redundant_clauses([H|T],CNF_L) :-  redundant_clause(H) ,
                                          delete_redundant_clauses(T,CNF_L) , !.
delete_redundant_clauses([H|T],[H|T1]) :- delete_redundant_clauses(T,T1).

% redundant_clause(+DNF_L)
% True if DNF_L is a redundant clause
redundant_clause([]) :- ! , fail.
redundant_clause([H|T]) :- neg_member_(H,T) , (H=true;H=false) , !.
redundant_clause([_|T]) :- redundant_clause(T).

% member_(+E,+L)
% E - The Element to search in the list L
% L - The list to search
member_(E,[H|_]) :- E==H , !.
member_(E,[_|T]) :- member_(E,T).

% neg_member_(+E,+L)
% E - The Element for which to search a negation in the list L
% L - The list to search
neg_member_(E,[H|_]) :- (E==H ; E==not(H) ; not(E)==H) , !.
neg_member_(E,[_|T]) :- neg_member_(E,T).

% solve(CNF)
% CNF - The CNF as list of lists of DNFs to solve
solve([]) :- !.
solve([H|T]) :- make_true(false,H) , solve(T).

% make_true(V,DNF)
% V - The value of this DNF up until this point
% DNF - The DNF to make true
make_true(false,[true]) :- !.
make_true(false,[not(false)]).
make_true(true,[true]) :- !.
make_true(true,[not(false)]).
make_true(true,[false]) :- !.
make_true(true,[not(true)]).
make_true(false,[true|T]) :- make_true(true,T).
make_true(false,[false|T]) :- make_true(false,T) , !.
make_true(false,[not(false)|T]) :- make_true(true,T).
make_true(false,[not(true)|T]) :- make_true(false,T).
make_true(true,[true|T]) :- make_true(true,T).
make_true(true,[false|T]) :- make_true(true,T) , !.
make_true(true,[not(false)|T]) :- make_true(true,T).
make_true(true,[not(true)|T]) :- make_true(true,T).
