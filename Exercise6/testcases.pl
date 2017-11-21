:- use_module(library(plunit)).
:- ensure_loaded(blatt6).


:- begin_tests(double_append).

test(double_append_forward,[nondet]) :-
    dapp([1],[2],[3],Res1) , Res1 = [1,2,3] ,
    dapp([1,2,3],[2,3],[3],Res2) , Res2 = [1,2,3,2,3,3] ,
    dapp([1],[],[],Res3) , Res3 = [1] ,
    dapp([],[],[],Res4) , Res4 = [].

test(double_append_backwards,[nondet]) :-
    dapp(A1,B1,C1,[1,2,3,4]) ,
    append([A1,B1,C1],[1,2,3,4]) ,
    dapp(A2,B2,C2,[]) ,
    A2 = [] , B2 = [] , C2 = [] ,
    dapp(A3,B3,C3,[1,2]) ,
    append([A3,B3,C3],[1,2]) ,
    dapp(A4,B4,[3],[1,2,3]) ,
    append([A4,B4,[3]],[1,2,3]) ,
    dapp([1],B5,C5,[1,2,3]) ,
    append([[1],B5,C5],[1,2,3]).

:- end_tests(double_append).

:- begin_tests(expression_logic).

test(is_atomic_expr,[nondet]) :-
    is_atomic_expr(a),
    is_atomic_expr(b),
    is_atomic_expr(c),
    \+ is_atomic_expr(f(a)),
    \+ is_atomic_expr(_X),
    \+ is_atomic_expr(not(a)).

test(is_literal,[nondet]) :-
    is_literal(a),
    is_literal(b),
    \+ is_literal(_X),
    \+ is_literal(f(a)),
    is_literal(not(a)),
    \+ is_literal(not(not(a))).

test(simplify_expr_basic,[nondet]) :-
    simplify_expr(a, a),
    simplify_expr(not(a), not(a)),
    simplify_expr(or(a, b), or(a, b)),
    simplify_expr(and(a, b), and(a, b)).

test(simplify_expr_negations,[nondet]) :-
    simplify_expr(not(not(a)), a),
    simplify_expr(not(not(not(a))), not(a)),
    simplify_expr(not(not(not(not(a)))), a).

test(simplify_expr_de_morgan,[nondet]) :-
    simplify_expr(not(and(a, b)), or(not(a), not(b))),
    simplify_expr(not(or(a, b)), and(not(a), not(b))).

test(simplify_expr_de_morgan_extended,[nondet]) :-
    simplify_expr(not(and(not(a), not(b))), or(a, b)).

test(is_clause,[nondet]) :-
    is_clause(a),
    is_clause(not(a)),
    is_clause(or(a, b)),
    is_clause(or(a, or(b, c))),
    is_clause(or(not(a), or(b, not(c)))),
    \+ is_clause(or(a, and(b, c))),
    \+ is_clause(and(a, b)),
    is_clause(or(or(a, b), c)).

test(is_clause_simplify,[nondet]) :-
    is_clause(not(and(a, b))).

test(is_horn_clause_simple,[nondet]) :-
    is_horn_clause(a),
    is_horn_clause(b).

test(is_horn_clause,[nondet]) :-
    is_horn_clause(not(a)),
    is_horn_clause(or(a, not(b))),
    is_horn_clause(or(not(a), b)),
    is_horn_clause(or(not(a), not(b))),
    \+ is_horn_clause(or(a, b)),
    \+ is_horn_clause(and(a, not(b))),

    is_horn_clause(or(a, or(not(b), not(c)))),
    \+ is_horn_clause(or(a, or(not(b), c))),

    is_horn_clause(or(or(not(a), b), not(c))).

test(is_horn_clause_simplify,[nondet]) :-
    is_horn_clause(not(and(a, b))).

:- end_tests(expression_logic).
