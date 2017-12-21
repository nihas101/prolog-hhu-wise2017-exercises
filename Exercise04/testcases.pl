:- use_module(library(plunit)).
:- ensure_loaded(blatt4).


:- begin_tests(automaton).

test(accept) :-
    accept([d,a,b,a,b,b,b,c,d,c]).

test(accept2) :-
    accept([d,a,b,d]).

test(accept3) :-
    accept([d,e]).

test(accept4) :-
    accept([d,a,b,a,b,a,a,a,a,b,b,a,b,b,a,a,b,b,b,e]).

test(not_accept,[fail]) :-
    accept([d,a,b,a,e,d,c]).

test(not_accept2,[fail]) :-
    accept([d,a,b,d,d]).

test(not_accept3,[fail]) :-
    accept([d,a,b,a,b,a,a,a,b,b,a,a,b,b,c,b,e]).

:- end_tests(automaton).

:- begin_tests(compress).

test(compress1) :-
    compress([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X), X == [a,b,c,a,d,e].

test(compress2) :-
    compress([a,a,a,a,b,c,c,a,d,e],X), X == [a,b,c,a,d,e].

test(compress3,[fail]) :-
    compress([a,a,a,a,b,c,c,a,d,e],[a,b,c,d,e]).

test(compress4,[fail]) :-
    compress([a,a,a,a,b,c,c,a,d,e],[a,a,a,a,b,c,c,a,d,e]).

:- end_tests(compress).

:- begin_tests(encodedecode).

test(encode) :-
    encode([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X),
    X == [[4,a],[1,b],[2,c],[2,a],[1,d],[4,e]].

test(encode2) :-
    encode([a,a,b,a,a,b,c,c,d,e,e],X),
    X == [[2,a],[1,b],[2,a],[1,b],[2,c],[1,d],[2,e]].

test(decode) :-
    decode([[2,a],[3,x]],X),
    X == [a,a,x,x,x].

test(decode2) :-
    decode([[2,a],[3,b],[1,c],[3,a]],X),
    X == [a,a,b,b,b,c,a,a,a].

:- end_tests(encodedecode).
