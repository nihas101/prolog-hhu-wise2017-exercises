:- use_module(library(plunit)).
:- ensure_loaded(sheeps).

% Start Testcases: run_tests(sheeps).

:- begin_tests(sheeps,[]).

test(sheep,[nondet]) :-
    sheep(friedrich) , 
    sheep(doerte).

test(sheep_failing,[fail]) :-
    sheep(sebastian).

test(father,[nondet]) :-
    father(friedrich,dolly).

test(father_2,[nondet]) :-
    father(gunter,peter).

test(father_failing,[fail]) :-
    father(doerte,pauline).

test(father_failing_2,[fail]) :-
    father(friedrich,peter).

test(mother,[nondet]) :-
    mother(haba,dolly).

test(mother_failing,[fail]) :-
    mother(gunter,pauline).

test(mother_failing_2,[fail]) :-
    mother(friedrich,peter).

test(ancestor,[nondet]) :-
    ancestor(friedrich,peter).

test(ancestor_2,[nondet]) :-
    ancestor(friedrich,pauline).

test(ancestor_failing,[fail]) :-
    ancestor(dolly,peter).

test(ancestor_failing_2,[fail]) :-
    ancestor(peter,peter).

:- end_tests(sheeps).
