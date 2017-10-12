:- module(sheeps).

:- use_module(library(plunit)).

female(dolly).
female(haba).
female(doerte).
female(pauline).

male(friedrich).
male(gunter).
male(peter).

parent(haba,dolly).
parent(haba,gunter).
parent(friedrich,dolly).
parent(friedrich,gunter).
parent(gunter,peter).
parent(gunter,pauline).
parent(doerte,peter).
parent(doerte,pauline).

%%%% Prolog Code hier
% Schreiben Sie ein Praedikat sheep/1, dass fuer alle Schafe in der Datenbank wahr ist.
sheep(X) :- female(X) ; male(X).
% Schreiben Sie zwei Praedikate father/2 und mother/2. Die Anfrage father(F,C) soll genau dann wahr
% sein, wenn F Vater von C ist (mother/2 analog).
father(F,C) :- male(F) , parent(F,C).
mother(M,C) :- female(M) , parent(M,C).
% Schreiben Sie ein Praedikat ancestor/2. Die Anfrage ancestor(A,D) soll genau dann wahr sein, wenn A
% eine Vorfahrin bzw. ein Vorfahr von D ist.
ancestor(A,D) :- parent(A,D).
ancestor(A,D) :- parent(A,C) , ancestor(C,D).



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
