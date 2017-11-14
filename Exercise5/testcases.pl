:- use_module(library(plunit)).
:- ensure_loaded(blatt5).


:- begin_tests(primes).

test(primes) :-
    primes(1,P) , P = [].

test(primes2) :-
    primes(7,P) , P = [2,3,5].

test(primes3) :-
    primes(25,P) , P = [2,3,5,7,11,13,17,19,23].

test(primes4) :-
    primes(15,P) , P = [2,3,5,7,11,13].

test(primes5) :-
    primes(17,P) , P = [2,3,5,7,11,13].

test(primes_fail,[fail]) :-
    primes(7,P) , member(4,P).

test(primes_fail2,[fail]) :-
    primes(17,P) , member(17,P).

:- end_tests(primes).
