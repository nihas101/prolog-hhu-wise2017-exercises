:- use_module(library(plunit)).
:- ensure_loaded(blatt3).

%% Start Testcases

:- begin_tests(duplicate).

test(duplicate,[nondet]) :-
    duplicate([a,b,c],3,L) , L = [a,a,a,b,b,b,c,c,c].

test(duplicate2,[nondet]) :-
    duplicate([a,b,c,d],2,L) , L = [a,a,b,b,c,c,d,d].

test(duplicate3,[nondet]) :-
    duplicate([a,b,c],1,L) , L = [a,b,c].

:- end_tests(duplicate).



:- begin_tests(sat).

test(simple_sat) :-
    is_true(cst(true)).

test(sat,[nondet]) :-
    is_true(or(not(and(cst(true),cst(false))),cst(false))).

test(sat2,[nondet]) :-
    is_true(or(not(and(cst(true),cst(false))),and(cst(true),or(cst(false),cst(true))))).

test(unsat,[fail]) :-
    is_true(or(not(cst(true)),cst(false))).

test(unsat2,[fail]) :-
    is_true(and(not(cst(true)),or(cst(false),cst(true)))).

:- end_tests(sat).



:- begin_tests(gcd).

test(gcd,[nondet]) :-
    gcd(36,63,GCD) , GCD = 9.

test(gcd2,[nondet]) :-
    gcd(1253,23,GCD) , GCD = 1.

test(gcd3,[nondet]) :-
    gcd(324,12,GCD) , GCD = 12.

test(gcd4,[nondet]) :-
    gcd(368,34,GCD) , GCD = 2.

test(gcd_function,[nondet]) :-
    GCD is gcd(36,63) , GCD = 9.

test(gcd_function2,[nondet]) :-
    GCD is gcd(1253,23) , GCD = 1.

test(coprime,[nondet]) :-
    coprime(1253,23).

test(coprime2,[nondet]) :-
    coprime(1234423,123).

test(coprime3,[fail]) :-
    coprime(324,12).

test(coprime4,[fail]) :-
    coprime(36,63).

test(range,[nondet]) :-
    range_1(10,L) , L = [1,2,3,4,5,6,7,8,9].

test(range2,[nondet]) :-
    range_1(5,L) , L = [1,2,3,4].

test(range3,[nondet]) :-
    range_1(1,L) , L = [].

test(range4,[nondet]) :-
    range_1(0,L) , L = [].

test(phi,[nondet]) :-
    phi(10,L) , L = 4.

test(phi2,[nondet]) :-
    phi(15,L) , L = 8.

test(phi3,[nondet]) :-
    phi(323,L) , L = 288.

:- end_tests(gcd).
