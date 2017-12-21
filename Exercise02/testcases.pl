:- use_module(library(plunit)).
:- ensure_loaded(blatt2).


:- begin_tests(lists).

test(is_a_list) :-
    is_a_list([a,b,c]).

test(is_not_a_list,[fail]) :-
    is_a_list(list).

test(infix) :-
    infix([b,c],[a,b,c,d]).

test(suffix) :-
    suffix([c,d],[a,b,c,d]).

test(prefix) :-
    prefix([a,b],[a,b,c,d]).

test(element_of) :-
    element_of(b, [a,b,c,d]).

test(not_element_of,[fail]) :-
    element_of(asdf,[a,b,c,d]).

test(del_element) :-
    del_element(a,[a,b,c,d],[b,c,d]).

test(del_element_twice) :-
    del_element(a,[a,b,c,d,a],[b,c,d]).

test(del_element_twice_failing,[fail]) :-
    del_element(a,[a,b,c,d,a],[b,c,d,a]).

:- end_tests(lists).

:- begin_tests(flatten).

test(flatten,[nondet]) :-
    flatten([],[]).

test(flatten2,[nondet]) :-
    flatten([a,b,c],[a,b,c]).

test(flatten3,[nondet]) :-
    flatten([[a,[b,c]]],[a,b,c]).

test(flatten4,[nondet]) :-
    flatten([a,b,[c],[d,e],[[f],g]],[a,b,c,d,e,f,g]).

test(flatten5,[nondet]) :-
    flatten([[a,b,[[[c]]],[[d,e]],[[f],[[g]]]]],[a,b,c,d,e,f,g]).

test(flatten6,[nondet]) :-
    flatten([[],a,[b,[c]],[]],[a,b,c]).

:- end_tests(flatten).

:- begin_tests(insert_at).

test(insert_at,[nondet]) :-
    insert_at(test,[a,b,c],2,[a,test,b,c]).

test(insert_at2,[nondet]) :-
    insert_at(k,[a,b],1,[k,a,b]).

test(insert_at3,[nondet]) :-
    insert_at(d,[a],3,[a,d]).

test(insert_at4,[nondet]) :-
    insert_at(p,[],3,[p]).

:- end_tests(insert_at).
