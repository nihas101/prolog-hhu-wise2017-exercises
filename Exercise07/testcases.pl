:- use_module(library(plunit)).
:- ensure_loaded(blatt7).


:- begin_tests(higher_order).

test(maplist_2,[]) :-
    mymaplist(ground,[]) , ! ,
    mymaplist('='(a),[]) , ! ,
    mymaplist(ground,[a,b,c]) , ! ,
    mymaplist(var,[_,_,_,_,_,_]) , ! ,
    \+mymaplist('='(a),[a,b,c]) , ! ,
    mymaplist('='(a),[a,a,a,a,a,a]) , ! ,
    mymaplist('='(a),[a,a,a,a,a,a,_,_,_]) , ! ,
    \+mymaplist('=='(a),[a,a,a,a,a,a,_,_,_]).

test(maplist_3,[]) :-
    mymaplist(atom_concat(a),[],M1) , ! , M1 = [] ,
    mymaplist(atom_concat(a),[a,b,c],M2) , ! , M2 = [aa,ab,ac] ,
    mymaplist(append([1]),[[2],[3]],M3) , ! , M3 = [[1,2],[1,3]].

test(filter,[]) :-
    myfilter(ground,[a,_,b,c,_,_,_,_],F1) , ! , F1 = [a,b,c] ,
    myfilter(integer,[],F2) , ! , F2 = [] ,
    myfilter(integer,[2,a,1,b,c],F3) , ! , F3 = [2,1] ,
    myfilter('<'(5),[1,2,3,4,5,6,7,8,9,10],F4) , ! , F4 = [6,7,8,9,10].

:- end_tests(higher_order).

:- begin_tests(polynome).

test(sort) :-
    pol_sort([(8,2),(-5,1),(-1,0),(7,3)],X),
    X == [(7,3),(8,2),(-5,1),(-1,0)].

test(add) :-
    pol_add([(7,3),(8,2),(-5,1),(-1,0)],
            [(7,3),(8,2),(-5,1),(-1,0)],
            X),
    X == [(14,3),(16,2),(-10,1),(-2,0)].

test(mul) :-
    pol_mul([(7,3),(8,2),(-5,1),(-1,0)],
            [(7,3),(8,2),(-5,1),(-1,0)],
            X),
    X == [(49,6),(112,5),(-6,4),(-94,3),(9,2),(10,1),(1,0)].

test(diff) :-
    pol_diff([(7,3),(8,2),(-5,1),(-1,0)],X),
    X == [(21,2),(16,1),(-5,0)].

test(pol_eval) :-
    pol_eval([(49,6),(112,5),(6,4),(94,3),(9,2),(10,1),(1,0)],
             7,
             X),
    X == 7694345.

:- end_tests(polynome).
