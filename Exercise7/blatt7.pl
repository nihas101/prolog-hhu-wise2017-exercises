:- module(blatt7).

% Aufgabe 2 (Higher-Order Praedikate)
mymaplist(_,[]) :- !.
mymaplist(P,[H|T]) :- call(P,H) , mymaplist(P,T).

mymaplist(_,[],[]) :- !.
mymaplist(P,[H|T],[H1|T1]) :- call(P,H,H1) , mymaplist(P,T,T1).

myfilter(_,[],[]) :- !.
myfilter(P,[H|T],[H|T1]) :- call(P,H) , myfilter(P,T,T1) , !.
myfilter(P,[_|T],R) :- myfilter(P,T,R).

% Aufgabe 3 (Polynome)
pol_sort(Polynom,SortedPolynom) :- pol_sort_(Polynom,SortedPolynom-[]) , !.
pol_sort_([],T-T) :- !.
pol_sort_([H|T],SortedPolynom-T1) :- split_polynom(H,T,L1,L2) , pol_sort_(L1,SortedPolynom-[H|T2]) , pol_sort_(L2,T2-T1).

split_polynom(_,[],[],[]) :- !.
split_polynom((A,B),[(AA,BB)|T],[(AA,BB)|T1],L2) :- B =< BB , split_polynom((A,B),T,T1,L2) , !.
split_polynom((A,B),[(AA,BB)|T],L1,[(AA,BB)|T2]) :- split_polynom((A,B),T,L1,T2).

pol_add(Pol1,Pol2,SumPol) :- pol_sort(Pol1,Pol1Sort) , pol_sort(Pol2,Pol2Sort) , pol_f(add,Pol1Sort,Pol2Sort,SumPol) , !.

pol_f(_,[],[],[]) :- !.
pol_f(_,L,[],L) :- !.
pol_f(_,[],L,L) :- !.
pol_f(F,[(X,Z)|T],[(Y,Z)|T1],[(XY,Z)|T2]) :- call(F,X,Y,XY) , pol_f(F,T,T1,T2) , !.
pol_f(F,[(X,Y)|T],[(A,B)|T1],[(X,Y)|T2]) :- Y>B , pol_f(F,T,[(A,B)|T1],T2) , !.
pol_f(F,[(X,Y)|T],[(A,B)|T1],[(A,B)|T2]) :- pol_f(F,[(X,Y)|T],T1,T2).

add(A,B,C) :- C is A+B.

pol_mul([],_,[]) :- !.
pol_mul([H|T],Pol2,MulPol) :- pol_mul_single(H,Pol2,HPol) , pol_sort(HPol,HPolSorted) , pol_mul(T,Pol2,Mulpol1) , pol_add(HPolSorted,Mulpol1,MulPol) , !.

pol_mul_single(_,[],[]) :- !.
pol_mul_single((A,B),[(C,D)|T],[(AC,BD)|T1]) :- AC is A*C , BD is B+D , pol_mul_single((A,B),T,T1).

pol_diff([],[]) :- !.
pol_diff([(_,0)|T],Diff) :- pol_diff(T,Diff) , !.
pol_diff([(A,B)|T],[(AA,BB)|T1]) :- AA is A*B , BB is B-1 , pol_diff(T,T1).

pol_eval([],_,0) :- !.
pol_eval([(A,B)|T],Value,Result) :- pol_eval(T,Value,Result1) , Result is A*Value^B + Result1.
