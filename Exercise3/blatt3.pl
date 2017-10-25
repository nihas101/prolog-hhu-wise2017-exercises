:- module(blatt3).

% Aufgabe 3 (SAT Solving in Prolog)
cst(true).
cst(false) :- fail.

is_true(A) :- A.

and(A,B) :- A , B.

or(A,B) :- A ; B .

not(B) :- \+ B.

% Aufgabe 4 (Größter gemeinsamer Teile und Anwendugen)
gcd(0,B,B) :- !.
gcd(A,0,A) :- !.
gcd(A,B,GCD) :-  A > B , B > 0 , A1 is A-B , gcd(A1,B,GCD) , !.
gcd(A,B,GCD) :- B >= A , A > 0 ,  B1 is B-A , gcd(A,B1,GCD).

coprime(A,B) :- gcd(A,B,1).

range_1(0,[]) :- !.
range_1(1,[]) :- !.
range_1(N,[1|L]) :- range_2(N,[1|L]-L) , !.
range_2(N,[A|_]-[B|T1]) :- B is A+1 , N > B , range_2(N,[B|T1]-T1) , !.
range_2(_,[_|[]]-[]). 

phi(M,N) :- range_1(M,L) , findall(A,(member(A,L),coprime(A,M)),L1) , length(L1,N).
% ALTERNATIVE WITHOUT FINDALL
phi_(0,0) :- !.
phi_(1,0) :- !.
phi_(M,N) :- range_1(M,L) , phi_(M,L,N) , !.
phi_(_,[],0) :- !.
phi_(M,[H|T],N) :- coprime(M,H) , phi_(M,T,N1) , N is N1+1 , !.
phi_(M,[_|T],N) :- phi_(M,T,N) , !.

% Aufgabe 5 (Listen)
duplicate(L,N,R) :- duplicate_(L,N,R-[]) , !.

duplicate_([],_,[]-[]) :- !.
duplicate_([H|T],N,R-R1) :- duplicate_Element(H,N,R-L2) , duplicate_(T,N,L2-R1) , !.

duplicate_Element(_,0,T-T) :- !.
duplicate_Element(H,N,[H|T]-T1) :- N1 is N-1 , duplicate_Element(H,N1,T-T1) , !.