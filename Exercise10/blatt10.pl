% Aufgabe 1 (Differenzlisten)
% (a)
same :- % Check equality
        X = [a,b|[]]-[] ,
        Y = [a,b,c]-[c] ,
        Z = [a,b|R1]-[R1] ,
        A = [a,b,c,d,e,f|R]-[d,e,f|R] ,
        B = [a,b,c]-[c,d] ,
        % Check difference
        X \= Y , X \= Z , X \= A , X \= B ,
        Y \= Z , Y \= A , Y \= B ,
        Z \= A , Z \= B ,
        A \= B.

% (b)
% toDL(+L,-DL,+T)
% L  - The list
% DL - The differencelist
toDL([],T,T).
toDL([H],[H|T],T) :- !.
toDL([H|T],[H|T1],T2) :- toDL(T,T1,T2) , !.

% (c)
% dlconcat(+L1-T1,+L2-T2,-ResL-T3).
dlconcat(L1-T1,T1-T2,L1-T2).

% d
% dlmember(M,DL)
dlmember(H,[H|T]-T) :- var(T) , !.
dlmember(H,[H|T]-_) :- \+var(T).
dlmember(H,[_|T]-T1) :- \+var(T) , dlmember(H,T-T1).


% Aufgabe 2 (Korrekte Klammerung)
parse_pars(Source,NumPairs) :- atom_codes(Source,SourceCodes) , bracket(NumPairs,SourceCodes,[]).

bracket(Pairs) --> (round(Pairs1) , bracket(Pairs2) , {Pairs is Pairs1 + Pairs2} , !)
                    ; ("" , {Pairs = 0}).
round(Pairs) -->   ("(" , ! , bracket(Pairs1) , {Pairs is Pairs1+1} , ")")
                    ; angular(Pairs).
angular(Pairs) --> ("<" , ! , bracket(Pairs1) , {Pairs is Pairs1+1} , ">")
                    ; curly(Pairs).
curly(Pairs) -->   ("{" , ! , bracket(Pairs1) , {Pairs is Pairs1+1} , "}")
                    ; square(Pairs).
square(Pairs) -->  "[" , ! , bracket(Pairs1) , {Pairs is Pairs1+1} , "]".

% Tests
test :- findall(X,test(X),Xs) , Xs = [0,1,2,3,4,5,6,7,8,9].
test(0) :- same.
test(1) :- toDL([1,2,3],[1,2,3,T],T).
test(2) :- toDL([1,2,3],DL,T) , T = [4,5,6] , DL = [1,2,3,4,5,6].
test(3) :- dlconcat([1,2,3|T]-T,[4,5,6|T1]-T1,Res-[]) , Res = [1,2,3,4,5,6].
test(4) :- dlconcat([1,2,3|T]-T,[4,5,6|T1]-T1,Res-T3) , T3 = [7] , Res = [1,2,3,4,5,6,7].
test(5) :- findall(M,dlmember(M,[1,2,3,4,5,6|T]-T),Ms) , Ms = [1,2,3,4,5,6] , var(T).
test(6) :- parse_pars("()",1).
test(7) :- parse_pars("(())[]<<>>{}{}{{<[]>}}",11).
test(8) :- parse_pars("()[]<>{}",4).
test(9) :- \+parse_pars("(())[",_).
