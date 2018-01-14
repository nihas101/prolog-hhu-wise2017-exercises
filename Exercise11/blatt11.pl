:- use_module(library(clpfd)).

% N = 7040. The solutions are not unique as they include transformed solutions (mirrored etc.)
num_magic_squares(Num) :- findall(MS,magic_square(MS),MSs) ,
                          length(MSs,Num).

magic_square(MS) :- setup_magic_square(MS) , ! ,
                    labeling([],MS).

setup_magic_square(MS) :- MS = [A1,A2,A3,A4,
                                B1,B2,B3,B4,
                                C1,C2,C3,C4,
                                D1,D2,D3,D4] ,
                          MS ins 1..16 ,
                          34 #= A1 + A2 + A3 + A4 ,
                          34 #= B1 + B2 + B3 + B4 ,
                          34 #= C1 + C2 + C3 + C4 ,
                          34 #= D1 + D2 + D3 + D4 ,
                          34 #= A1 + B1 + C1 + D1 ,
                          34 #= A2 + B2 + C2 + D2 ,
                          34 #= A3 + B3 + C3 + D3 ,
                          34 #= A4 + B4 + C4 + D4 ,
                          34 #= A1 + B2 + C3 + D4 ,
                          34 #= A4 + B3 + C2 + D1 ,
                          all_different(MS) , !.

% N = 4 , Num = 2
% N = 5 , Num = 10
% N = 6 , Num = 4
% N = 7 , Num = 40
% N = 8 , Num = 92
num_sol(N,Num) :- findall(Sol,queens(N,Sol),Sols) ,
                  length(Sols,Num).

queens(N,Sol) :- setup_queens(1,N,Sol,PSol) ,
                 setup_relations(N,Sol) , ! ,
                 labeling([],PSol).

setup_queens(N,N,[(X,N)],[X]) :- X in 1..N , !.
setup_queens(Y,N,[(X,Y)|T],[X|T1]) :- Y1 is Y+1 ,                 % vertical
                                      X in 1..N ,
                                      setup_queens(Y1,N,T,T1).

setup_relations(_,[]) :- !.
setup_relations(N,[(X,_)|T]) :- setup_relations(N,1,X,T) ,
                                setup_relations(N,T).

setup_relations(_,_,_,[]) :- !.
setup_relations(N,Offset,X,[(X1,_)|T]) :- X1 #\= X ,              % horizontal
                                          Offset #\= abs(X-X1) ,  % diagonal
                                          Offset1 is Offset+1,
                                          setup_relations(N,Offset1,X,T).
