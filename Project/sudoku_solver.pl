:- module(sudoku_solver,[solvername/1, solve_sudoku/2]).

:- use_module(library(clpfd)).

% solvername(-Name)
% Name - The solver's name
solvername('nihas101 Sudoku Solver').

% solve_sudoku(+Rows,-SolvedSudoku)
% Rows - A list of the rows of the sudoku
% SolvedSudoku - The solved sudoku
solve_sudoku(Rows,Sudoku) :-  length_(Rows,NN) ,
                              N is round(sqrt(NN)) ,
                              length(Sudoku,NN) ,
                              set_list_length(Sudoku,NN) ,
                              setup_sudoku(NN,Rows,Sudoku) ,
                              split_sudoku_columns(Sudoku,Columns) ,
                              split_sudoku_squares(Sudoku,N,Squares) , ! ,
                              maplist(all_different,Sudoku) ,
                              maplist(all_different,Columns) ,
                              maplist(all_different,Squares) ,
                              flatten(Sudoku,FSudoku) , ! ,
                              labeling([],FSudoku).

% set_list_length(+L,+NN)
% L - A list of lists
% NN - The length to set the lists to
set_list_length([],_).
set_list_length([H|T],NN) :- length(H,NN) , set_list_length(T,NN).

% setup_sudoku(+NN,+Sud,-CSud)
% NN - The length of a row
% Sud - The given sudoku
% CSud - The sudoku with constraints
setup_sudoku(_,[],[]).
setup_sudoku(NN,[[]|T1],[[]|T3]) :- setup_sudoku(NN,T1,T3).
setup_sudoku(NN,[[H|T]|T1],[[H|T2]|T3]) :- var(H) , H in 1..NN , setup_sudoku(NN,[T|T1],[T2|T3]).
setup_sudoku(NN,[[H|T]|T1],[[H1|T2]|T3]) :- H1 in H..H , setup_sudoku(NN,[T|T1],[T2|T3]).

% split_sudoku_columns(+Rows,-Columns)
% Rows - The Sudoku as a list of rows
% Columns - The sudoku as a list of columns
split_sudoku_columns([],L) :- list_of_empty_lists(L) , !.
split_sudoku_columns([H|T],L) :-  split_list(H,L,T1) ,
                                  split_sudoku_columns(T,T1) , !.

% split_sudoku_squares(+Rows,+N,-Squares)
% N - The depth of a square
% Rows - The sudoku as a list of rows
% Squares - The sudoku as a list of columns
split_sudoku_squares(Rows,N,Squares) :- split_sudoku_squares0(N,Rows,Squares).

split_sudoku_squares0(_,[],[]).
split_sudoku_squares0(N,[H|T],R) :- split_list(N,H,R-Tail,Ts) ,
                                    split_sudoku_squaresR(N,N,T,Ts,TR) ,
                                    split_sudoku_squares0(N,TR,Tail).

split_sudoku_squaresR(1,_,T,Ts,T) :- list_of_empty_lists(Ts).
split_sudoku_squaresR(N1,N,[H|T],Ts,TR) :-  N2 is N1-1 ,
                                            split_list(N,H,Ts-[],Ts1) ,
                                            split_sudoku_squaresR(N2,N,T,Ts1,TR).

% split_list(+L,-LL,-T)
% L - The list to split into lists
% LL - The list of lists
% T - The tails of the lists
split_list([],[],[]).
split_list([H|T],[[H|T1]|T2],[T1|T3]) :- split_list(T,T2,T3).

% split_list(+N,+L,-R,-Ts)
% N - The length of a list
% L - The list to split
% R - The resulting list of lists
% Ts - The tails of the lists
split_list(N,L,R-T,Ts) :- split_list_(N,N,L,R-T,Ts) , !.
split_list_(_,_,[],[Ts|T]-T,[Ts]) :- !.
split_list_(0,N,Rows,[T|T1]-T2,[T|Ts]) :- split_list_(N,N,Rows,T1-T2,Ts) , !.
split_list_(N1,N,[H|T1],[[H|T2]|T3]-T4,Tails) :-  N2 is N1-1,
                                                  split_list_(N2,N,T1,[T2|T3]-T4,Tails).

% list_of_empty_lists(?L)
% L - list of empty lists.
list_of_empty_lists([[]]).
list_of_empty_lists([[]|T]) :- list_of_empty_lists(T).

length_([],0).
length_([_|T],N) :- length_(T,N1) , N is N1+1.
