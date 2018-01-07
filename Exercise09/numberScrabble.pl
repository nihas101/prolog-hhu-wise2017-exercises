:- write("---------- Number Scrabble ----------
R U L E S :
- Two players choose from the numbers 1 through 9
- A number can only be chosen once by one player
- The player whose sum of numbers equals 15 first has won
- If no player can reach 15 as sum anymore the game is a draw

H O W  T O  P L A Y  T H E  G A M E :
- Type \'play(0).\' to let the AI play against itself
- Type \'play(1).\' to play a game against AI
- Type \'play(2).\' to play a game against another player
- Each round type one of the numbers from the given set\n\n").

% print_state(+R,+S)
% R - The round (i.e. number of plays)
% S - The state of the game
print_state(Round,state(_,_,_,p1(Sum1,Choices1),p2(Sum2,Choices2))) :-
write("---------- R O U N D : ") , print(Round) , write(" ----------") , nl , nl ,
write("Player 1: ") , print_choices(Choices1) , write(" = ") , print(Sum1) , nl ,
write("Player 2: ") , print_choices(Choices2) , write(" = ") , print(Sum2) , nl , nl , !.

% print_choices(+C)
% C - The choices of the player to be printed
print_choices([]) :- write("-").
print_choices([H|T]) :- print(H) , print_choices0(T) , !.
print_choices0([]) :- !.
print_choices0([H|T]) :- write(" + ") , print(H) , print_choices0(T) , !.

% print_win(S)
% S - The endstate of the game
print_win(state(_,0,_,_,_)) :- nl , write("---------- Draw! ----------") , nl.
print_win(state(_,-1,_,_,_)) :- nl , write("---------- ") , write("Player 1 has won!") , write(" ----------") , nl.
print_win(state(_,1,_,_,_)) :- nl , write("---------- ") , write("Player 2 has won!") , write(" ----------") , nl.

% play(N)
% N = 0 : Let the ai play against itself
% N = 1 : Play against the ai
% N = 2 : Play with 2 players
play(0) :- start(S) , print_state(0,S) , play(ai1,1,S) , !.
play(1) :- start(S) , print_state(0,S) , play(player,1,S) , !.
play(2) :- start(S) , print_state(0,S) , play(player1,1,S) , !.

% play(+Player,+Round,+State)
% Player - The current player
% Round - The current round
% State - The state of the game
play(ai1,N,State) :-  findall(NState,s(ai,State,NState),NStates) ,
                      minmax(NStates,state(M,_,R,P1,P2)) ,
                      check_win(ai1,N,state(M,_,R,P1,P2)).
play(ai2,N,State) :-  N1 is N+1,
                      findall(NState,s(ai,State,NState),NStates) ,
                      minmax(NStates,state(M,_,R,P1,P2)) ,
                      nl , print_state(N,state(M,_,R,P1,P2)) ,
                      check_win(ai2,N1,state(M,_,R,P1,P2)).

play(player,N,State) :- move_input(player,State,NState) ,
                        check_win(player,N,NState).
play(ai,N,State) :- N1 is N+1,
                    findall(NState,s(ai,State,NState),NStates) ,
                    minmax(NStates,NextState) ,
                    print_state(N,NextState) ,
                    check_win(ai,N1,NextState).

play(player1,N,State) :-  write("Player 1 - ") ,
                          move_input(player1,State,NState) ,
                          check_win(player1,N,NState).
play(player2,N,State) :-  N1 is N+1 , write("Player 2 - ") ,
                          move_input(player2,State,NState) ,
                          print_state(N,NState) , nl ,
                          check_win(player2,N1,NState).

% check_win(+Player,+Round,+State)
% Player - The current player
% Round - The current round
% State - The current state of the game
check_win(Player,Round,State) :- (goal(State) -> print_win(State) ; other_player(Player,OPlayer) , play(OPlayer,Round,State)).

% start(-State)
% State - The state at the beginning of the game
start(state(max,_,Range, p1(0,[]), p2(0,[]))) :- range_1(10,Range).

% move_input(+Player,+State,-NextState)
% Player - The current player
% State - The State before moving
% NextState - The State after moving
move_input(Player,State,NextState) :- \+goal(State) ,
                                      s(Player,State,NextState).

% minmax(+States,-NextState)
% States - The start states in a list
% NextState - The next state according to the minimax algorithm
minmax([],state(none)).
minmax([state(M,V,R,P1,P2)|States],NextState) :-  \+goal(state(M,V,R,P1,P2)) , ! ,
                                        findall(NState,s(ai,state(M,V,R,P1,P2),NState),NStates) ,
                                        minmax(NStates,state(_,V,_,_,_)) ,
                                        minmax(States,MinMaxState) ,
                                        minmax_state(M,state(M,V,R,P1,P2),MinMaxState,NextState) , !.
minmax([state(M,V,R,P1,P2)|States],NextState) :-  goal(state(M,V,R,P1,P2)) ,
                                                  minmax(States,NState) ,
                                                  minmax_state(M,state(M,V,R,P1,P2),NState,NextState).
% s(+Player,+State,-NextState)
% Player - The current player
% State - The State before moving
% NextState - The State after moving
s(player,state(max,_,Range,p1(Sum,Player1),Player2),state(min,_,NRange,p1(Sum1,[N|Player1]),Player2)) :- prompt(Range,N,NRange) , Sum1 is Sum+N , nl.

s(player1,state(max,_,Range,p1(Sum,Player1),Player2),state(min,_,NRange,p1(Sum1,[N|Player1]),Player2)) :- prompt(Range,N,NRange) , Sum1 is Sum+N , nl.
s(player2,state(min,_,Range,Player1,p2(Sum,Player2)),state(max,_,NRange,Player1,p2(Sum1,[N|Player2]))) :- prompt(Range,N,NRange) , Sum1 is Sum+N , nl.

s(ai,state(max,Val,Range,p1(Sum,Player1),Player2),state(min,Val,NRange,p1(Sum1,[N|Player1]),Player2)) :- select(N,Range,NRange) , Sum1 is Sum+N.
s(ai,state(min,Val,Range,Player1,p2(Sum,Player2)),state(max,Val,NRange,Player1,p2(Sum1,[N|Player2]))) :- select(N,Range,NRange) , Sum1 is Sum+N.

% goal(+State)
% State - One of the possible endstates
goal(state(_,-1,_,p1(15,_),p2(_,_))).
goal(state(_,1,_,p1(_,_),p2(15,_))).
goal(state(_,0,Range,p1(Sum1,_),p2(Sum2,_))) :- length(Range,Length) ,
                                                Plys is Length//2 ,
                                                \+can_win(Sum1,Range,Plys) ,
                                                \+can_win(Sum2,Range,Plys).
goal(state(_,0,_,p1(N0,_),p2(N1,_))) :- N0 > 15 , N1 > 15.
goal(state(_,-1,_,p1(_,_),p2(N,_))) :- N > 15.
goal(state(_,1,_,p1(N,_),p2(_,_))) :- N > 15.

% prompt(+Range,+N,-NRange)
% Range - The list of numbers to choose from
% N - The choosen number
% NRange - The leftover numbers
prompt(Range,N,NRange) :- write('Choose one of the following numbers ') , print(Range) , write(': ') , read(N) , select(N,Range,NRange) , !.
prompt(Range,N,NRange) :- prompt(Range,N,NRange).

can_win(Sum,Range,Plys) :-  Plys > 0 ,
                            select(N,Range,_) ,
                            15 is Sum+N , !.
can_win(Sum,Range,Plys) :-  Plys > 1 ,
                            select(N,Range,NRange) ,
                            Sum1 is Sum+N ,
                            Plys1 is Plys-1 ,
                            can_win(Sum1,NRange,Plys1) , !.
% minmax_state(+M,+State1,+State2,-MState)
% M - min or max switch
% State1 - The first state to compare
% State2 - The second state to compare
% MState - The state that is either of higher or lower value
minmax_state(_,state(none),State,State) :- !.
minmax_state(_,State,state(none),State) :- !.
minmax_state(max,state(M,V,R,P1,P2),state(_,V1,_,_,_),state(M,V,R,P1,P2)) :- V > V1 , !.
minmax_state(max,state(_,_,_,_,_),state(M1,V1,R1,P11,P21),state(M1,V1,R1,P11,P21)) :- !.
minmax_state(min,state(M,V,R,P1,P2),state(_,V1,_,_,_),state(M,V,R,P1,P2)) :- V < V1 , !.
minmax_state(min,state(_,_,_,_,_),state(M1,V1,R1,P11,P21),state(M1,V1,R1,P11,P21)) :- !.

% sort_states(+M,+States,-SortedStates)
% M - min or max switch
% States - The states to sort
% SortedStates - The states in either ascending or descending order
sort_states(_,[],T-T) :- !.
sort_states(M,[State|States],SortedStates-T) :- split_states(M,State,States,States1,States2) ,
                                                sort_states(M,States1,SortedStates-[State|T1]) ,
                                                sort_states(M,States2,T1-T) , !.
% split_states(+M,+State,+States,-States1,-States2)
% M - min or max switch
% State - The state for which States1 =< State =< States2 or States1 >= State >= States2 holds
% States - The States before splitting
% States1 - The elements for which States1 =< State or States1 >= State holds
% States2 - The elements for which States2 >= State or States2 =< State holds
split_states(_,_,[],[],[]) :- !.
split_states(min,state(_,V,_,_,_),[state(P,V1,R,P1,P2)|CStates],[state(P,V1,R,P1,P2)|States1],States2) :- V > V1 ,
                                                                                                          split_states(min,state(_,V,_,_,_),CStates,States1,States2) , !.
split_states(min,State,[CState|CStates],States1,[CState|States2]) :- split_states(min,State,CStates,States1,States2) , !.
split_states(max,state(_,V,_,_,_),[state(P,V1,R,P1,P2)|CStates],[state(P,V1,R,P1,P2)|States1],States2) :- V < V1 ,
                                                                                                          split_states(max,state(_,V,_,_,_),CStates,States1,States2) , !.
split_states(max,State,[CState|CStates],States1,[CState|States2]) :- split_states(max,State,CStates,States1,States2) , !.

range_1(0,[]) :- !.
range_1(1,[]) :- !.
range_1(N,[1|L]) :- range_2(N,[1|L]) , !.
range_2(N,[A,B|T1]) :- B is A+1 ,
                       N > B ,
                       range_2(N,[B|T1]) , !.
range_2(_,[_]).

% other_player(+Player,-OtherPlayer)
% Player - The current player
% OtherPlayer - The other player
other_player(ai1,ai2).
other_player(ai2,ai1).
other_player(player,ai).
other_player(ai,player).
other_player(player1,player2).
other_player(player2,player1).

% TESTS:
test(min_0,SortedStates) :- sort_states(min,[state(min,1,1,1,1)],SortedStates-[]).
test(max_0,SortedStates) :- sort_states(max,[state(max,1,1,1,1)],SortedStates-[]).
test(min_1,SortedStates) :- sort_states(min,[state(min,1,1,1,1),state(min,2,2,2,2)],SortedStates-[]).
test(max_1,SortedStates) :- sort_states(max,[state(max,1,1,1,1),state(max,2,2,2,2)],SortedStates-[]).
test(min_2,SortedStates) :- sort_states(min,[state(min,3,3,3,3),state(min,1,1,1,1),state(min,2,2,2,2)],SortedStates-[]).
test(max_2,SortedStates) :- sort_states(max,[state(max,3,3,3,3),state(max,1,1,1,1),state(max,2,2,2,2)],SortedStates-[]).

test(minmax,NState) :- start(State) , findall(NState,s(ai,State,NState),NStates) , minmax(NStates,NState).
