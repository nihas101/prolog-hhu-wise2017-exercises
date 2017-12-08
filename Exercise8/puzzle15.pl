% Aufgabe 1 (15-Puzzle)

% DFS - Depth First Search
dfs(State,FState) :- dfs_(State,[],FState).
dfs_(State,_,State) :- goal(State).
dfs_(State,Hist,FState) :-  s(State,NState) ,
                            \+member(NState,Hist) ,
                            dfs_(NState,[NState|Hist],FState).

% IDS - Iterative Deepening Search
ids(State,FState) :-  it(N) ,
                      ids_(N,State,[],FState).
ids_(_,State,_,State) :- goal(State).
ids_(N,State,Hist,FState) :-  N > 0 , N1 is N-1 ,
                              s(State,NState) ,
                              \+member(NState,Hist) ,
                              ids_(N1,NState,[NState|Hist],FState).

% BFS - Breadth First Search
bfs(State,FState) :- empty_queue(EQueue) , enqueue([State],EQueue,Queue) , bfs_(Queue,FState).
bfs_(Queue,FState) :- dequeue(State,Queue,NQueue) ,
                      (goal(State) , FState = State ;
                      findall(Succ,s(State,Succ),Succs) ,
                      enqueue(Succs,NQueue,NNQueue) ,
                      bfs_(NNQueue,FState)).

% BFS A* - Breadth First Search with A*
bfs_a(State,FState) :-  empty_queue(EQueue) ,
                        manhattan_distance(State,H) ,
                        enqueue([(State,H)],EQueue,Queue) ,
                        bfs_a_(Queue,FState).
bfs_a_(Queue,FState) :- dequeue((State,_),Queue,NQueue) ,
                        (goal(State) , FState = State ;
                        findall((Succ,H1),(s(State,Succ),manhattan_distance(State,H1)),Succs) ,
                        enqueue(Succs,NQueue,NNQueue) ,
                        sort_queue(NNQueue,NNSQueue) ,
                        bfs_a_(NNSQueue,FState)).

% Goal
goal(c([x,1,2,3],[4,5,6,7],[8,9,10,11],[12,13,14,15])).

% Successor-function
s(c(P1,P2,P3,P4),c(NP1,NP2,P3,P4)) :- holds_x(P1) ,
                                    (switch(P1,NP1) , NP2 = P2 ;
                                    switch(P1,P2,NP1,NP2) , !).
s(c(P1,P2,P3,P4),c(NP1,NP2,NP3,P4)) :- holds_x(P2),
                                    (switch(P2,NP2) , NP1 = P1 , NP3 = P3 ;
                                    switch(P2,P1,NP2,NP1) , NP3 = P3 ;
                                    switch(P2,P3,NP2,NP3) , NP1 = P1 , !).
s(c(P1,P2,P3,P4),c(P1,NP2,NP3,NP4)) :- holds_x(P3),
                                    (switch(P3,NP3) , NP2 = P2 , NP4 = P4 ;
                                    switch(P3,P2,NP3,NP2) , NP4 = P4 ;
                                    switch(P3,P4,NP3,NP4) , NP2 = P2 , !).
s(c(P1,P2,P3,P4),c(P1,P2,NP3,NP4)) :- holds_x(P4) ,
                                    (switch(P4,NP4) , NP3 = P3 ;
                                    switch(P4,P3,NP4,NP3) , !).

switch([H,x|T],[x,H|T]).
switch([x,H|T],[H,x|T]).
switch([H|T],[H|T1]) :- switch(T,T1).

switch([x|T],[H|T1],[H|T],[x|T1]) :- !.
switch([H|T],[H1|T1],[H|T2],[H1|T3]) :- switch(T,T1,T2,T3) , !.

holds_x([x,_,_,_]) :- !.
holds_x([_,x,_,_]) :- !.
holds_x([_,_,x,_]) :- !.
holds_x([_,_,_,x]) :- !.

% manhattan_distance
manhattan_distance(Puzzle,MHD) :- manhattan_distance(Puzzle,0,MHD) , !.
manhattan_distance(c([],[],[],[]),_,0) :- !.
manhattan_distance(c([H|T],[H1|T1],[H2|T2],[H3|T3]),X,MHD) :- X1 is X+1,
                                                              manhattan_distance(c(T,T1,T2,T3),X1,MHD1) ,
                                                              to_mhd(H,(HX,HY)) , %MHD2 is abs(X-HX) + HY ,
                                                              to_mhd(H1,(H1X,H1Y)) , %MHD3 is abs(X-H1X) + abs(1-H1Y) ,
                                                              to_mhd(H2,(H2X,H2Y)) , %MHD4 is abs(X-H2X) + abs(2-H2Y) ,
                                                              to_mhd(H3,(H3X,H3Y)) , %MHD5 is abs(X-H3X) + abs(3-H3Y) ,
                                                              MHD is MHD1 + abs(X-HX) + HY + abs(X-H1X) + abs(1-H1Y) + abs(X-H2X) + abs(2-H2Y) + abs(X-H3X) + abs(3-H3Y) , !.

to_mhd(x,(0,0)) :- !.
to_mhd(E,(X,Y)) :- X is E mod 4 , Y is E // 4 , !.


% Queues
empty_queue(T-T) :- !.

enqueue(List,Queue-T,Queue-T1) :- enqueue_(List,T-T1) , !.
enqueue_([E],[E|T]-T) :- !.
enqueue_([H|T],[H|T1]-T2) :- enqueue_(T,T1-T2) , !.

dequeue(E,[E|Queue]-T,Queue-T) :- !.
dequeue([H|T1],Queue-T,[H|T2]-T) :- dequeue(T1,Queue-T,T2-T) , !.

sort_queue(Queue-_,SQueue-T) :- sort_queue_(Queue,SQueue-T) , !.

sort_queue_([],T-T) :- !.
sort_queue_([H|T1],SQ1-T) :- split_queue(H,T1,Q1,Q2) ,
                            sort_queue_(Q1,SQ1-[H|T2]) ,
                            sort_queue_0(Q2,T2-T) , !.

sort_queue_0(V,_) :- var(V) , !.
sort_queue_0([H|T1],SQ1-T) :- split_queue(H,T1,Q1,Q2) ,
                             sort_queue_(Q1,SQ1-[H|T2]) ,
                             sort_queue_0(Q2,T2-T) , !.

split_queue(_,T,[],T) :- var(T) , !.
split_queue(_,[],[],[]) :- !.
split_queue((_,H),[(Succ,H1)|T1],[(Succ,H1)|T],Q2) :- H1 < H , split_queue((_,H),T1,T,Q2) , !.
split_queue((_,H),[(Succ,H1)|T1],Q1,[(Succ,H1)|T]) :- H =< H1 , split_queue((_,H),T1,Q1,T) , !.

% Iterates through the numbers 0...N
it(0).
it(N) :- it(N1) , N is N1+1.

% Comparisons:
test :- t(N) , print(N) , nl , test(N).

t('dfs_goal').
t('dfs_1').
%t('dfs_2').
t('ids_goal').
t('ids_1').
%t('ids_2').
t('bfs_goal').
t('bfs_1').
%t('bfs_2').
t('bfs_a_goal').
t('bfs_a_1').
t('bfs_a_2').
t('bfs_a_3').


% 2 inferences
test('dfs_goal') :- time(dfs(c([x,1,2,3],[4,5,6,7],[8,9,10,11],[12,13,14,15]),FState)) , FState = c([x,1,2,3],[4,5,6,7],[8,9,10,11],[12,13,14,15]) , !.
% 3 inferences
test('ids_goal') :- time(ids(c([x,1,2,3],[4,5,6,7],[8,9,10,11],[12,13,14,15]),FState)) , FState = c([x,1,2,3],[4,5,6,7],[8,9,10,11],[12,13,14,15]) , !.
% 6 inference
test('bfs_goal') :- time(bfs(c([x,1,2,3],[4,5,6,7],[8,9,10,11],[12,13,14,15]),FState)) , FState = c([x,1,2,3],[4,5,6,7],[8,9,10,11],[12,13,14,15]) , !.
% 62 inferences
test('bfs_a_goal') :- time(bfs_a(c([x,1,2,3],[4,5,6,7],[8,9,10,11],[12,13,14,15]),FState)) , FState = c([x,1,2,3],[4,5,6,7],[8,9,10,11],[12,13,14,15]) , !.

% 9 inferences
test('dfs_1') :- time(dfs(c([1,x,2,3],[4,5,6,7],[8,9,10,11],[12,13,14,15]),FState)) , FState = c([x,1,2,3],[4,5,6,7],[8,9,10,11],[12,13,14,15]) , !.
% 16 inferences
test('ids_1') :- time(ids(c([1,x,2,3],[4,5,6,7],[8,9,10,11],[12,13,14,15]),FState)) , FState = c([x,1,2,3],[4,5,6,7],[8,9,10,11],[12,13,14,15]) , !.
% 38 inferences
test('bfs_1') :- time(bfs(c([1,x,2,3],[4,5,6,7],[8,9,10,11],[12,13,14,15]),FState)) , FState = c([x,1,2,3],[4,5,6,7],[8,9,10,11],[12,13,14,15]) , !.
% 330 inferences
test('bfs_a_1') :- time(bfs_a(c([1,x,2,3],[4,5,6,7],[8,9,10,11],[12,13,14,15]),FState)) , FState = c([x,1,2,3],[4,5,6,7],[8,9,10,11],[12,13,14,15]) , !.

% 551,722,975 inferences and DNF
test('dfs_2') :- time(dfs(c([1,2,3,4],[x,5,6,7],[8,9,10,11],[12,13,14,15]),FState)) , FState = c([x,1,2,3],[4,5,6,7],[8,9,10,11],[12,13,14,15]) , !.
% 335,779,267 inferences and DNF
test('ids_2') :- time(ids(c([1,2,3,4],[x,5,6,7],[8,9,10,11],[12,13,14,15]),FState)) , FState = c([x,1,2,3],[4,5,6,7],[8,9,10,11],[12,13,14,15]) , !.
% 11,157,107 inferences and DNF
test('bfs_2') :- time(bfs(c([1,2,3,4],[x,5,6,7],[8,9,10,11],[12,13,14,15]),FState)) , FState = c([x,1,2,3],[4,5,6,7],[8,9,10,11],[12,13,14,15]) , !.
% 1,072 inference
test('bfs_a_2') :- time(bfs_a(c([1,2,3,4],[x,5,6,7],[8,9,10,11],[12,13,14,15]),FState)) , FState = c([x,1,2,3],[4,5,6,7],[8,9,10,11],[12,13,14,15]) , !.

% 1,074 inferences
test('bfs_a_3') :- time(bfs_a(c([1,5,2,4],[3,7,6,8],[10,9,11,12],[13,x,14,15]),FState)) , FState = c([x,1,2,3],[4,5,6,7],[8,9,10,11],[12,13,14,15]) , !.
