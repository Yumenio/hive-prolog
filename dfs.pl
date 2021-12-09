
:- module(dfs, [single_dfs/5, length_dfs/5, capped_dfs/5, dfs_path/6, full_dfs/4]).
:- use_module(utils).

boku_no_neighbours(Visited, [X,Y], [M,N]):-
    not(member([M, N], Visited)), adjacents(X, Y, M, N).

ineighbours([X,Y], Candidates, Visited, Nbs):-
    include(boku_no_neighbours(Visited, [X,Y]), Candidates, Nbs).

%% dfs starting from a root 
dfs_path(X, Y, Deep, Cells, _, Result):-
    dfs_path([[X, Y]], Deep, Cells, [], Result1),
    list_to_set(Result1, Result).
%% Done, all visited
dfs_path(_, 0, _, Result, Result).
%% Skip elements that are already visited
dfs_path([Hex|Tail], Deep, Cells, Visited, Result):-
    member(Hex, Visited),
    dfs_path(Tail, Deep, Cells, Visited, Result).
%% add all adjacents
dfs_path([H|T], Deep, L, Visited, T1):-
    not(member(H, Visited)),
    nth0( 0, H, X1),
    nth0( 1, H, Y1),
    append(Visited, T, V1),
    % neighbours(X1, Y1, L, V1, Nbs),
    ineighbours([X1, Y1], L, V1, Nbs),
    % append(Nbs, T, ToVisit),
    predecessor(Deep, Deep1),
    dfs_path(Nbs, Deep1, L, [H|Visited], T1).

single_dfs([X, Y], Dest, Candidates, OnGameCells, Solution):-
    % maplist(get_coordinates, Candidates, MappedCandidates),
    single_path([], [X, Y], Dest, Candidates, OnGameCells, RevSolution),
    reverse(RevSolution, Solution).

single_path(Stack, [X, Y], [X, Y], _, _, [[X, Y]|Stack]). % : write("Found:\n"), write_all([[X, Y]|Stack]).

single_path(Stack, [X, Y], Dest, Candidates, OnGameCells, Sol):-
    boku_no_adj([X, Y], Candidates, Stack, Adj),
    reachable([X, Y], Adj, OnGameCells),
    single_path([[X, Y]|Stack], Adj, Dest, Candidates, OnGameCells, Sol).


capped_dfs([X, Y], Dest, Candidates, Cap, Solution):-
    capped_path([], [X, Y], Dest, Candidates, Cap, RevSolution),
    reverse(RevSolution, Solution).
    
capped_path(Stack, [X, Y], [X, Y], _, Cap, [[X, Y]|Stack]):- length(Stack, StackLength), StackLength is Cap.

capped_path(Stack, [X, Y], Dest, Candidates, Cap, Path):-
    length(Stack, PathLength), PathLength < Cap,
    boku_no_adj([X, Y], Candidates, Stack, Adj),
    capped_path([[X, Y]|Stack], Adj, Dest, Candidates, Cap, Path).

length_dfs([X, Y], Length, Candidates, OnGameCells, Solution):-
    length_path([], [X, Y], Length, Candidates, OnGameCells, RevSolution),
    reverse(RevSolution, Solution).

length_path(Stack, [X, Y], Length, _, _, [[X, Y]|Stack]):-
    length(Stack, StackLength), StackLength is Length.

length_path(Stack, [X, Y], Length, Candidates, OnGameCells, Path):-
    length(Stack, StackLength), StackLength < Length,
    boku_no_adj([X, Y], Candidates, Stack, Adj),
    reachable([X, Y], Adj, OnGameCells),
    length_path([[X, Y]|Stack], Adj, Length, Candidates, OnGameCells, Path).

full_dfs([X, Y], Candidates, OnGameCells, Solution):-
    any_path([], [X, Y], Candidates, OnGameCells, RevSolution),
    reverse(RevSolution, Solution).

any_path(Stack, [X, Y], _, _, [[X, Y]|Stack]):- length(Stack, L), L > 0. 

any_path(Stack, [X, Y], Candidates, OnGameCells, Path):-
    boku_no_adj([X, Y], Candidates, Stack, Adj),
    reachable([X, Y], Adj, OnGameCells),
    any_path([ [X, Y] | Stack], Adj, Candidates, OnGameCells, Path).