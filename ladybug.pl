
:- module(ladybug, [ladybug_move/6, ladybug_path/3]).
:- use_module(utils).
:- use_module(dfs).

ladybug_move(Hex1, X, Y, Player, Opponent, Player_R):-
    onGameCells(Player, Opponent, OnGameCells),
    not(occupied(X, Y, OnGameCells)),
    get_all(Hex1, T, Row, Col, C, _, _, _),
    can_move(Hex1, X, Y, OnGameCells),
    new_hex(T, X, Y, C, 0, 1, 2, Hex2),

    delete(OnGameCells, Hex1, OnGameCellsAux),

    empty_neighbours(OnGameCells, [], OnGameCells, Free_Cells), !,
    maplist(get_coordinates, OnGameCellsAux, OnGameCellsAuxCoordinates),
    append(Free_Cells, OnGameCellsAuxCoordinates, AllLadybugPathCells),
    
    capped_dfs([Row, Col], [X, Y], AllLadybugPathCells, 3, Path),

    valid_ladybug_path(Path, OnGameCellsAuxCoordinates, Free_Cells),

    find_hex(Hex1, Player, 0, Pos),
    replace_nth0(Player, Pos, _, Hex2, Player_R).

ladybug_path(Hex, OnGameCells, Path):-
    get_all(Hex, _, Row, Col, _, _, _, _),
    freedom_to_move(Hex, OnGameCells),

    delete(OnGameCells, Hex, OnGameCellsAux),

    empty_neighbours(OnGameCellsAux, [], OnGameCellsAux, Free_Cells), !,
    maplist(get_coordinates, OnGameCellsAux, OnGameCellsAuxCoordinates),
    append(Free_Cells, OnGameCellsAuxCoordinates, AllLadybugPathCells),
    
    ladybug_dfs([Row, Col], 3, AllLadybugPathCells, Path),

    valid_ladybug_path(Path, OnGameCellsAuxCoordinates, Free_Cells).

ladybug_path(_, _, []).

valid_ladybug_path(Path, OnGameCellsCoordinates, HiveBorderCellCoordinates):-
    nth0(1, Path, SecondMove),
    nth0(2, Path, ThirdMove),
    nth0(3, Path, FourthMove),
    member(SecondMove, OnGameCellsCoordinates),
    member(ThirdMove, OnGameCellsCoordinates),
    member(FourthMove, HiveBorderCellCoordinates).

ladybug_dfs([X, Y], Length, Candidates, Solution):-
    get_ladybug_path([], [X, Y], Length, Candidates, RevSolution),
    reverse(RevSolution, Solution).

get_ladybug_path(Stack, [X, Y], Length, _, [[X, Y]|Stack]):-
    length(Stack, StackLength), StackLength is Length, printall(["Found", [[X,Y]|Stack]]).

get_ladybug_path(Stack, [X, Y], Length, Candidates, Path):-
    length(Stack, StackLength), StackLength < Length,
    boku_no_adj([X, Y], Candidates, Stack, Adj),
    get_ladybug_path([[X, Y]|Stack], Adj, Length, Candidates, Path).

find_ladybug_paths(OnGameCells, X, Y, Depth, Paths):-
    empty_neighbours(OnGameCells, [], OnGameCells, Free_Cells),
    maplist(get_coordinates, OnGameCells, OG),
    append(OG, Free_Cells, Board),
    findall(P, dfs_path(X, Y, Depth, Board, _, P), V1),
    list_to_set(V1, P1),
    reverse_all(P1, Paths).