:- module(ant, [ant_move/6, ant_path/3]).
:- use_module(utils).

ant_move(Hex1, X, Y, Player, Opponent, Player_R):-
    onGameCells(Player, Opponent, OnGameCells),
    not(occupied(X, Y, OnGameCells)),
    get_all(Hex1, T, Row, Col, C, _, _, _),
    can_move(Hex1, X, Y, OnGameCells),!,
    new_hex(T, X, Y, C, 0, 1, 2, Hex2),
    delete(OnGameCells, Hex1, OnGameCellsAux),
    empty_neighbours(OnGameCellsAux, [], OnGameCellsAux, Free_Cells),
    single_dfs([Row, Col], [X, Y], Free_Cells, OnGameCells, Path),
    valid_path_end(Path, [X, Y]),
    find_hex(Hex1, Player, 0, Pos),
    replace_nth0(Player, Pos, _, Hex2, Player_R).

ant_path(Hex, OnGameCells, Path):-
    get_all(Hex, _, Row, Col, _, _, _, _),
    freedom_to_move(Hex, OnGameCells),
    delete(OnGameCells, Hex, OnGameCellsAux),
    vecinos_void(OnGameCellsAux, [], OnGameCellsAux, Free_Cells),!,
    full_dfs([Row, Col], Free_Cells, OnGameCells, Path).

ant_path(_, _, []).