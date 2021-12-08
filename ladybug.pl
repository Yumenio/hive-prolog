
ladybug_move(Hex1, X, Y, Player, Opponent, Player_R):-
    onGameCells(Player, Opponent, OnGameCells),
    not(occupied(X, Y, OnGameCells)),
    get_all(Hex1, T, Row, Col, C, _, _, _),
    can_move(Hex1, X, Y, OnGameCells),
    new_hex(T, X, Y, C, 0, 1, 2, Hex2),
    delete(OnGameCells, Hex1, OnGameCellsAux),
    empty_neighbours(OnGameCells, [], OnGameCells, Free_Cells),
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
    vecinos_void(OnGameCellsAux, [], OnGameCellsAux, Free_Cells), !,
    maplist(get_coordinates, OnGameCellsAux, OnGameCellsAuxCoordinates),
    append(Free_Cells, OnGameCellsAuxCoordinates, AllLadybugPathCells),
    ladybug_dfs([Row, Col], 3, AllLadybugPathCells, Path),
    valid_ladybug_path(Path, OnGameCellsAuxCoordinates, Free_Cells).

ladybug_path(_, _, []).