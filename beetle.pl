:- module(beetle, [beetle_move/6, beetle_path/3]).
:- use_module(utils).

beetle_move(Hex1, X, Y, Player, Opponent, Player_R):-
    onGameCells(Player, Opponent, OnGameCells),
    not(occupied(X, Y, OnGameCells)),
    get_color(Hex1, C), get_type(Hex1, T),
    new_hex(T, X, Y, C, 0, 1, 2, Hex2),
    adjacents(Hex1, Hex2),
    can_move(Hex1, X, Y, OnGameCells),
    delete(OnGameCells, Hex1, OnGameCellsTemp),
    have_adjacent(X, Y, OnGameCellsTemp),
    find_hex(Hex1, Player, 0, Pos),
    replace_nth0(Player, Pos, _, Hex2, Player_R).

beetle_move(Hex1, X, Y, Player, Opponent, Player_R):-
    onGameCells(Player, Opponent, OnGameCells),
    occupied(X, Y, OnGameCells),
    find_hex([X,Y],OnGameCells, OccupiedHex),
    adjacents(Hex1, OccupiedHex),
    get_color(Hex1, C), get_height(OccupiedHex, H), get_type(Hex1, T),
    successor(H, H1),
    new_hex(T, X, Y, C, H1, 1, 2, Hex2),
    can_move(Hex1, X, Y, OnGameCells), !,
    find_hex(Hex1, Player, 0, Pos),
    replace_nth0(Player, Pos, _, Hex2, Player_R).

beetle_path(Hex, OnGameCells, Path):-
    freedom_to_move(Hex, OnGameCells), !,
    get_row(Hex, Row), get_col(Hex, Col),
    adjacents(AdjX, AdjY, Row, Col),
    delete(OnGameCells, Hex, OnGameCellsTemp),
    have_adjacent(AdjX, AdjY, OnGameCellsTemp),
    Path = [[Row, Col], [AdjX, AdjY]].
    
beetle_path(_, _, []).