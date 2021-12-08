:- module(queen, [queen_move/6, queen_path/3]).
:- use_module(utils).

queen_move(Hex1, X, Y, Player, Opponent, Player_R):-
    onGameCells(Player, Opponent, OnGameCells),
    not(occupied(X, Y, OnGameCells)),
    get_color(Hex1, C), get_type(Hex1, T),
    new_hex(T, X, Y, C, 0, 1, 2, Hex2),
    adjacents(Hex1, Hex2),
    can_move(Hex1, X, Y, OnGameCells),
    find_hex(Hex1, Player, 0, Pos),
    replace_nth0(Player, Pos, _, Hex2, Player_R).  

queen_path(Hex, OnGameCells, Path):-
    freedom_to_move(Hex, OnGameCells), !,
    get_row(Hex, Row), get_col(Hex, Col),
    adjacents(AdjX, AdjY, Row, Col),
    not(occupied(AdjX, AdjY, OnGameCells)),
    delete(OnGameCells, Hex, OnGameCellsTemp),
    have_adjacent(AdjX, AdjY, OnGameCellsTemp),
    Path = [[Row, Col], [AdjX, AdjY]].

queen_path(_, _, []).