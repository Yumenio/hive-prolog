:- module(pillbug, [pillbug_move/6, pillbug_path/3, pillbug_special/7]).
:- use_module(queen).
:- use_module(utils).

pillbug_move(Hex1, X, Y, Player, Opponent, Player_R):-
    queen_move(Hex1, X, Y, Player, Opponent, Player_R).

pillbug_path(Hex, OnGameCells, Path):-
    queen_path(Hex, OnGameCells, Path).

pillbug_special(PillbugHex, MovingHex, X, Y, Player, Opponent, Player_R):-
    onGameCells(Player, Opponent, OnGameCells),
    not(occupied(X, Y, OnGameCells)),
    get_all(MovingHex, T, MVRow, MVCol, Color, _, _, _),
    can_move(MovingHex, X, Y, OnGameCells), !,
    pillbug_can_carry(PillbugHex, [X, Y], OnGameCells),
    pillbug_can_carry(PillbugHex, [MVRow, MVCol], OnGameCells),
    new_hex(T, X, Y, Color, 0, 1, 2, NewHex),
    find_hex(MovingHex, Player, 0, Pos),
    replace_nth0(Player, Pos, _, NewHex, Player_R).

pillbug_can_carry(PillbugHex, [X, Y], OnGameCells):-
    get_all(PillbugHex, _, Row, Col, _, Height, _, Blocked),
    find_hex([X, Y], OnGameCells, CarriedHex), get_height(CarriedHex, 0),
    maplist(get_coordinates, OnGameCells, OnGameCellsCoor),
    Height is 0, % the hex being moved cannot be part of a stack of pieces
    Blocked is 0, % the pillbug cannot move the last cell the opponent moved
    findall([X1, Y1], onGame_adjacents(X1, Y1, OnGameCellsCoor, Row, Col), Adj1),
    findall([X2, Y2], onGame_adjacents(X2, Y2, OnGameCellsCoor, X, Y), Adj2),
    intersection(Adj1, Adj2, CommonAdjs),
    not(two_common_of_height_two(CommonAdjs, OnGameCells, [])).