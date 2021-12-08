grasshoper_move(Hex1, X, Y, Player, Opponent, Player_R):-
    onGameCells(Player, Opponent, OnGameCells),
    not(occupied(X, Y, OnGameCells)),
    get_all(Hex1, T, _, _, C, _, _, _),
    can_move(Hex1, X, Y, OnGameCells), !,
    new_hex(T, X, Y, C, 0, 1, 2, Hex2),
    find_grasshoper_path(Hex1, OnGameCells, Path),
    valid_paths(X, Y, Path, ValidPath),
    length(ValidPath, LVP),
    LVP > 0,
    find_hex(Hex1, Player, 0, Pos),
    replace_nth0(Player, Pos, _, Hex2, Player_R).

grasshoper_path(Hex, OnGameCells, Path):-
    freedom_to_move(Hex, OnGameCells), !,
    find_grasshoper_path(Hex, OnGameCells, Path).


find_grasshoper_path(Hex, OnGameCells, Path):-
    get_row(Hex, Row), get_col(Hex, Col),
    straight_line(Row, Col, 1, 0, OnGameCells,[], R10),
    maplist(get_coordinates, R10, Path),
    length(Path, L), L > 2.

find_grasshoper_path(Hex, OnGameCells, Path):-
    get_row(Hex, Row), get_col(Hex, Col),
    straight_line(Row, Col, 0, 1, OnGameCells,[], R01),
    maplist(get_coordinates, R01, Path),
    length(Path, L), L > 2.

find_grasshoper_path(Hex, OnGameCells, Path):-
    get_row(Hex, Row), get_col(Hex, Col),
    straight_line(Row, Col, -1, 1, OnGameCells, [], R_11),
    maplist(get_coordinates, R_11, Path),
    length(Path, L), L > 2.
    
find_grasshoper_path(Hex, OnGameCells, Path):-
    get_row(Hex, Row), get_col(Hex, Col),
    straight_line(Row, Col, -1, 0, OnGameCells, [], R_10),
    maplist(get_coordinates, R_10, Path),
    length(Path, L), L > 2.
    
find_grasshoper_path(Hex, OnGameCells, Path):-
    get_row(Hex, Row), get_col(Hex, Col),    
    straight_line(Row, Col, 0, -1, OnGameCells, [], R0_1),
    maplist(get_coordinates, R0_1, Path),
    length(Path, L), L > 2.
    

find_grasshoper_path(Hex, OnGameCells, Path):-
    get_row(Hex, Row), get_col(Hex, Col),
    straight_line(Row, Col, 1, -1, OnGameCells, [], R1_1),
    maplist(get_coordinates, R1_1, Path),
    length(Path, L), L > 2.
    
    
% find_grasshoper_paths(Hex, OnGameCells, Paths):-
%     get_row(Hex, Row), get_col(Hex, Col),
%     straight_line(Row, Col, 1, 0, OnGameCells,[], R10),
%     maplist(get_coordinates, R10, R10Coor),
%     straight_line(Row, Col, 0, 1, OnGameCells,[], R01),
%     maplist(get_coordinates, R01, R01Coor),
%     straight_line(Row, Col, -1, 1, OnGameCells, [], R_11),
%     maplist(get_coordinates, R_11, R_11Coor),
%     straight_line(Row, Col, -1, 0, OnGameCells, [], R_10),
%     maplist(get_coordinates, R_10, R_10Coor),
%     straight_line(Row, Col, 0, -1, OnGameCells, [], R0_1),
%     maplist(get_coordinates, R0_1, R0_1Coor),
%     straight_line(Row, Col, 1, -1, OnGameCells, [], R1_1),
%     maplist(get_coordinates, R1_1, R1_1Coor),
%     append([[R10Coor],[R01Coor],[R_11Coor],[R_10Coor],[R0_1Coor],[R1_1Coor]], AllPaths),
%     include(path_greater_than_2(), AllPaths, Paths).
straight_line(Row, Col, _, _, OnGameCells, Acc, R):-
    not(occupied(Row, Col, OnGameCells)),
    append(Acc, [[Row,Col]], R).

straight_line(Row, Col, DirRow, DirCol, OnGameCells, Acc, R):-
    occupied(Row, Col, OnGameCells),
    find_hex([Row, Col], OnGameCells, Hex),
    append(Acc, [Hex], Acc1),
    sum([Row, DirRow], Row1), sum(Col, DirCol, Col1),
    straight_line(Row1, Col1, DirRow, DirCol, OnGameCells, Acc1, R).