:- use_module(board).
:- use_module(utils).
:- use_module(queen).
:- use_module(beetle).
:- use_module(ant).
:- use_module(spider).
:- use_module(pillbug).
:- use_module(ladybug).
:- use_module(dfs).

first_placed(Player1,Player1_R,Hex):-
    read_line_to_string(user_input,Raw_input),
    split_string(Raw_input,"\s","\s",Input),
    (length(Input,L), L is 3,
    parse_input_place(Raw_input,Type,Row,Col),
    new_hex(Type,Row,Col,1,0,1,0,Hex),
    find_free_bug(Type,Player1,0,Pos),
    replace_nth0(Player1,Pos,_,Hex,Player1_R)
    );
    (write("You did something wrong, try again\n"),
    first_placed(Player1,Player1_R,Hex)).

second_placed(Hex, Player2, Player2_R):-
    read_line_to_string(user_input, Raw_input),
    split_string(Raw_input,"\s","\s",Input),
    length(Input, L),
    ((L is 3, parse_input_place(Raw_input, Type, Row, Col),
    new_hex(Type, Row, Col, 2, 0, 1, 0, Hex_), adjacents(Hex, Hex_),
    find_free_bug(Type, Player2, 0, Pos),
    replace_nth0(Player2, Pos, _, Hex_, Player2_R) );
    (write("You did something wrong, try again\n"),
    second_placed(Hex, Player2, Player2_R))
    ).

first_two_places(Player1,Player2,Player1_R,Player2_R):-
    write("First turn:\n"),
    first_placed(Player1,Player1_R,Hex),
    write("Second turn:\n"),
    second_placed(Hex,Player2,Player2_R).

init_game():-
    players(Player1,Player2),
    first_two_places(Player1,Player2,Player1_R,Player2_R),
    game(Player1_R,Player2_R, 2).

game(Player1,Player2, Turn):-
    write("Turn Player1:\n"),
    turn_player1(Turn, Player1, Player2, NewPlayer11, NewPlayer21),
    unblock(NewPlayer11, UNewPlayer11), unblock(NewPlayer21, UNewPlayer21),
    show_board(UNewPlayer11, UNewPlayer21),

    write("Turn Player2:\n"),
    turn_player2(Turn, UNewPlayer11, UNewPlayer21, NewPlayer22, NewPlayer12),
    unblock(NewPlayer12, UNewPlayer12), unblock(NewPlayer22, UNewPlayer22),
    show_board(UNewPlayer12, UNewPlayer22),
    
    successor(Turn, Turn1),
    game(UNewPlayer12, UNewPlayer22, Turn1).
% mover para board la conversion de las celdas y tal
show_board(Player_1, Player_2):-
    include(is_on_game(), Player_1, Player1),
    include(is_on_game(), Player_2, Player2),
    append(Player1, Player2, OnGameCells),
    %get_converted_cells(OnGameCells, Converted),

    board(OnGameCells, Board),
    write(Board).

turn_player1(Turn, Player1, Player2, NewPlayer1, NewPlayer2):-
    read_line_to_string(user_input, Raw_input),
    split_string(Raw_input,"\s","\s",Input),
    ( % caso prueba
    (length(Input,L1), L1 is 3,
    parse_input_place(Raw_input, Type, Row, Col),
    Type = "test",
    test([Row, Col], Player1, Player2),
    NewPlayer1 = Player1, NewPlayer2 = Player2);
    
    % caso poner ficha
    (length(Input,L1), L1 is 3,
    parse_input_place(Raw_input, Type, Row, Col),
    place_hex(Turn, Type, Row, Col, 1, Player1, Player2, NewPlayer1), NewPlayer2 = Player2 );
    
    
    % caso mover ficha
    (length(Input,L2), L2 is 4,
    parse_input_move(Raw_input, R1, C1, R2, C2),
    move_hex(R1, C1, R2, C2, Player1, Player2, NewPlayer1), NewPlayer2 = Player2
    );
    
    % caso pillbug special
    (length(Input, L3), L3 is 6,
    parse_input_special(Raw_input, PillRow, PillCol, HexOriginRow, HexOriginCol, HexDestRow, HexDestCol),
    onGameCells(Player1, Player2, OnGameCells),
    find_hex([PillRow, PillCol], OnGameCells, PillbugHex),
    get_type(PillbugHex, PillbugType), PillbugType = "pillbug",
    check_color(PillbugHex, Player1),
    find_hex([HexOriginRow, HexOriginCol], OnGameCells, MovingHex),
    get_color(MovingHex, MovingHexColor),
    (
        ( MovingHexColor is 2, NewPlayer1 = Player1, pillbug_special(PillbugHex, MovingHex, HexDestRow, HexDestCol, Player2, Player1, NewPlayer2));
        ( MovingHexColor is 1, NewPlayer2 = Player2, pillbug_special(PillbugHex, MovingHex, HexDestRow, HexDestCol, Player1, Player2, NewPlayer1))
    )
        
    );
    % caso no válido
    (write("\nInvalid input, please try again\n"),
    turn_player1(Turn, Player1, Player2, NewPlayer1, NewPlayer2))
    ).

turn_player2(Turn, Player1, Player2, NewPlayer2, NewPlayer1):-
    read_line_to_string(user_input, Raw_input),
    split_string(Raw_input,"\s","\s",Input),
    ( % caso prueba
    (length(Input,L1), L1 is 1,
    Raw_input = "test",
    test(Player2, Player1),
    NewPlayer1 = Player1, NewPlayer2 = Player2);
    
    % caso poner ficha
    (length(Input,L1), L1 is 3,
    parse_input_place(Raw_input,Type,Row,Col),
    place_hex(Turn, Type,Row,Col,2,Player1,Player2,NewPlayer2), NewPlayer1 = Player1 );
    
    % caso mover ficha
    (length(Input,L2), L2 is 4,
    parse_input_move(Raw_input,R1,C1,R2,C2),
    move_hex(R1, C1, R2, C2, Player2, Player1, NewPlayer2), NewPlayer1 = Player1
    );

    % caso pillbug special
    (length(Input, L3), L3 is 6,
    parse_input_special(Raw_input, PillRow, PillCol, HexOriginRow, HexOriginCol, HexDestRow, HexDestCol),
    onGameCells(Player2, Player1, OnGameCells),
    find_hex([PillRow, PillCol], OnGameCells, PillbugHex),
    get_type(PillbugHex, PillbugType), PillbugType = "pillbug",
    check_color(PillbugHex, Player2),
    find_hex([HexOriginRow, HexOriginCol], OnGameCells, MovingHex),
    get_color(MovingHex, MovingHexColor),
    (
        ( MovingHexColor is 2, pillbug_special(PillbugHex, MovingHex, HexDestRow, HexDestCol, Player2, Player1, NewPlayer2));
        ( MovingHexColor is 1, pillbug_special(PillbugHex, MovingHex, HexDestRow, HexDestCol, Player1, Player2, NewPlayer1))
    )
        
    );

    % caso no válido
    (write("\nInvalid input, please try again\n"),
    turn_player2(Turn, Player1, Player2, NewPlayer2, NewPlayer1))
    ).

%---------------------- Moves ----------------------

move_hex(X, Y, X1, Y1, Player, Opponent, Player_R):-
    onGameCells(Player, Opponent, OnGameCells),
    find_hex([X, Y], OnGameCells, Hex),
    check_color(Hex, Player),
    get_type(Hex, T),
    ((T = "queen",  queen_move(Hex, X1, Y1, Player, Opponent, Player_R));
    (T = "ant",       ant_move(Hex, X1, Y1, Player, Opponent, Player_R));
    (T = "grasshoper", grasshoper_move(Hex, X1, Y1, Player, Opponent, Player_R));
    (T = "beetle", beetle_move(Hex, X1, Y1, Player, Opponent, Player_R));
    (T = "ladybug", ladybug_move(Hex, X1, Y1, Player, Opponent, Player_R));
    (T = "pillbug", pillbug_move(Hex, X1, Y1, Player, Opponent, Player_R));
    (T = "mosquito", mosquito_move(Hex, X1, Y1, Player, Opponent, Player_R));
    (T = "spider", spider_move(Hex, X1, Y1, Player, Opponent, Player_R))).

can_move(Hex1, X1, Y1, OnGameCells):-
    length(OnGameCells, L),
    get_all(Hex1, T, X, Y, C, _, _, B), B is 0,
    queen_on_game(OnGameCells, C),
    neighbours(Hex1, OnGameCells, Nbs), !,
    nth0(0, Nbs, Nb),
    get_all(Nb, _, Nb_x, Nb_y, _, _, _, _),
    new_hex(T, X, Y, C, 0, 0, 0, New_Hex),
    find_hex(Hex1, OnGameCells, 0, Pos),
    replace_nth0(OnGameCells, Pos, _, New_Hex, OG),
    % onGameSingle(OG, OGC),
    include(is_on_game(), OG, OGC),
    maplist(get_coordinates, OGC, OGCoor),
    cc_bfs( [Nb_x, Nb_y] , OGCoor, CC),
    length(CC, CCNodes), CCNodes is L-1,
    have_adjacent(X1, Y1, OnGameCells). 

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



mosquito_move(Hex1, X, Y, Player, Opponent, Player_R):-
    onGameCells(Player, Opponent, OnGameCells),
    boku_no_adj(Hex1, OnGameCells, Adj), get_type(Adj, T),
    mosquito_move_aux(Hex1, T, X, Y, Player, Opponent, Player_R).
    
mosquito_move_aux(Hex1, "queen", X, Y, Player, Opponent, Player_R):-
    queen_move(Hex1, X, Y, Player, Opponent, Player_R).
mosquito_move_aux(Hex1, "beetle", X, Y, Player, Opponent, Player_R):-
    beetle_move(Hex1, X, Y, Player, Opponent, Player_R).
mosquito_move_aux(Hex1, "grasshoper", X, Y, Player, Opponent, Player_R):-
    grasshoper_move(Hex1, X, Y, Player, Opponent, Player_R).
mosquito_move_aux(Hex1, "spider", X, Y, Player, Opponent, Player_R):-
    spider_move(Hex1, X, Y, Player, Opponent, Player_R).
mosquito_move_aux(Hex1, "ant", X, Y, Player, Opponent, Player_R):-
    ant_move(Hex1, X, Y, Player, Opponent, Player_R).
mosquito_move_aux(Hex1, "ladybug", X, Y, Player, Opponent, Player_R):-
    ladybug_move(Hex1, X, Y, Player, Opponent, Player_R).
mosquito_move_aux(Hex1, "pillbug", X, Y, Player, Opponent, Player_R):-
    pillbug_move(Hex1, X, Y, Player, Opponent, Player_R).

mosquito_path(Hex, OnGameCells, Path):-
    freedom_to_move(Hex, OnGameCells), !,
    boku_no_adj(Hex, OnGameCells, Adj), get_type(Adj, T),
    mosquito_path_aux(Hex, T, OnGameCells, Path).

mosquito_path_aux(Hex, "queen", OnGameCells, Path):-
    queen_path(Hex, OnGameCells, Path).
mosquito_path_aux(Hex, "beetle", OnGameCells, Path):-
    beetle_path(Hex, OnGameCells, Path).
mosquito_path_aux(Hex, "grasshoper", OnGameCells, Path):-
    grasshoper_path(Hex, OnGameCells, Path).

find_grasshoper_path(Hex, OnGameCells, Path):-
    get_row(Hex, Row), get_col(Hex, Col),
    straight_line(Row, Col, 1, 0, OnGameCells, [], R10),
    maplist(get_coordinates, R10, Path),
    length(Path, L), L > 2.

find_grasshoper_path(Hex, OnGameCells, Path):-
    get_row(Hex, Row), get_col(Hex, Col),
    straight_line(Row, Col, 0, 1, OnGameCells, [], R01),
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

straight_line(Row, Col, _, _, OnGameCells, Acc, R):-
    not(occupied(Row, Col, OnGameCells)),
    append(Acc, [[Row,Col]], R).

straight_line(Row, Col, DirRow, DirCol, OnGameCells, Acc, R):-
    occupied(Row, Col, OnGameCells),
    find_hex([Row, Col], OnGameCells, Hex),
    append(Acc, [Hex], Acc1),
    sum([Row, DirRow], Row1), sum([Col, DirCol], Col1),
    straight_line(Row1, Col1, DirRow, DirCol, OnGameCells, Acc1, R).



test([X, Y], Player, Opponent):-
    find_hex([X,Y], Player, Hex),
    onGameCells(Player, Opponent, OnGameCells),
    findall(Path, mosquito_path(Hex, OnGameCells, Path), AllAntPaths),
    list_to_set(AllAntPaths, AllAntPathsSingle),
    write_all(AllAntPathsSingle).
