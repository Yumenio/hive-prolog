:- module(game, [init_game/0]).
:- use_module(board).
:- use_module(utils).
:- use_module(queen).
:- use_module(beetle).
:- use_module(ant).
:- use_module(spider).
:- use_module(pillbug).
:- use_module(ladybug).
:- use_module(grasshoper).
:- use_module(mosquito).
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
    %onGameCells(UNewPlayer11, UNewPlayer21, OG1),
    %queen_count(UNewPlayer11, OG1, Count1), write("la reina blanca esta rodeada por: "), write(Count1), write("\n"),
    show_board(UNewPlayer11, UNewPlayer21),

    write("Turn Player2:\n"),
    turn_player2(Turn, UNewPlayer11, UNewPlayer21, NewPlayer22, NewPlayer12),
    unblock(NewPlayer12, UNewPlayer12), unblock(NewPlayer22, UNewPlayer22),
    %onGameCells(UNewPlayer12, UNewPlayer22, OG2),
    %write("asd\n"),
    %queen_count(UNewPlayer22, OG2, Count2), write("la reina negra esta rodeada por: "), write(Count2), write("\n"),
    
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
    split_string(Raw_input, "\s", "\s", Input),
    ( % caso prueba
    (length(Input, L1), L1 is 3,
    parse_input_place(Raw_input, Type, Row, Col),
    Type = "test",
    test([Row, Col], Player2, Player1),
    NewPlayer1 = Player1, NewPlayer2 = Player2);
    % caso poner ficha
    (length(Input, L1), L1 is 3,
    parse_input_place(Raw_input, Type, Row, Col),
    place_hex(Turn, Type, Row, Col, 2, Player1, Player2, NewPlayer2), NewPlayer1 = Player1 );
    % caso mover ficha
    (length(Input, L2), L2 is 4,
    parse_input_move(Raw_input, R1, C1, R2, C2),
    write_all([R1, C1, R2, C2]),
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

move_hex(X, Y, X1, Y1, Player, Opponent, Player_R):-
    onGameCells(Player, Opponent, OnGameCells),
    find_hex([X, Y], OnGameCells, Hex),
    write_all([Hex]),
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


test([_, _], Player, Oponent):-
    playerMovements(Player, Oponent, Allmovements), write_all(Allmovements).