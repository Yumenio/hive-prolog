:- use_module(board).
:- use_module(utils).
:- use_module(queen).
% devuelve en Hex una celda en juego en las coordenadas X, Y. En caso de no existir devuelve falso.
find_all_at([X, Y], [Hex|Tail], Hex1):- 
    get_all(Hex, _, Row1, Col1, _, _, OG1,_),
    ( (Row1 is X, Col1 is Y, OG1 is 1, Hex1 = Hex);
    find_all_at([X, Y], Tail, Hex1)).

find_hex(Pos, L, Hex1):-
    findall(Hex, find_all_at(Pos, L, Hex), Hexs),
    length(Hexs, Len), Len > 0,
    nth0(0, Hexs, H),
    higher(Hexs, H, Hex1), !.

find_hex(_, [], _, -1):- false().
find_hex(Hex, [H|T], Index, Pos):-
    get_all(Hex, Type, Row, Col, Color, Height, _, _),
    get_all(H, Type1, Row1, Col1, Color1, Height1, O, _),
    (Type1 = Type, Row1 is Row, Col1 is Col, Color1 is Color, 
    Height1 is Height, O is 1, Pos is Index); 
    (Index1 is Index +1, find_hex(Hex, T, Index1, Pos)).

free_bug_place(_, _, []):- false().
free_bug_place(T, C, [hex(Type, _, _, Color, _, OnGame, _)|Tail]):-
    Color is C, Type = T, OnGame is 0 ; free_bug_place(T, C, Tail).

valid_place(Cell, Cells):- 
    % onGameSingle(Cells, OnGameCells),
    include(is_on_game, Cells, OnGameCells),
    neighbours(Cell, OnGameCells, Nbs), !,
    length(Nbs,L), L > 0,
    get_color(Cell, C1), all_same_color(C1, Nbs).

queen_on_game([hex(Type, _, _, Color, _, OnGame, _)|_], PlayerColor):-
    Type = "queen", Color = PlayerColor, OnGame is 1.
queen_on_game([_|T], PlayerColor):- queen_on_game(T, PlayerColor).

valid_state(Cells, Turn, Color, Type):-
    queen_on_game(Cells, Color); Turn < 4; (Turn is 4, Type = "queen").

find_free_bug(_, [], _, -1).
find_free_bug(T, [hex(Type, _, _, _, _, OnGame, _)|Tail], Index, Pos):-
    (Type = T, OnGame is 0, Pos is Index); 
    (Index1 is Index + 1, find_free_bug(T, Tail, Index1, Pos)).

% falta por refactorizar
% Cells es celdas en juegos de ambos players 
can_place_hex(Turn, Type, X, Y, Color, Cells):-
    % onGameSingle(Cells,OnGameCells),
    include(is_on_game(), Cells, OnGameCells),
    not(occupied(X, Y, OnGameCells)),
    free_bug_place(Type, Color, Cells), !,
    % new_hex(Type, X, Y, Color, _, 0, 0, Hex),
    % valid_place(Hex, Cells),
    valid_state(Cells, Turn, Color, Type).

place_hex(Turn, Type, X, Y, Color, Player1, Player2, Player_R):-
    append(Player1, Player2, Cells),
    can_place_hex(Turn, Type, X, Y, Color, Cells),
    new_hex(Type, X, Y, Color, 0, 1, 0, Hex),
    (Color is 1,
    find_free_bug(Type, Player1, 0, Pos),
    replace_nth0(Player1, Pos, _, Hex, Player_R)
    ;
    Color is 2,
    find_free_bug(Type, Player2, 0, Pos),
    replace_nth0(Player2, Pos, _, Hex, Player_R)
    ).

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

% DFS stuffs
neighbours(_, [], []).
neighbours(Hex, [Nb|Tail], Nbs):- 
    (adjacents(Hex, Nb), neighbours(Hex, Tail, Nbs_), 
    append([Nb], Nbs_, Nbs)); neighbours(Hex, Tail, Nbs).

%% dfs starting from a root 
dfs(Root, OnGameCells, Result):-
    dfs([Root], OnGameCells, [], Result).
%% Done, all visited
dfs([], _, Result, Result).
%% Skip elements that are already visited
dfs([Hex|Tail], OnGameCells, Visited, Result):-
    member(Hex, Visited),
    dfs(Tail, OnGameCells, Visited, Result).
%% add all adjacents
dfs([H|T], L, Visited, T1):-
    not(member(H, Visited)),
    neighbours(H, L, Nbs),
    append(Nbs, T, ToVisit),
    dfs(ToVisit, L, [H|Visited], T1).

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
    get_converted_cells(OnGameCells, Converted),

    board(Converted, Board),
    write(Board).

turn_player1(Turn, Player1, Player2, NewPlayer1, NewPlayer2):-
    read_line_to_string(user_input, Raw_input),
    split_string(Raw_input,"\s","\s",Input),
    (
    % caso poner ficha
    (length(Input,L1), L1 is 3,
    parse_input_place(Raw_input,Type,Row,Col),
    place_hex(Turn, Type,Row,Col,1,Player1,Player2,NewPlayer1), NewPlayer2 = Player2 );
    
    % caso mover ficha
    (length(Input,L2), L2 is 4,
    parse_input_move(Raw_input,R1,C1,R2,C2),
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
    ( % caso poner ficha
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
    printall(["DFS from", Nb_x, Nb_y, "using", OGCoor, "resulted in", CC]),
    length(CC, CCNodes), CCNodes is L-1,
    have_adjacent(X1, Y1, OnGameCells). 

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

spider_move(Hex1, X, Y, Player, Opponent, Player_R):-
    onGameCells(Player, Opponent, OnGameCells),
    not(occupied(X, Y, OnGameCells)),
    get_all(Hex1, T, Row, Col, C, _, _, _),
    can_move(Hex1, X, Y, OnGameCells), !,
    new_hex(T, X, Y, C, 0, 1, 2, Hex2),
    delete(OnGameCells, Hex1, OnGameCellsAux),
    empty_neighbours(OnGameCellsAux, [], OnGameCellsAux, Free_Cells),
    % findall(Path, length_dfs([Row, Col], 3, Free_Cells, Path), AllPaths),
    length_dfs([Row, Col], 3, Free_Cells, OnGameCells, Path),
    valid_path_end(Path, [X, Y]),
    find_hex(Hex1, Player, 0, Pos),
    replace_nth0(Player, Pos, _, Hex2, Player_R).

grasshoper_move(Hex1, X, Y, Player, Opponent, Player_R):-
    onGameCells(Player, Opponent, OnGameCells),
    not(occupied(X, Y, OnGameCells)),
    get_all(Hex1, T, _, _, C, _, _, _),
    can_move(Hex1, X, Y, OnGameCells), !,
    new_hex(T, X, Y, C, 0, 1, 2, Hex2),
    find_grasshoper_paths(Hex1, OnGameCells, Paths),
    valid_paths(X, Y, Paths, ValidPaths),
    length(ValidPaths, LVP),
    LVP > 0,
    find_hex(Hex1, Player, 0, Pos),
    replace_nth0(Player, Pos, _, Hex2, Player_R).

beetle_move(Hex1, X, Y, Player, Opponent, Player_R):-
    onGameCells(Player, Opponent, OnGameCells),
    not(occupied(X, Y, OnGameCells)),
    get_color(Hex1, C), get_type(Hex1, T),
    new_hex(T, X, Y, C, 0, 1, 2, Hex2),
    adjacents(Hex1, Hex2),
    can_move(Hex1, X, Y, OnGameCells),
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

pillbug_move(Hex1, X, Y, Player, Opponent, Player_R):-
    queen_move(Hex1, X, Y, Player, Opponent, Player_R).

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


pillbug_can_carry(PillbugHex, [X, Y], OnGameCells):-
    get_all(PillbugHex, _, Row, Col, _, Height, _, Blocked),
    maplist(get_coordinates, OnGameCells, OnGameCellsCoor),
    Height is 0, % the hex being moved cannot be part of a stack of pieces
    Blocked is 0, % the pillbug cannot move the last cell the opponent moved
    findall([X1, Y1], onGame_adjacents(X1, Y1, OnGameCellsCoor, Row, Col), Adj1),
    findall([X2, Y2], onGame_adjacents(X2, Y2, OnGameCellsCoor, X, Y), Adj2),
    intersection(Adj1, Adj2, CommonAdjs),
    not(two_common_of_height_two(CommonAdjs, OnGameCells, [])).

onGame_adjacents(X, Y, OnGameCellsCoordinates, AdjX, AdjY):-
    adjacents(X, Y, AdjX, AdjY),
    member([X, Y], OnGameCellsCoordinates).

two_common_of_height_two([[X, Y]|T], OnGameCells, Analized):-
    find_hex([X, Y], OnGameCells, hex(_,_,_,_,Height,_,_)),
    Height > 0, one_common_of_height_two(T, OnGameCells, [[X, Y]|Analized]).
two_common_of_height_two([[X, Y]|T], OnGameCells, Analized):- 
    two_common_of_height_two(T, OnGameCells, [[X, Y]|Analized]).

one_common_of_height_two([[X, Y]|_], OnGameCells, Analized):-
    find_hex([X, Y], OnGameCells, hex(_,_,_,_,Height,_,_)),
    not(member([X, Y], Analized)), Height > 0.
one_common_of_height_two([[X, Y]|T], OnGameCells, Analized):- 
    one_common_of_height_two(T, OnGameCells, [[X, Y]|Analized]).



find_grasshoper_paths(Hex, OnGameCells, Paths):-
    get_row(Hex, Row), get_col(Hex, Col),
    straight_line(Row, Col, 1, 0, OnGameCells,[], R10),
    straight_line(Row, Col, 0, 1, OnGameCells,[], R01),
    straight_line(Row, Col, -1, 1, OnGameCells, [], R_11),
    straight_line(Row, Col, -1, 0, OnGameCells, [], R_10),
    straight_line(Row, Col, 0, -1, OnGameCells, [], R0_1),
    straight_line(Row, Col, 1, -1, OnGameCells, [], R1_1),
    append([[R10],[R01],[R_11],[R_10],[R0_1],[R1_1]], AllPaths),
    include(path_greater_than_2(), AllPaths, Paths).

find_ladybug_paths(OnGameCells, X, Y, Depth, Paths):-
    empty_neighbours(OnGameCells, [], OnGameCells, Free_Cells),
    maplist(get_coordinates, OnGameCells, OG),
    append(OG, Free_Cells, Board),
    findall(P, dfs_path(X, Y, Depth, Board, _, P), V1),
    list_to_set(V1, P1),
    reverse_all(P1, Paths).

straight_line(Row, Col, _, _, OnGameCells, Acc, R):-
    not(occupied(Row, Col, OnGameCells)),
    append(Acc, [[Row,Col]], R).

straight_line(Row, Col, DirRow, DirCol, OnGameCells, Acc, R):-
    occupied(Row, Col, OnGameCells),
    find_hex([Row, Col], OnGameCells, Hex),
    append(Acc, [Hex], Acc1),
    sum([Row, DirRow], Row1), sum(Col, DirCol, Col1),
    straight_line(Row1, Col1, DirRow, DirCol, OnGameCells, Acc1, R).

valid_ladybug_path(Path, OnGameCellsCoordinates, HiveBorderCellCoordinates):-
    nth0(1, Path, SecondMove),
    nth0(2, Path, ThirdMove),
    nth0(3, Path, FourthMove),
    member(SecondMove, OnGameCellsCoordinates),
    member(ThirdMove, OnGameCellsCoordinates),
    member(FourthMove, HiveBorderCellCoordinates).

find_depth_paths(OnGameCells, X, Y, Depth, Paths):-
    empty_neighbours(OnGameCells, [], OnGameCells, Free_Cells),
    findall(P, dfs_path(X, Y, Depth, Free_Cells, _, P), V1),
    list_to_set(V1, P1),
    reverse_all(P1, Paths).

find_all_paths(_,_,_,1,[]).
find_all_paths(OnGameCells, X, Y, Depth, Paths):-
    find_depth_paths(OnGameCells,X,Y,Depth,P1),
    predecessor(Depth,D1),
    find_all_paths(OnGameCells, X, Y, D1, P2),
    append(P1,P2,Paths).

neighbours(_, _, [], _, []).
neighbours(X, Y, [Nb|Tail], Visited, Nbs):- 
    (is_nb(X, Y, Nb, Visited), neighbours(X, Y, Tail, Visited, Nbs1), append([Nb], Nbs1, Nbs)); 
    neighbours(X, Y, Tail, Visited, Nbs).

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
    single_path([], [X, Y], Dest, Candidates, OnGameCells, RevSolution),
    reverse(RevSolution, Solution).

single_path(Stack, [X, Y], [X, Y], _, _, [[X, Y]|Stack]). 
single_path(Stack, [X, Y], Dest, Candidates, OnGameCells, Sol):-
    boku_no_adj([X, Y], Candidates, Stack, Adj),
    reachable([X, Y], Adj, OnGameCells),
    single_path([[X, Y]|Stack], Adj, Dest, Candidates, OnGameCells, Sol).


capped_dfs([X, Y], Dest, Candidates, Cap, OnGameCells, Solution):-
    capped_path([], [X, Y], Dest, Candidates, Cap, OnGameCells, RevSolution),
    reverse(RevSolution, Solution).
    
capped_path(Stack, [X, Y], [X, Y], _, Cap, _, [[X, Y]|Stack]):- 
    length(Stack, StackLength), StackLength is Cap.

capped_path(Stack, [X, Y], Dest, Candidates, Cap, OnGameCells, Path):-
    length(Stack, PathLength), PathLength < Cap,
    boku_no_adj([X, Y], Candidates, Adj),
    reachable([X, Y], Adj, OnGameCells),
    capped_path([[X, Y]|Stack], Adj, Dest, Candidates, Cap, OnGameCells, Path).


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

cc_bfs([X, Y], Candidates, CC):-
    cc_path([ [X, Y] ], [], Candidates, CC).

cc_path([], Visited, _, Visited).
cc_path([Head|Queue], Visited, Candidates, CC):-
    findall(Adj, boku_no_adj(Head, Candidates, Visited, Adj), Adjs),
    union(Queue, Adjs, NewQueue),
    cc_path(NewQueue, [Head|Visited], Candidates, CC).

