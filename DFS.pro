:-use_module(board).


get_type(Hex,Type):-       arg(1,Hex,Type).
get_row(Hex,Row):-         arg(2,Hex,Row).
get_col(Hex,Col):-         arg(3,Hex,Col).
get_color(Hex,Color):-     arg(4,Hex,Color).
get_height(Hex,Height):-   arg(5,Hex,Height).
get_onGame(Hex,OnGame):-   arg(6,Hex,OnGame).
get_blocked(Hex,OnGame):-  arg(7,Hex,OnGame).

get_all(Hex, T, X, Y, C, H, O, B):- 
    get_type(Hex,T),
    get_row(Hex,X),
    get_col(Hex,Y),
    get_color(Hex,C),
    get_height(Hex,H),
    get_onGame(Hex,O),
    get_blocked(Hex,B).


printall([]):-
    write("\n").
printall([X|T]):-
    write(X),
    write(" "),
    printall(T).

add(X, Y, Z):- Z is X + Y.
same(X,X).
successor(X, Y):- Y is X + 1.
predecessor(X, Y):- Y is X - 1.
halve(L,R):- R is L/2 + 1.

replace_nth0(List, Index, OldElem, NewElem, NewList) :-
    nth0(Index,List,OldElem,Transfer),
    nth0(Index,NewList,NewElem,Transfer).

reverssed([X],[X]).
reverssed([X|Y],L2):-
    reverssed(Y,L2R),
    append(L2R,[X],L2).

reverse_all(L, R):-
    maplist(reverse(),L,R).

adjacents(Hex1,Hex2):- 
    get_row(Hex1,Row1),
    get_row(Hex2,Row2),
    get_col(Hex1,Col1),
    get_col(Hex2,Col2),
    ((Col1 is Col2, (Row1 is Row2-1 ; Row1 is Row2+1) );
    (Col1 is Col2+1, (Row1 is Row2 ; Row1 is Row2-1) );
    (Col1 is Col2-1, ( Row1 is Row2 ; Row1 is Row2+1) )).




new_hex(Type, Row, Col, Color, Height, OnGame, Blocked, hex(Type, Row, Col, Color, Height, OnGame, Blocked)).

init_player1(Color,List):-
    new_hex("queen",     _,_,Color,0,0,0,Queen),
    new_hex("ant",       _,_,Color,0,0,0,Ant1),
    new_hex("ant",       _,_,Color,0,0,0,Ant2),
    new_hex("ant",       _,_,Color,0,0,0,Ant3),
    new_hex("grasshoper",_,_,Color,0,0,0,Grasshoper1),
    new_hex("grasshoper",_,_,Color,0,0,0,Grasshoper2),
    new_hex("grasshoper",_,_,Color,0,0,0,Grasshoper3),
    new_hex("beetle",    _,_,Color,0,0,0,Beetle1),
    new_hex("beetle",    _,_,Color,0,0,0,Beetle2),
    new_hex("spider",    _,_,Color,0,0,0,Spider1),
    new_hex("spider",    _,_,Color,0,0,0,Spider2),
    new_hex("mosquito",  _,_,Color,0,0,0,Mosquito),
    new_hex("pillbug",   _,_,Color,0,0,0,PillBug),
    new_hex("ladybug",   _,_,Color,0,0,0,Ladybug),
    append([], [Queen, Ant1, Ant2, Ant3, 
                Grasshoper1, Grasshoper2, 
                Grasshoper3, Beetle1, Beetle2, 
                Spider1, Spider2, Mosquito, PillBug, Ladybug], List).
init_player2(Color,List):-
    new_hex("queen",     _,_,Color,0,0,0,Queen),
    new_hex("ant",       _,_,Color,0,0,0,Ant1),
    new_hex("ant",       _,_,Color,0,0,0,Ant2),
    new_hex("ant",       _,_,Color,0,0,0,Ant3),
    new_hex("grasshoper",_,_,Color,0,0,0,Grasshoper1),
    new_hex("grasshoper",_,_,Color,0,0,0,Grasshoper2),
    new_hex("grasshoper",_,_,Color,0,0,0,Grasshoper3),
    new_hex("beetle",    _,_,Color,0,0,0,Beetle1),
    new_hex("beetle",    _,_,Color,0,0,0,Beetle2),
    new_hex("spider",    _,_,Color,0,0,0,Spider1),
    new_hex("spider",    _,_,Color,0,0,0,Spider2),
    new_hex("mosquito",  _,_,Color,0,0,0,Mosquito),
    new_hex("pillbug",   _,_,Color,0,0,0,PillBug),
    new_hex("ladybug",   _,_,Color,0,0,0,Ladybug),
    append([], [Queen, Ant1, Ant2, Ant3, 
                Grasshoper1, Grasshoper2, 
                Grasshoper3, Beetle1, Beetle2, 
                Spider1, Spider2, Mosquito, PillBug, Ladybug], List).

players(Player1,Player2):-
    player1(Player1),
    player2(Player2).

player1(List):- init_player1(1, List).
player2(List):- init_player2(2, List).

% onGameSingle([], []).
% onGameSingle([Hex|Tail], List):- get_onGame(Hex, OnGame), ((OnGame is 1, onGameSingle(Tail, L), append([Hex], L, List)) ; onGameSingle(Tail, L), append([], L, List)).

% onGameCells(Player1, Player2, List):- onGameSingle(Player1, L3), 
%                             onGameSingle(Player2, L4), append(L3, L4, List).

onGameCells(Player1, Player2, Result):-
    include(is_on_game(), Player1, OnGamePlayer1),
    include(is_on_game(), Player2, OnGamePlayer2),
    append(OnGamePlayer1, OnGamePlayer2, Result).

is_on_game(Hex):- get_onGame(Hex, OG), OG is 1.

% devuelve en Hex una celda en juego en las coordenadas X, Y. En caso de no existir devuelve falso.

find_queen(Color, [hex("queen", Row, Col, Color, Height, OnGame, Block)|_], hex("queen", Row, Col, Color, Height, OnGame, Block)).
find_queen(Color, [_|Tail], Hex):- find_queen(Color, Tail, Hex).

find_hex(Pos, L, Hex1):-
    findall(Hex, find_all_at(Pos, L, Hex), Hexs),
    length(Hexs, Len), Len > 0,
    nth0(0, Hexs, H),
    higher(Hexs, H, Hex1), !.

higher([], Ch, Ch).
higher([Head|Tail], Current_Higher, Higher):-
    get_height(Current_Higher, H), get_height(Head, H1), 
    (
    (H1 >= H, higher(Tail, Head, High1), get_height(High1, Higher1), 
    ((H1 >= Higher1, Higher = Head); (Higher = High1)));

    (H >= H1, higher(Tail, Current_Higher, High1), get_height(High1, Higher1), 
    ((H >= Higher1, Higher = Current_Higher); (Higher = High1)))
    %podria faltar el caso en que H1 == H

    ).

find_all_at([X, Y], [Hex|Tail], Hex1):- 
    get_all(Hex, _, Row1, Col1, _, _, OG1,_),
    ( (Row1 is X, Col1 is Y, OG1 is 1, Hex1 = Hex);
    find_all_at([X, Y], Tail, Hex1)).

find_hex(_, [], _, -1):- 2 is 1.
find_hex(Hex, [H|T], Index, Pos):-
    get_all(Hex, Type, Row, Col, Color, Height, _, _),
    get_all(H, Type1, Row1, Col1, Color1, Height1, O, _),
    (Type1 = Type, Row1 is Row,
    Col1 is Col, Color1 is Color, Height1 is Height, O is 1, Pos is Index); 
    (successor(Index, Index1), find_hex(Hex, T, Index1, Pos)).

% Devuelve un Hex adjacente a H.
get_adjacent(_, [], 0).
get_adjacent(Hex, [Adj|Tail], Adj):- adjacents(Hex, Adj); get_adjacent(Hex, Tail, Adj).


free_bug_place(_, _, []):-1 is 2.
free_bug_place(T, Color, [Hex|Tail]):-
    (get_type(Hex, T1), get_onGame(Hex, OnGame), get_color(Hex, C),
    C is Color, T1 = T, OnGame is 0) ; free_bug_place(T, Color, Tail).

occupied( _, _, []):- 2 is 1.
occupied(X, Y, [Hex|Tail]):-
    (get_row(Hex, R1), get_col(Hex, C1),
    R1 is X, C1 is Y ) ; occupied(X, Y, Tail).

all_same_color(_, []).
all_same_color(Color, [H|T]):-
    get_color(H, C1), 
    C1 is Color, 
    all_same_color(Color, T).

valid_place(Cell, Cells):- 
    % onGameSingle(Cells, OnGameCells),
    include(is_on_game, Cells, OnGameCells),
    neighbours(Cell, OnGameCells, Nbs), !,
    length(Nbs,L), L > 0,
    get_color(Cell, C1), all_same_color(C1, Nbs).

get_first_letter(T, L):-
    T = "queen",      L = "Q".
get_first_letter(T, L):-
    T = "ant",        L = "A".
get_first_letter(T, L):-
    T = "grasshoper", L = "G".
get_first_letter(T, L):-
    T = "beetle",     L = "B".
get_first_letter(T, L):-
    T = "spider",     L = "S".
get_first_letter(T, L):-
    T = "mosquito",   L = "M".
get_first_letter(T, L):-
    T = "pillbug",    L = "P".
get_first_letter(T, L):-
    T = "ladybug",    L = "L".
get_color_letter(C, L):-
    C is 1, L = "W".
get_color_letter(C, L):-
    C is 2, L = "B".

check_coordinates(X, Y, Hex):-
    get_row(Hex, X1), get_col(Hex, Y1),
    X1 is X, Y1 is Y.
check_for_highests([], _, []).
check_for_highests([Head|Tail], L, Result):-
    get_row(Head, X), get_col(Head, Y),
    include(check_coordinates(X, Y), L, Filtered),
    write(Tail), write(" primer caso\n"),
    length(Filtered, Len), Len is 1, check_for_highests(Tail, L, R), 
    append(Filtered, R, Result).
check_for_highests([Head|Tail], L, Result):-
    write(Head), write(" segundo caso\n"),
    get_row(Head, X), get_col(Head, Y),
    include(check_coordinates(X, Y), L, Filtered),
    length(Filtered, Len), Len > 1, nth0(0, Filtered, Current_Higher),
    higher(Filtered, Current_Higher, Higher),
    check_for_highests(Tail, L, R), 
    append([Higher], R, Result).
% check_for_highests([Head|Tail], L, Result):-
%     write(Head), write(" segundo caso\n"),
%     get_row(Head, X), get_col(Head, Y),
%     include(check_coordinates(X, Y), L, Filtered),
%     length(Filtered, Len), Len > 1, nth0(0, Filtered, Current_Higher),
%     higher(Filtered, Current_Higher, Higher),
%     check_for_highests(Tail, L, R), 
%     append([Higher], R, Result).
    


% revisar uno por uno y buscar cuantos en esa posicion, 
% si hay mas de uno quedarse con el higher

convert_cells(Hex, Converted_Hex):-
    get_type(Hex, T), get_first_letter(T, Letter), 
    get_color(Hex, C), get_color_letter(C, Color),
    concat(Color, Letter, Name), get_row(Hex, X), get_col(Hex, Y), 
    Converted_Hex = [X, Y, Name].

get_converted_cells(Cells, Converted_Cells):-
    check_for_highests(Cells, Cells, CC),
    maplist(convert_cells, CC, Converted_Cells).


queen_on_game([hex(Type, _, _, Color, _, OnGame, _)|_], PlayerColor):-
    Type = "queen", Color = PlayerColor, OnGame is 1.
queen_on_game([_|T], PlayerColor):- queen_on_game(T, PlayerColor).

    % get_color(H, C),
    % get_onGame(H, O),
    % get_type(H, T1),
    % ((C is Color,
    % T1 = "queen",
    % O is 1);
    % queen_on_game(T, Color)).

valid_state(Cells, Turn, Color, Type):-
    queen_on_game(Cells, Color); Turn < 4; (Turn is 4, Type = "queen").

find_free_bug(_, [], _, -1).
find_free_bug(Type, [H|T], Index, Pos):-
    (get_type(H, T1), T1 = Type, get_onGame(H, O), O is 0, Pos is Index); 
    (successor(Index, Index1), find_free_bug(Type, T, Index1, Pos)).


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

get_coordinates(hex(_, Row, Col, _, _, _, _), [Row, Col]). %- get_row(Hex, Row), get_col(Hex, Col), Coor = [Row, Col].
get_coordinates([Row, Col], [Row, Col]).

unblock(BlockedHex, UnblockedHex):-
    maplist(reduce_block(), BlockedHex, UnblockedHex).

reduce_block(hex(Type, Row, Col, Color, Height, OnGame, X), hex(Type, Row, Col, Color, Height, OnGame, XB)):- X > 0, succ(XB, X).
reduce_block(hex(Type, Row, Col, Color, Height, OnGame,0), hex(Type, Row, Col, Color, Height, OnGame,0)).

% DFS stuffs
neighbours(_, [], []).
neighbours(Hex, [Nb|Tail], Nbs):- 
    (adjacents(Hex, Nb), neighbours(Hex, Tail, Nbs_), 
    append([Nb], Nbs_, Nbs)); 
    neighbours(Hex, Tail, Nbs).

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

parse_input_place(Raw_input, Type, Row, Col):-
    split_string(Raw_input,"\s","\s",Input),
    nth0(0,Input,Type),
    nth0(1,Input,R1),
    nth0(2,Input,C1),
    atom_number(R1,Row),
    atom_number(C1,Col).

parse_input_move(Raw_input,R1,C1,R2,C2):-
    split_string(Raw_input,"\s","\s",Input),
    nth0(0,Input,R_1),
    nth0(1,Input,C_1),
    nth0(2,Input,R_2),
    nth0(3,Input,C_2),
    atom_number(R_1,R1),
    atom_number(C_1,C1),
    atom_number(R_2,R2),
    atom_number(C_2,C2).

parse_input_special(Raw_input, R1, C1, R2, C2, R3, C3):-
    split_string(Raw_input,"\s","\s",Input),
    nth0(0,Input,R_1),
    nth0(1,Input,C_1),
    nth0(2,Input,R_2),
    nth0(3,Input,C_2),
    nth0(4,Input,R_3),
    nth0(5,Input,C_3),
    atom_number(R_1,R1),
    atom_number(C_1,C1),
    atom_number(R_2,R2),
    atom_number(C_2,C2),
    atom_number(R_3,R3),
    atom_number(C_3,C3).


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
    ( % caso prueba
    (length(Input,L1), L1 is 3,
    parse_input_place(Raw_input, Type, Row, Col),
    Type = "test",
    test([Row, Col], Player1, Player2),
    NewPlayer1 = Player1, NewPlayer2 = Player2);
    
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


check_color(Hex, Player):-
    nth0(0, Player, PlayerHex),
    get_color(PlayerHex, PHC), get_color(Hex, HC),
    HC is PHC.
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


have_adjacent(X, Y, [hex(_,Row,Col,_,_,_,_)|_]):- adjacents(X, Y, Row, Col),!.
have_adjacent(X, Y, [_|T]):- have_adjacent(X, Y, T).
have_adjacent(_, _, []):- false().


can_move(Hex1, X1, Y1, OnGameCells):-
    length(OnGameCells, L),
    get_all(Hex1, T, X, Y, C, _,_,B),
    B is 0,
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


queen_move(Hex1, X, Y, Player, Opponent, Player_R):-
    onGameCells(Player, Opponent, OnGameCells),
    not(occupied(X, Y, OnGameCells)),
    get_color(Hex1, C), get_type(Hex1, T),
    new_hex(T, X, Y, C, 0, 1, 2, Hex2),
    adjacents(Hex1, Hex2),
    can_move(Hex1, X, Y, OnGameCells),
    find_hex(Hex1, Player, 0, Pos),
    replace_nth0(Player, Pos, _, Hex2, Player_R).
    %Faltaria verificar que puede meterse ahi.    
    
queen_path(Hex, OnGameCells, Path):-
    freedom_to_move(Hex, OnGameCells), !,
    
    get_row(Hex, Row), get_col(Hex, Col),
    adjacents(AdjX, AdjY, Row, Col),
    not(occupied(AdjX, AdjY, OnGameCells)),
    
    delete(OnGameCells, Hex, OnGameCellsTemp),
    have_adjacent(AdjX, AdjY, OnGameCellsTemp),
    Path = [[Row, Col], [AdjX, AdjY]].

queen_path(_, _, []).


ant_move(Hex1, X, Y, Player, Opponent, Player_R):-
    onGameCells(Player, Opponent, OnGameCells),
    not(occupied(X, Y, OnGameCells)),
    get_all(Hex1, T, Row, Col, C, _, _, _),
    can_move(Hex1, X, Y, OnGameCells),!,
    new_hex(T, X, Y, C, 0, 1, 2, Hex2),

    delete(OnGameCells, Hex1, OnGameCellsAux),
    
    vecinos_void(OnGameCellsAux, [], OnGameCellsAux, Free_Cells),

    single_dfs([Row, Col], [X, Y], Free_Cells, OnGameCells, Path),

    valid_path_end(Path, [X, Y]),

    find_hex(Hex1, Player, 0, Pos),
    replace_nth0(Player, Pos, _, Hex2, Player_R).

ant_path(Hex, OnGameCells, Path):-
    % onGameCells(Player, Opponent, OnGameCells),
    get_all(Hex, _, Row, Col, _, _, _, _),
    freedom_to_move(Hex, OnGameCells),

    delete(OnGameCells, Hex, OnGameCellsAux),

    vecinos_void(OnGameCellsAux, [], OnGameCellsAux, Free_Cells),!,
    full_dfs([Row, Col], Free_Cells, OnGameCells, Path).

ant_path(_, _, []).


spider_move(Hex1, X, Y, Player, Opponent, Player_R):-
    onGameCells(Player, Opponent, OnGameCells),
    not(occupied(X, Y, OnGameCells)),
    get_all(Hex1, T, Row, Col, C, _, _, _),
    can_move(Hex1, X, Y, OnGameCells), !,
    new_hex(T, X, Y, C, 0, 1, 2, Hex2),
    
    delete(OnGameCells, Hex1, OnGameCellsAux),
    vecinos_void(OnGameCellsAux, [], OnGameCellsAux, Free_Cells),

    % findall(Path, length_dfs([Row, Col], 3, Free_Cells, Path), AllPaths),
    % write_all(AllPaths),

    length_dfs([Row, Col], 3, Free_Cells, OnGameCells, Path),
    valid_path_end(Path, [X, Y]),

    find_hex(Hex1, Player, 0, Pos),
    replace_nth0(Player, Pos, _, Hex2, Player_R).

spider_path(Hex, OnGameCells, Path):-
    get_all(Hex, _, Row, Col, _, _, _, _),
    freedom_to_move(Hex, OnGameCells),

    delete(OnGameCells, Hex, OnGameCellsAux),

    vecinos_void(OnGameCellsAux, [], OnGameCellsAux, Free_Cells), !,
    length_dfs([Row, Col], 3, Free_Cells, OnGameCells, Path).

spider_path(_, _, []).


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


ladybug_move(Hex1, X, Y, Player, Opponent, Player_R):-
    onGameCells(Player, Opponent, OnGameCells),
    not(occupied(X, Y, OnGameCells)),
    get_all(Hex1, T, Row, Col, C, _, _, _),
    can_move(Hex1, X, Y, OnGameCells),
    new_hex(T, X, Y, C, 0, 1, 2, Hex2),

    delete(OnGameCells, Hex1, OnGameCellsAux),

    vecinos_void(OnGameCells, [], OnGameCells, Free_Cells), !,
    maplist(get_coordinates, OnGameCellsAux, OnGameCellsAuxCoordinates),
    append(Free_Cells, OnGameCellsAuxCoordinates, AllLadybugPathCells),
    
    capped_dfs([Row, Col], [X, Y], AllLadybugPathCells, 3, Path),

    valid_ladybug_path(Path, OnGameCellsAuxCoordinates, Free_Cells),

    % printall(["Found:", Path]),

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


pillbug_move(Hex1, X, Y, Player, Opponent, Player_R):-
    queen_move(Hex1, X, Y, Player, Opponent, Player_R).

pillbug_path(Hex, OnGameCells, Path):-
    queen_path(Hex, OnGameCells, Path).


pillbug_special(PillbugHex, MovingHex, X, Y, Player, Opponent, Player_R):-
    onGameCells(Player, Opponent, OnGameCells),
    not(occupied(X, Y, OnGameCells)),
    get_all(MovingHex, T, MVRow, MVCol, Color, _, _, _),
    % printall(OnGameCells),
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
mosquito_path_aux(Hex, "ant", OnGameCells, Path):-
    ant_path(Hex, OnGameCells, Path).
mosquito_path_aux(Hex, "spider", OnGameCells, Path):-
    spider_path(Hex, OnGameCells, Path).
mosquito_path_aux(Hex, "ladybug", OnGameCells, Path):-
    ladybug_path(Hex, OnGameCells, Path).
mosquito_path_aux(Hex, "pillbug", OnGameCells, Path):-
    pillbug_path(Hex, OnGameCells, Path).

mosquito_path_aux(_, "mosquito", _, []).



pillbug_can_carry(PillbugHex, [X, Y], OnGameCells):-
    get_all(PillbugHex, _, Row, Col, _, Height, _, Blocked),
    maplist(get_coordinates, OnGameCells, OnGameCellsCoor),
    Height is 0, % the hex being moved cannot be part of a stack of pieces
    Blocked is 0, % the pillbug cannot move the last cell the opponent moved
    findall([X1, Y1], onGame_adjacents(X1, Y1, OnGameCellsCoor, Row, Col), Adj1),
    findall([X2, Y2], onGame_adjacents(X2, Y2, OnGameCellsCoor, X, Y), Adj2),
    intersection(Adj1, Adj2, CommonAdjs),

    % printall(["Adjacents to", Row, Col, "are", Adj1]),
    % printall(["Adjacents to", X, Y, "are", Adj2]),
    
    % printall(["Common adjacents of", Row, Col, "and", X, Y, "are", CommonAdjs]),
    not(two_common_of_height_two(CommonAdjs, OnGameCells, [])).

onGame_adjacents(X, Y, OnGameCellsCoordinates, AdjX, AdjY):-
    adjacents(X, Y, AdjX, AdjY),
    member([X, Y], OnGameCellsCoordinates).

two_common_of_height_two([[X, Y]|T], OnGameCells, Analized):-
    % printall(["looking for two cells of height >= 1, current is", X, Y, "tail = ",T, "  already analized:", Analized]),
    find_hex([X, Y], OnGameCells, hex(_,_,_,_,Height,_,_)),
    Height > 0, one_common_of_height_two(T, OnGameCells, [[X, Y]|Analized]).
    % printall(["Found hex", Row, Col, "of height =", Height, "1/2"])
two_common_of_height_two([[X, Y]|T], OnGameCells, Analized):- two_common_of_height_two(T, OnGameCells, [[X, Y]|Analized]).

one_common_of_height_two([[X, Y]|_], OnGameCells, Analized):-
    % printall(["looking for just one cell of height >= 1, current is", X, Y, "tail = ",T, "  already analized:", Analized]),
    find_hex([X, Y], OnGameCells, hex(_,_,_,_,Height,_,_)),
    not(member([X, Y], Analized)),
    Height > 0.%, printall(["Found hex", Row, Col, "of height =", Height, "2/2"]).
one_common_of_height_two([[X, Y]|T], OnGameCells, Analized):- one_common_of_height_two(T, OnGameCells, [[X, Y]|Analized]).



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


find_ladybug_paths(OnGameCells, X, Y, Depth, Paths):-
    vecinos_void(OnGameCells, [], OnGameCells, Free_Cells),
    maplist(get_coordinates, OnGameCells, OG),
    append(OG, Free_Cells, Board),
    findall(P, dfs_path(X, Y, Depth, Board, _, P), V1),
    % write_all(V1),
    list_to_set(V1, P1),
    reverse_all(P1, Paths).

straight_line(Row, Col, _, _, OnGameCells, Acc, R):-
    % write("Checking for not occupied\n"),
    not(occupied(Row, Col, OnGameCells)),
    % printall(["End of line at", Row, Col, "appending to ", Acc]),
    append(Acc, [[Row,Col]],R).

straight_line(Row, Col, DirRow, DirCol, OnGameCells, Acc, R):-
    % write("Checking for occupied\n"),
    occupied(Row, Col, OnGameCells),
    % printall(["Jumping over", Row, Col, "appending to", Acc ]),
    find_hex([Row, Col], OnGameCells, Hex),
    append(Acc, [Hex], Acc1),
    add(Row, DirRow, Row1), add(Col, DirCol, Col1),
    straight_line(Row1, Col1, DirRow, DirCol, OnGameCells, Acc1, R).

path_of_length_3(X):- length(X, L), L = 4.   % 4 because length of a path is |Path|-1
path_greater_than_2(X) :- length(X, L), L > 2.

there_is_a_path(_,_,[]):- write("exiting\n"),1 = 2.
there_is_a_path(X,Y,[H|T]):-
    printall(["looking for ", X, Y, "in ", H]),
    (last(H,L), nth0(0, L, HX), HX=X, nth0(1, L, HY), HY = Y, ! );
    there_is_a_path(X,Y,T).

valid_path_end(Path, DestHex):-
    last(Path, Last), Last = DestHex.

valid_ladybug_path(Path, OnGameCellsCoordinates, HiveBorderCellCoordinates):-
    nth0(1, Path, SecondMove),
    nth0(2, Path, ThirdMove),
    nth0(3, Path, FourthMove),
    % printall(["Analyzing validity of the path found\n", SecondMove, "and", ThirdMove, "belong? to ", OnGameCellsAuxCoordinates, "\n", FourthMove, "belongs? to", Free_Cells]),
    member(SecondMove, OnGameCellsCoordinates),
    member(ThirdMove, OnGameCellsCoordinates),
    member(FourthMove, HiveBorderCellCoordinates).


vecino(Hex, Cells, Voids, A):-
    get_all(Hex, _, X1, Y1, _, _, _, _),
    adjacents(X, Y, X1, Y1), not(occupied(X, Y, Cells)), append([X], [Y], A),
    not(member(A, Voids)).

vecinos(Hex,  Empties, Cells, Vecinos):-
    findall(Nb, vecino(Hex, Cells, Empties, Nb), Vecinos).

vecinos_void([],_, _, []).
vecinos_void([Hex|Tail], Empties, Cells, V):-
    vecinos(Hex, Empties, Cells, V1),
    append(Empties, V1, Empties1),
    vecinos_void(Tail, Empties1, Cells, V2),
    append(V1, V2, V).

find_depth_paths(OnGameCells, X, Y, Depth, Paths):-
    vecinos_void(OnGameCells, [], OnGameCells, Free_Cells),
    findall(P, dfs_path(X, Y, Depth, Free_Cells, _, P), V1),
    % write_all(V1),
    list_to_set(V1, P1),
    reverse_all(P1, Paths).

find_all_paths(_,_,_,1,[]).
find_all_paths(OnGameCells, X, Y, Depth, Paths):-
    find_depth_paths(OnGameCells,X,Y,Depth,P1),
    predecessor(Depth,D1),
    find_all_paths(OnGameCells, X, Y, D1, P2),
    append(P1,P2,Paths).


write_all([]):-write("\n").
write_all([Head|Tail]):-
    write(Head), write("\n"), write_all(Tail).

adjacents(Row1, Col1, Row2, Col2):-
    ((Col1 is Col2, (Row1 is Row2-1 ; Row1 is Row2+1) );
    (Col1 is Col2+1, (Row1 is Row2 ; Row1 is Row2-1) );
    (Col1 is Col2-1, ( Row1 is Row2 ; Row1 is Row2+1) )).


is_nb(X, Y, Nb, Visited):-
    nth0( 0, Nb, X1),
    nth0( 1, Nb, Y1),
    not(member([X1,Y1], Visited)),
    adjacents(X, Y, X1, Y1).


neighbours(_, _, [], _, []).
neighbours(X, Y, [Nb|Tail], Visited, Nbs):- 
    (is_nb(X, Y, Nb, Visited), neighbours(X, Y, Tail, Visited, Nbs1), append([Nb], Nbs1, Nbs)); 
    neighbours(X, Y, Tail, Visited, Nbs).

boku_no_neighbours(Visited, [X,Y], [M,N]):-
    write("Visited:\n"),
    printall(Visited),
    printall(["Analizing if", M, N, "is adjacent to", X, Y]),
    not(member([M, N], Visited)),
    write("Ok, it wasn't visited\n"),
    adjacents(X, Y, M, N), write("Adjacent found, appending\n").

ineighbours([X,Y], Candidates, Visited, Nbs):-
    include(boku_no_neighbours(Visited, [X,Y]), Candidates, Nbs).

% is_nb(X, Y, Visited, Nb):-
%     write_all(["is_nb",Visited, Nb]),
%     nth0( 0, Nb, X1),
%     nth0( 1, Nb, Y1),
%     not(member([X1,Y1], Visited)),
%     adjacents(X, Y, X1, Y1).
% neighbours(X, Y, V, L, Nbs):-
%     write_all([X, Y, L]),
%     include(is_nb(X, Y, V), L, Nbs),
%     write_all([Nbs]).
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

true_path(X, Y,  Head):-
    length(Head, L), predecessor(L, P), nth0(P, Head, H), 
    nth0( 0, H, X1), nth0( 1, H, Y1),
    X1 is X, Y1 is Y.

valid_paths(X, Y, Paths, ValidPaths):-
    include(true_path(X,Y), Paths, ValidPaths).

reachable( [From_x, From_y], [To_x, To_y], OnGameCells):-
    % Common_x and Common_y goes first bc adjacents() instantiate the first two args out of the others
    adjacents(CommonAdj_x, CommonAdj_y, From_x, From_y),
    adjacents(CommonAdj_x, CommonAdj_y, To_x, To_y),
    not(occupied(CommonAdj_x, CommonAdj_y, OnGameCells)).

single_dfs([X, Y], Dest, Candidates, OnGameCells, Solution):-
    % maplist(get_coordinates, Candidates, MappedCandidates),
    single_path([], [X, Y], Dest, Candidates, OnGameCells, RevSolution),
    reverse(RevSolution, Solution).

single_path(Stack, [X, Y], [X, Y], _, _, [[X, Y]|Stack]). % : write("Found:\n"), write_all([[X, Y]|Stack]).

single_path(Stack, [X, Y], Dest, Candidates, OnGameCells, Sol):-
    boku_no_adj([X, Y], Candidates, Stack, Adj),
    reachable([X, Y], Adj, OnGameCells),
    single_path([[X, Y]|Stack], Adj, Dest, Candidates, OnGameCells, Sol).


capped_dfs([X, Y], Dest, Candidates, Cap, OnGameCells, Solution):-
    capped_path([], [X, Y], Dest, Candidates, Cap, OnGameCells, RevSolution),
    reverse(RevSolution, Solution).
    
capped_path(Stack, [X, Y], [X, Y], _, Cap, _, [[X, Y]|Stack]):- length(Stack, StackLength), StackLength is Cap.

capped_path(Stack, [X, Y], Dest, Candidates, Cap, OnGameCells, Path):-
    length(Stack, PathLength), PathLength < Cap,
    boku_no_adj([X, Y], Candidates, Adj),
    reachable([X, Y], Adj, OnGameCells),
    capped_path([[X, Y]|Stack], Adj, Dest, Candidates, Cap, OnGameCells, Path).


ladybug_dfs([X, Y], Length, Candidates, Solution):-
    printall(["Candidates are", Candidates]),
    get_ladybug_path([], [X, Y], Length, Candidates, RevSolution),
    reverse(RevSolution, Solution).

get_ladybug_path(Stack, [X, Y], Length, _, [[X, Y]|Stack]):-
    length(Stack, StackLength), printall(["Is", Stack, "of Length =", Length]), StackLength is Length, printall(["Found", [[X,Y]|Stack]]).

get_ladybug_path(Stack, [X, Y], Length, Candidates, Path):-
    length(Stack, StackLength), StackLength < Length,
    printall(["Current node is", X, Y, "path = ", Stack]),
    boku_no_adj([X, Y], Candidates, Stack, Adj),
    printall(["Calling recursively with", Adj]), write("\n"),
    get_ladybug_path([[X, Y]|Stack], Adj, Length, Candidates, Path).


length_dfs([X, Y], Length, Candidates, OnGameCells, Solution):-
    length_path([], [X, Y], Length, Candidates, OnGameCells, RevSolution),
    reverse(RevSolution, Solution).

length_path(Stack, [X, Y], Length, _, _, [[X, Y]|Stack]):-
    % printall(["Found",[[X,Y]|Stack] ]),
    length(Stack, StackLength), StackLength is Length.

length_path(Stack, [X, Y], Length, Candidates, OnGameCells, Path):-
    length(Stack, StackLength), StackLength < Length,
    boku_no_adj([X, Y], Candidates, Stack, Adj),
    reachable([X, Y], Adj, OnGameCells),
    length_path([[X, Y]|Stack], Adj, Length, Candidates, OnGameCells, Path).

full_dfs([X, Y], Candidates, OnGameCells, Solution):-
    any_path([], [X, Y], Candidates, OnGameCells, RevSolution),
    reverse(RevSolution, Solution).

any_path(Stack, [X, Y], _, _, [[X, Y]|Stack]):- length(Stack, L), L > 0. %, printall([ "Found", [ [X, Y]|Stack] ]).

any_path(Stack, [X, Y], Candidates, OnGameCells, Path):-
    % printall(["current node", X, Y, ", path is", Stack]),
    boku_no_adj([X, Y], Candidates, Stack, Adj),
    reachable([X, Y], Adj, OnGameCells),
    % printall(["calling recursively with", Adj]),
    any_path([ [X, Y] | Stack], Adj, Candidates, OnGameCells, Path).

cc_bfs([X, Y], Candidates, CC):-
    cc_path([ [X, Y] ], [], Candidates, CC).

cc_path([Head|Queue], Visited, Candidates, CC):-
    findall(Adj, boku_no_adj(Head, Candidates, Visited, Adj), Adjs),
    union(Queue, Adjs, NewQueue),
    cc_path(NewQueue, [Head|Visited], Candidates, CC).

cc_path([], Visited, _, Visited).

boku_no_adj([X, Y], [ [HX, HY]|_], Stack, [HX, HY]):-
    not(member([HX, HY], Stack)),
    adjacents(X, Y, HX, HY).
boku_no_adj([X, Y], [_|T], Stack, Adj):- boku_no_adj([X, Y], T, Stack, Adj).

% Hex definition
boku_no_adj(Hex, [Adj|_], Adj):-
    adjacents(Hex, Adj).
boku_no_adj(Hex, [_|T], Adj):-
    boku_no_adj(Hex,T, Adj).


freedom_to_move(Hex, OnGameCells):-
    length(OnGameCells, L),
    get_all(Hex, Type, X, Y, Color, Height, _, Block),
    queen_on_game(OnGameCells, Color),
    find_hex(Hex, OnGameCells, 0, Pos),
    new_hex(Type, X, Y, Color, Height, 0, Block, HexTemp),
    replace_nth0(OnGameCells, Pos, _, HexTemp, OnGameCellsTemp),
    include(is_on_game(), OnGameCellsTemp, OnGameRemaining),
    maplist(get_coordinates, OnGameRemaining, OnGameRemainingCoor),
    adjacents(AdjX, AdjY, X, Y),
    cc_bfs([AdjX, AdjY], OnGameRemainingCoor, CC),
    length(CC, CCNodes), CCNodes is L-1.


test([_, _], Player, Opponent):-
    playerMovements(Player, Opponent, AllMoves),
    write_all(AllMoves),
    maplist(evaluate_movements(Player, Opponent), AllMoves, AllValues),
    write_all(AllMoves),
    printall(["Evaluation of movements:", AllValues]).


    % find_hex([X,Y], Player, Hex),
    % onGameCells(Player, Opponent, OnGameCells),
    % findall(Path, grasshoper_path(Hex, OnGameCells, Path), AllAntPaths),
    % list_to_set(AllAntPaths, AllAntPathsSingle),
    % write_all(AllAntPathsSingle).


% minimax(Player, Opponent, Depth, Move):-
%     onGameCells(Player, Opponent, OnGameCells),
%     playerMovements(Player, Opponent, Movements).


join_paths([], []).
join_paths([Head|Tail], Result):-
    join_paths(Tail, Result1),
    append(Head, Result1, Result).

playerMovements(Player, Opponent, AllMovements):-
    onGameCells(Player, Opponent, OnGameCells),
    maplist(get_hex_moves(OnGameCells), Player, Result),
    join_paths(Result, AllMovements).

get_hex_moves(OnGameCells, Hex, HexMovements):-
    get_type(Hex, "queen"), get_onGame(Hex, 1),
    findall(Path, queen_path(Hex, OnGameCells, Path), HexMovements).
get_hex_moves(OnGameCells, Hex, HexMovements):-
    get_type(Hex, "grasshoper"), get_onGame(Hex, 1),
    findall(Path, grasshoper_path(Hex, OnGameCells, Path), HexMovements).
get_hex_moves(OnGameCells, Hex, HexMovements):-
    get_type(Hex, "beetle"), get_onGame(Hex, 1),
    findall(Path, beetle_path(Hex, OnGameCells, Path), HexMovements).
get_hex_moves(OnGameCells, Hex, HexMovements):-
    get_type(Hex, "spider"), get_onGame(Hex, 1),
    findall(Path, spider_path(Hex, OnGameCells, Path), HexMovements).
get_hex_moves(OnGameCells, Hex, HexMovements):-
    get_type(Hex, "ant"), get_onGame(Hex, 1),
    findall(Path, ant_path(Hex, OnGameCells, Path), HexMovements).
get_hex_moves(OnGameCells, Hex, HexMovements):-
    get_type(Hex, "ladybug"), get_onGame(Hex, 1),
    findall(Path, ladybug_path(Hex, OnGameCells, Path), HexMovements).
get_hex_moves(OnGameCells, Hex, HexMovements):-
    get_type(Hex, "pillbug"), get_onGame(Hex, 1),
    findall(Path, pillbug_path(Hex, OnGameCells, Path), HexMovements).
get_hex_moves(OnGameCells, Hex, HexMovements):-
    get_type(Hex, "mosquito"), get_onGame(Hex, 1),
    findall(Path, mosquito_path(Hex, OnGameCells, Path), HexMovements).

get_hex_moves(_, _, []).


evaluate_movements(Player, Opponent, Movement, Value):- % to use with maplist() over the movement list
    nth0(0, Movement, HexPos),
    % printall(["Evaluating", Movement]),
    last(Movement, DestPos),
    find_hex(HexPos, Player, Hex),
    evaluate_hex_movement(Hex, DestPos, Player, Opponent, Value).

evaluate_hex_movement(hex(Type, Row, Col, Color, Height, OnGame, Block), [X, Y], Player, Opponent, Value):-
    find_hex(hex(Type, Row, Col, Color, Height, OnGame, Block), Player, 0, Pos),
    new_hex(Type, X, Y, Color, Height, OnGame, Block,HexTemp),
    replace_nth0(Player, Pos, _, HexTemp, Player_R),
    evaluate_after_before(Player, Opponent, Player_R, Value).


evaluate_after_before(PlayerBefore, Opponent, PlayerAfter, Value):-
    nth0(0,PlayerBefore, hex(_, _, _, PlayerColor, _, _, _)),
    onGameCells(PlayerBefore, Opponent, OnGameCellsBefore),
    onGameCells(PlayerAfter, Opponent, OnGameCellsAfter),
    surrounding_queen_count(PlayerColor, OnGameCellsBefore, BeforePlayerCount, BeforeOpponentCount),
    surrounding_queen_count(PlayerColor, OnGameCellsAfter, AfterPlayerCount, AfterOpponentCount),
    (
        (
            AfterPlayerCount = 6, Value = -1000
        )
        ;
        (
            AfterOpponentCount = 6, Value = 1000
        )
        ;
        (
            Value is ( BeforePlayerCount - AfterPlayerCount) + ( AfterOpponentCount - BeforeOpponentCount)
        )
    ).

surrounding_queen_count(PlayerColor, OnGameCells, PlayerCount, OpponentCount):-
    find_queen(PlayerColor, OnGameCells, PlayerQueen),
    neighbours(PlayerQueen, OnGameCells, QueenSurrounders), !,
    length(QueenSurrounders, PlayerCount),
    
    opponent_color(PlayerColor, OpponentColor),
    (
        (   % enemy queen in game
            find_queen(OpponentColor, OnGameCells, OpponentQueen),
            neighbours(OpponentQueen, OnGameCells, OpponentSurrounders), !,
            length(OpponentSurrounders, OpponentCount)
        )
        ;
        (   % enemy queen not in game
            OpponentCount = 0    
        )
    
    ).


opponent_color(1, 2).
opponent_color(2, 1).