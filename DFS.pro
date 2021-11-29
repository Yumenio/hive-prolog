% import(list_utils).

get_type(Hex,Type):-       arg(1,Hex,Type).
get_row(Hex,Row):-         arg(2,Hex,Row).
get_col(Hex,Col):-         arg(3,Hex,Col).
get_color(Hex,Color):-     arg(4,Hex,Color).
get_height(Hex,Height):-   arg(5,Hex,Height).
get_onGame(Hex,OnGame):-   arg(6,Hex,OnGame).

get_all(Hex, T, X, Y, C, H, O):- 
    get_type(Hex,T),
    get_row(Hex,X),
    get_col(Hex,Y),
    get_color(Hex,C),
    get_height(Hex,H),
    get_onGame(Hex,O).


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




new_hex(Type,Row,Col,Color,Height,OnGame, hex(Type,Row,Col,Color,Height, OnGame)).

init_player1(Color,List):-
    new_hex("queen",     _,_,Color,0,0,Queen),
    new_hex("ant",       _,_,Color,0,0,Ant1),
    new_hex("ant",       _,_,Color,0,0,Ant2),
    new_hex("ant",       _,_,Color,0,0,Ant3),
    new_hex("grasshoper",_,_,Color,0,0,Grasshoper1),
    new_hex("grasshoper",_,_,Color,0,0,Grasshoper2),
    new_hex("grasshoper",_,_,Color,0,0,Grasshoper3),
    new_hex("beetle",    _,_,Color,0,0,Beetle1),
    new_hex("beetle",    _,_,Color,0,0,Beetle2),
    new_hex("spider",    _,_,Color,0,0,Spider1),
    new_hex("spider",    _,_,Color,0,0,Spider2),
    new_hex("mosquito",  _,_,Color,0,0,Mosquito),
    new_hex("pillbug",   _,_,Color,0,0,PillBug),
    new_hex("ladybug",   _,_,Color,0,0,Ladybug),
    append([], [Queen, Ant1, Ant2, Ant3, 
                Grasshoper1, Grasshoper2, 
                Grasshoper3, Beetle1, Beetle2, 
                Spider1, Spider2, Mosquito, PillBug, Ladybug], List).
init_player2(Color,List):-
    new_hex("queen",     _,_,Color,0,0,Queen),
    new_hex("ant",       _,_,Color,0,0,Ant1),
    new_hex("ant",       _,_,Color,0,0,Ant2),
    new_hex("ant",       _,_,Color,0,0,Ant3),
    new_hex("grasshoper",_,_,Color,0,0,Grasshoper1),
    new_hex("grasshoper",_,_,Color,0,0,Grasshoper2),
    new_hex("grasshoper",_,_,Color,0,0,Grasshoper3),
    new_hex("beetle",    _,_,Color,0,0,Beetle1),
    new_hex("beetle",    _,_,Color,0,0,Beetle2),
    new_hex("spider",    _,_,Color,0,0,Spider1),
    new_hex("spider",    _,_,Color,0,0,Spider2),
    new_hex("mosquito",  _,_,Color,0,0,Mosquito),
    new_hex("pillbug",   _,_,Color,0,0,PillBug),
    new_hex("ladybug",   _,_,Color,0,0,Ladybug),
    append([], [Queen, Ant1, Ant2, Ant3, 
                Grasshoper1, Grasshoper2, 
                Grasshoper3, Beetle1, Beetle2, 
                Spider1, Spider2, Mosquito, PillBug, Ladybug], List).

players(Player1,Player2):-
    player1(Player1),
    player2(Player2).

player1(List):- init_player1(1, List).
player2(List):- init_player2(2, List).

list_print([]):- write("\n").
list_print([Hex|Tail]):- write(Hex), write(" "), list_print(Tail).

% onGameSingle([], []).
% onGameSingle([Hex|Tail], List):- get_onGame(Hex, OnGame), ((OnGame is 1, onGameSingle(Tail, L), append([Hex], L, List)) ; onGameSingle(Tail, L), append([], L, List)).

% onGameCells(Player1, Player2, List):- onGameSingle(Player1, L3), 
%                             onGameSingle(Player2, L4), append(L3, L4, List).

onGameCells(Player1, Player2, Result):-
    include(is_on_game(), Player1, OnGamePlayer1),
    include(is_on_game(), Player2, OnGamePlayer2),
    append(OnGamePlayer1, OnGamePlayer2, Result).

is_on_game(Hex):- get_onGame(Hex, OG), OG is 1.

% test area
%llamar_dfs(Adj):- onGameCells(L), nth0(0, L, R), dfs(R, L, V), nth0(0, V, Hex), write(Hex), get_adjacent(Hex, L, Adj).
% llamar_dfs(Hex):- onGameCells(L), find_hex(1,1, L, Hex).
% :- debug.
% compare(X, Y):- X = Y; X is Y.
% compare_things([],[]).
% compare_things([H1|T1], [H2|T2]):-write(H1), write(H2),compare(H1,H2), compare_things(T1, T2).

% devuelve en Hex una celda en juego en las coordenadas X, Y. En caso de no existir devuelve falso.

% new_hex(Type,Row,Col,Color,Height,OnGame, hex(Type,Row,Col,Color,Height, OnGame)).
test_f(A):-
    new_hex("queen",  1, 1, 1, 0, 1, Hex1),
    new_hex("queen",  1, 2, 1, 0, 1, Hex2),
    new_hex("beetle", 1, 1, 1, 1, 1, Hex3),
    new_hex("beetle", 1, 1, 1, 2, 1, Hex4),
    new_hex("beetle", 1, 1, 1, 3, 1, Hex5),
    new_hex("ant",    2, 1, 1, 0, 1, Hex6),
    Hexs = [Hex1, Hex2, Hex3, Hex4, Hex5, Hex6],
    find_hex([1, 1], Hexs, A).

find_hex(Pos, L, Hex1):-
    findall(Hex, find_all_at(Pos, L, Hex), Hexs),
    length(Hexs, Len), Len > 0,
    nth0(0, Hexs, H),
    higher(Hexs, H, Hex1).

higher([], Ch, Ch):- write(Ch).
higher([Head|Tail], Current_Higher, Higher):-
    get_height(Current_Higher, H), get_height(Head, H1), 
    (
    (H1 >= H, higher(Tail, Head, High1), get_height(High1, Higher1), 
    ((H1 >= Higher1, Higher = Head); (Higher = High1)));

    (H >= H1, higher(Tail, Current_Higher, High1), get_height(High1, Higher1), 
    ((H >= Higher1, Higher = Current_Higher); (Higher = High1)))
    %podria faltar el caso en que H1 == H

    ).

find_all_at(Pos, [Hex|Tail], Hex1):- 
    length(Pos, L), L is 2,
    nth0(0, Pos, X),
    nth0(1, Pos, Y),
    ((get_all(Hex, _, Row1, Col1, _, _, OG1),
    Row1 is X, Col1 is Y, OG1 is 1, 
    Hex1 = Hex); 
    find_all_at(Pos, Tail, Hex1)).

find_hex(_, [], _, -1):- 2 is 1.
find_hex(Hex, [H|T], Index, Pos):-
    get_all(Hex, Type, Row, Col, Color, Height, _),
    get_all(H, Type1, Row1, Col1, Color1, Height1, O),
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

p(X):- write(X).

queen_on_game([H|T], Color):-
    get_color(H, C),
    get_onGame(H, O), 
    get_type(H, T1), 
    ((C is Color,
    T1 = "queen",
    O is 1);
    queen_on_game(T, Color)).

valid_state(Cells, Turn, Color, Type):-
    queen_on_game(Cells, Color); Turn < 4; (Turn is 4, Type = "queen").

% find_free_bug(_, [], -1).
% find_free_bug(Type, [H|T], Pos):-
%     (get_type(H, T), T is Type, length(T, L), Pos is L+1); 
%     (find_free_bug(Type, T, Pos)).


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
    new_hex(Type, X, Y, Color, _, 0, Hex),
    valid_place(Hex, Cells),
    valid_state(Cells, Turn, Color, Type).

place_hex(Turn, Type, X, Y, Color, Player1, Player2, Player_R):-
    append(Player1, Player2, Cells),
    can_place_hex(Turn, Type, X, Y, Color, Cells),
    new_hex(Type, X, Y, Color, 0, 1, Hex),
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
    new_hex(Type,Row,Col,1,0,1,Hex),
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
    new_hex(Type, Row, Col, 2, 0, 1, Hex_), adjacents(Hex, Hex_),
    find_free_bug(Type, Player2, 0, Pos),
    replace_nth0(Player2, Pos, _, Hex_, Player2_R) );
    (write("You did something wrong, try again\n"),
    second_placed(Hex, Player2, Player2_R))
    ).

get_coordinates(Hex, Coor):- get_row(Hex, Row), get_col(Hex, Col), Coor = [Row, Col].

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
    % onGameSingle(Player1,Board11),
    % printall(Board11),
    write("Turn Player1:\n"),
    turn_player1(Turn, Player1, Player2, NewPlayer1),
    % onGameSingle(NewPlayer1,Board12),
    include(is_on_game(), NewPlayer1, Board12),
    printall(Board12),
    % onGameSingle(Player2,Board21),
    % printall(Board21),
    write("Turn Player2:\n"),
    turn_player2(Turn, Player1, Player2, NewPlayer2),
    % onGameSingle(NewPlayer2,Board22),
    include(is_on_game(), NewPlayer2, Board22),
    printall(Board22),
    successor(Turn,Turn1),
    game(NewPlayer1,NewPlayer2, Turn1).


turn_player1(Turn, Player1, Player2, NewPlayer1):-
    read_line_to_string(user_input, Raw_input),
    split_string(Raw_input,"\s","\s",Input),
    (
    % caso poner ficha
    (length(Input,L1), L1 is 3,
    parse_input_place(Raw_input,Type,Row,Col),
    % printall([Type,Row,Col]),
    place_hex(Turn, Type,Row,Col,1,Player1,Player2,NewPlayer1) );
    
    % caso mover ficha
    (length(Input,L2), L2 is 4,
    parse_input_move(Raw_input,R1,C1,R2,C2),
    move_hex(R1, C1, R2, C2, Player1, Player2, NewPlayer1)
    );
    
    % caso no válido
    (write("\nInvalid input, please try again\n"),
    turn_player1(Turn, Player1, Player2, NewPlayer1))
    ).

turn_player2(Turn, Player1, Player2, NewPlayer2):-
    read_line_to_string(user_input, Raw_input),
    split_string(Raw_input,"\s","\s",Input),
    ( % caso poner ficha
    (length(Input,L1), L1 is 3,
    parse_input_place(Raw_input,Type,Row,Col),
    % printall([Type,Row,Col]),
    place_hex(Turn, Type,Row,Col,2,Player1,Player2,NewPlayer2) );
    
    % caso mover ficha
    (length(Input,L2), L2 is 4,
    parse_input_move(Raw_input,R1,C1,R2,C2),
    move_hex(R1, C1, R2, C2, Player2, Player1, NewPlayer2)
    );

    % caso no válido
    (write("\nInvalid input, please try again\n"),
    turn_player2(Turn, Player1, Player2, NewPlayer2))
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
    (T = "spider", spider_move(Hex, X1, Y1, Player, Opponent, Player_R))).

have_adjacent(_, _, []).
have_adjacent(X, Y, [Hex|Tail]):-
    get_row(Hex, X1), get_col(Hex, Y1), 
    (adjacents(X, Y, X1, Y1);have_adjacent(X, Y, Tail)).

can_move(Hex1, X1, Y1, OnGameCells):-
    length(OnGameCells, L),
    get_all(Hex1, T, X, Y, C, _,_), 
    neighbours(Hex1, OnGameCells, Nbs),
    nth0(0, Nbs, Nb),
    new_hex(T, X, Y, C, 0, 0, New_Hex),
    find_hex(Hex1, OnGameCells, 0, Pos),
    replace_nth0(OnGameCells, Pos, _, New_Hex, OG),
    % onGameSingle(OG, OGC),
    include(is_on_game(), OG, OGC),
    dfs(Nb, OGC, Result),
    length(Result, L1), L1 is L-1,
    have_adjacent(X1, Y1, OnGameCells).


queen_move(Hex1, X, Y, Player, Opponent, Player_R):-
    onGameCells(Player, Opponent, OnGameCells),
    not(occupied(X, Y, OnGameCells)),
    get_color(Hex1, C), 
    new_hex("queen", X, Y, C, 0, 1, Hex2),
    adjacents(Hex1, Hex2),
    can_move(Hex1, X, Y, OnGameCells),
    find_hex(Hex1, Player, 0, Pos),
    replace_nth0(Player, Pos, _, Hex2, Player_R).
    %Faltaria verificar que puede meterse ahi.

ant_move(Hex1, X, Y, Player, Opponent, Player_R):-
    onGameCells(Player, Opponent, OnGameCells),
    not(occupied(X, Y, OnGameCells)),
    get_all(Hex1, T, Row, Col, C, _, _),
    can_move(Hex1, X, Y, OnGameCells),
    new_hex(T, X, Y, C, 0, 1, Hex2),

    delete(OnGameCells, Hex1, OnGameCellsAux),

    vecinos_void(OnGameCellsAux, [], OnGameCellsAux, Free_Cells),
    length(Free_Cells, L),
    % halve(L, L2),
    find_all_paths(OnGameCellsAux, Row, Col, L, Paths), !,
    valid_paths(X, Y, Paths, ValidPaths),
    write("Found:\n"),
    write_all(ValidPaths),
    length(ValidPaths, LVP),
    LVP > 0,
    find_hex(Hex1, Player, 0, Pos),
    replace_nth0(Player, Pos, _, Hex2, Player_R).

spider_move(Hex1, X, Y, Player, Opponent, Player_R):-
    onGameCells(Player, Opponent, OnGameCells),
    not(occupied(X, Y, OnGameCells)),
    get_all(Hex1, T, Row, Col, C, _, _),
    can_move(Hex1, X, Y, OnGameCells),
    new_hex(T, X, Y, C, 0, 1, Hex2),
    
    delete(OnGameCells, Hex1, OnGameCellsAux),

    find_all_paths(OnGameCellsAux, Row, Col, 4, Paths), !,
    valid_paths(X, Y, Paths, ValidPaths),
    write("ALL:\n"),
    write(ValidPaths),
    include(path_of_length_3(), ValidPaths, ValidPathsL3),
    write("Found:\n"),
    write_all(ValidPathsL3),
    length(ValidPathsL3, LVP),
    write(LVP), write("\n"),
    LVP > 0,
    find_hex(Hex1, Player, 0, Pos),
    replace_nth0(Player, Pos, _, Hex2, Player_R).

grasshoper_move(Hex1, X, Y, Player, Opponent, Player_R):-
    onGameCells(Player, Opponent, OnGameCells),
    not(occupied(X, Y, OnGameCells)),
    get_all(Hex1, T, _, _, C, _, _),
    can_move(Hex1, X, Y, OnGameCells), !,
    new_hex(T, X, Y, C, 0, 1, Hex2),

    find_grasshoper_paths(Hex1, OnGameCells, Paths),
    valid_paths(X, Y, Paths, ValidPaths),
    length(ValidPaths, LVP),
    LVP > 0,
    find_hex(Hex1, Player, 0, Pos),
    replace_nth0(Player, Pos, _, Hex2, Player_R).

beetle_move(Hex1, X, Y, Player, Opponent, Player_R):-
    onGameCells(Player, Opponent, OnGameCells),
    not(occupied(X, Y, OnGameCells)),
    get_color(Hex1, C),
    new_hex("beetle", X, Y, C, 0, 1, Hex2),
    adjacents(Hex1, Hex2),
    can_move(Hex1, X, Y, OnGameCells),
    find_hex(Hex1, Player, 0, Pos),
    replace_nth0(Player, Pos, _, Hex2, Player_R).
    
beetle_move(Hex1, X, Y, Player, Opponent, Player_R):-
    onGameCells(Player, Opponent, OnGameCells),
    occupied(X, Y, OnGameCells),
    find_hex([X,Y],OnGameCells, OccupiedHex),
    adjacents(Hex1, OccupiedHex),
    get_color(Hex1, C), get_height(OccupiedHex, H),
    successor(H, H1),
    new_hex("beetle", X, Y, C, H1, 1, Hex2),
    can_move(Hex1, X, Y, OnGameCells), !,
    find_hex(Hex1, Player, 0, Pos),
    replace_nth0(Player, Pos, _, Hex2, Player_R).
    

ladybug_move(Hex1, X, Y, Player, Opponent, Player_R):-
    onGameCells(Player, Opponent, OnGameCells),
    not(occupied(X, Y, OnGameCells)),
    get_all(Hex1, T, Row, Col, C, _, _),
    can_move(Hex1, X, Y, OnGameCells),
    new_hex(T, X, Y, C, 0, 1, Hex2),

    delete(OnGameCells, Hex1, OnGameCellsAux),

    vecinos_void(OnGameCellsAux, [], OnGameCellsAux, Free_Cells),
    maplist(get_coordinates, OnGameCellsAux, OnGameCellsAuxCoordinates),
    append(Free_Cells, OnGameCellsAuxCoordinates, AllLadybugPathCells),
    length(Free_Cells, L),
    find_all_paths(AllLadybugPathCells, Row, Col, L, Paths), !,
    valid_paths(X, Y, Paths, ValidPaths),
    write("Found:\n"),
    write_all(ValidPaths),
    length(ValidPaths, LVP),
    LVP > 0,
    find_hex(Hex1, Player, 0, Pos),
    replace_nth0(Player, Pos, _, Hex2, Player_R).

find_grasshoper_paths(Hex, OnGameCells, Paths):-
    get_row(Hex, Row), get_col(Hex, Col),
    % successor(Row, Row1), predecessor(Row,Row_1),
    % successor(Col, Col1), predecessor(Col, Col_1),
    straight_line(Row, Col, 1, 0, OnGameCells,[], R10),
    straight_line(Row, Col, 0, 1, OnGameCells,[], R01),
    straight_line(Row, Col, -1, 1, OnGameCells, [], R_11),
    straight_line(Row, Col, -1, 0, OnGameCells, [], R_10),
    straight_line(Row, Col, 0, -1, OnGameCells, [], R0_1),
    straight_line(Row, Col, 1, -1, OnGameCells, [], R1_1),
    append([[R10],[R01],[R_11],[R_10],[R0_1],[R1_1]], AllPaths),
    include(path_greater_than_2(), AllPaths, Paths).

straight_line(Row, Col, _, _, OnGameCells, Acc, R):-
    write("Checking for not occupied\n"),
    not(occupied(Row, Col, OnGameCells)),
    printall(["End of line at", Row, Col, "appending to ", Acc]),
    append(Acc, [[Row,Col]],R).

straight_line(Row, Col, DirRow, DirCol, OnGameCells, Acc, R):-
    write("Checking for occupied\n"),
    occupied(Row, Col, OnGameCells),
    printall(["Jumping over", Row, Col, "appending to", Acc ]),
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


vecino(Hex, Cells, Voids, A):-
    get_all(Hex, _, X1, Y1, _, _, _),
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
    neighbours(X1, Y1, L, V1, Nbs),
    % append(Nbs, T, ToVisit),
    predecessor(Deep, Deep1),
    dfs_path(Nbs, Deep1, L, [H|Visited], T1).

true_path(X, Y,  Head):-
    length(Head, L), predecessor(L, P), nth0(P, Head, H), 
    nth0( 0, H, X1), nth0( 1, H, Y1),
    X1 is X, Y1 is Y.

valid_paths(X, Y, Paths, ValidPaths):-
    include(true_path(X,Y), Paths, ValidPaths).
%((X1 is X, Y1 is Y, true_path(X, Y, Tail, H1), write("primer caso"), append([Head], H1, H));
%(write("segundo caso"), true_path(X, Y, Tail, H1),  H = H1)).

% is_valid_path(1, 2, [[[1,2],[1,2],[1,2]], [[3,4],[1,2],[1,2]], [[3,4],[1,2],[0,2]]], H).