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
    write("asd"),
    write("\n").
printall([X|T]):-
    write(X),
    write(" "),
    printall(T).

successor(X, Y):- Y is X + 1.

replace_nth0(List, Index, OldElem, NewElem, NewList) :-
    nth0(Index,List,OldElem,Transfer),
    nth0(Index,NewList,NewElem,Transfer).

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
    new_hex("pillBug",   _,_,Color,0,0,PillBug),
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
    new_hex("pillBug",   _,_,Color,0,0,PillBug),
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

onGameSingle([], []).
onGameSingle([Hex|Tail], List):- get_onGame(Hex, OnGame), ((OnGame is 1, onGameSingle(Tail, L), append([Hex], L, List)) ; onGameSingle(Tail, L), append([], L, List)).

onGameCells(Player1, Player2, List):- onGameSingle(Player1, L3), 
                            onGameSingle(Player2, L4), append(L3, L4, List).

% test area
%llamar_dfs(Adj):- onGameCells(L), nth0(0, L, R), dfs(R, L, V), nth0(0, V, Hex), write(Hex), get_adjacent(Hex, L, Adj).
% llamar_dfs(Hex):- onGameCells(L), find_hex(1,1, L, Hex).
% :- debug.
% compare(X, Y):- X = Y; X is Y.
% compare_things([],[]).
% compare_things([H1|T1], [H2|T2]):-write(H1), write(H2),compare(H1,H2), compare_things(T1, T2).

% devuelve en Hex una celda en juego en las coordenadas X, Y. En caso de no existir devuelve 0.
find_hex(_, _, [], _):- 2 is 1.
find_hex(X, Y, [Hex|Tail], Hex):-
    (get_row(Hex, Row1), 
    get_col(Hex, Col1),
    get_onGame(Hex, OG1),
    Row1 is X, Col1 is Y, OG1 is 1 ); find_hex(X, Y, Tail, Hex).

find_hex(_, [], _, -1):- 2 is 1.
find_hex(Hex, [H|T], Index, Pos):-
    get_all(Hex, Type, Row, Col, Color, Height, _),
    get_all(H, Type1, Row1, Col1, Color1, Height1, O),
    (Type1 is Type, Row1 is Row, Type1 is Type,
    Col1 is Col, Color1 is Color, Height1 is Height, O is 1, Pos is Index); 
    (successor(Index, Index1), find_hex(Hex, T, Index1, Pos)).

% Devuelve un Hex adjacente a H.
get_adjacent(_, [], 0).
get_adjacent(Hex, [Adj|Tail], Adj):- adjacents(Hex, Adj); get_adjacent(Hex, Tail, Adj).


free_bug_place(_, _, []):-1 is 2.
free_bug_place(T, Color, [Hex|Tail]):-
    (get_type(Hex, T1), get_onGame(Hex, OnGame), get_color(Hex, C),
    C is Color, T1 = T, OnGame is 0) ; free_bug_place(T, Color, Tail).

ocuppied( _, _, []).
occupied(X, Y, [Hex|Tail]):-
    (get_row(Hex, R1), get_col(Hex, C1),
    R1 is X, C1 is Y ) ; occupied(X, Y, Tail).

all_same_color(_, []).
all_same_color(Color, [H|T]):-
    get_color(H, C1), 
    C1 is Color, 
    all_same_color(Color, T).

valid_place(Cell, Cells):- 
    onGameSingle(Cells, OnGameCells), 
    neighbours(Cell, OnGameCells, Nbs), !,
    length(Nbs, L), L > 0,
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
    onGameSingle(Cells,OnGameCells),
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
    onGameSingle(Player1,Board11),
    % printall(Board11),
    write("Turn Player1:\n"),
    turn_player1(Turn, Player1, Player2, NewPlayer1),
    onGameSingle(NewPlayer1,Board12),
    printall(Board12),
    onGameSingle(Player2,Board21),
    % printall(Board21),
    write("Turn Player2:\n"),
    turn_player2(Turn, Player1, Player2, NewPlayer2),
    onGameSingle(NewPlayer2,Board22),
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
    parse_input_move(Raw_input,R1,C1,R2,C2)
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
    (length(Input,L2), L2 is 2);
    
    % caso no válido
    (write("\nInvalid input, please try again\n"),
    turn_player2(Turn, Player1, Player2, NewPlayer2))
    ).


%---------------------- Moves ----------------------

move_hex(X, Y, X1, Y1, Player, Opponent, Player_R):-
    % onGameCells(Player, Opponent, OnGameCells),
    find_hex(X, Y, Player, Hex),
    get_type(Hex, T), 
    (T = "queen", queen_move(Hex, X1, Y1, Player, Opponent, Player_R)).

can_move(Hex1, OnGameCells):-
    length(OnGameCells, L),
    get_all(Hex1, T, X, Y, C, _,_), 
    neighbours(Hex1, OnGameCells, Nbs),
    nth0(Nbs, 0, Nb),
    new_hex(T, X, Y, C, 0, 0, New_Hex),
    find_hex(Hex1, OnGameCells, 0, Pos),
    replace_nth0(OnGameCells, Pos, _, New_Hex, OG),
    onGameSingle(OG, OGC),
    dfs(Nb, OGC, Result),
    length(Result, L1), L1 is L-1.


queen_move(Hex1, X, Y, Player, Opponent, Player_R):-
    onGameCells(Player, Opponent, OnGameCells),
    not(occupied(X, Y, OnGameCells)),
    get_color(Hex1, C), 
    new_hex("queen", X, Y, C, 0, 1, Hex2),
    adjacents(Hex1, Hex2),
    can_move(Hex1, OnGameCells),
    find_hex(Hex1, Player, 0, Pos),
    replace_nth0(Player, Pos, _, Hex2, Player_R).
    %Faltaria verificar que puede meterse ahi.

