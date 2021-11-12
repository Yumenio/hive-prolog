get_type(Hex,Type):-       arg(1,Hex,Type).
get_row(Hex,Row):-         arg(2,Hex,Row).
get_col(Hex,Col):-         arg(3,Hex,Col).
get_color(Hex,Color):-     arg(4,Hex,Color).
get_height(Hex,Height):-   arg(5,Hex,Height).
get_onGame(Hex,OnGame):-   arg(6,Hex,OnGame).


printall([]):-
    write("\n").
    printall([X|T]):-
    write(X),
    write(" "),
    printall(T).

adjacents(Hex1,Hex2):-
    get_row(Hex1,Row1),
    get_row(Hex2,Row2),
    get_col(Hex1,Col1),
    get_col(Hex2,Col2),
    ((Col1 is Col2, (Row1 is Row2-1 ; Row1 is Row2+1) );
    (Col1 is Col2+1, (Row1 is Row2 ; Row1 is Row2-1) );
    (Col1 is Col2-1, (Row1 is Row2 ; Row1 is Row2+1) ) ).




new_hex(Type,Row,Col,Color,Height,OnGame, hex(Type,Row,Col,Color,Height, OnGame)).

init_player1(Color,List):-
    new_hex("queen",     0,0,Color,0,1,Queen),
    new_hex("ant",       _,_,Color,0,0,Ant1),
    new_hex("ant",       _,_,Color,0,0,Ant2),
    new_hex("ant",       0,1,Color,0,1,Ant3),
    new_hex("grasshoper",_,_,Color,0,0,Grasshoper1),
    new_hex("grasshoper",1,1,Color,0,1,Grasshoper2),
    new_hex("grasshoper",_,_,Color,0,0,Grasshoper3),
    new_hex("beetle",    _,_,Color,0,0,Beetle1),
    new_hex("beetle",    0,2,Color,0,1,Beetle2),
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
    new_hex("queen",     2,1,Color,0,1,Queen),
    new_hex("ant",       _,_,Color,0,0,Ant1),
    new_hex("ant",       3,0,Color,0,1,Ant2),
    new_hex("ant",       _,_,Color,0,0,Ant3),
    new_hex("grasshoper",_,_,Color,0,0,Grasshoper1),
    new_hex("grasshoper",_,_,Color,0,0,Grasshoper2),
    new_hex("grasshoper",_,_,Color,0,0,Grasshoper3),
    new_hex("beetle",    3,1,Color,0,1,Beetle1),
    new_hex("beetle",    _,_,Color,0,0,Beetle2),
    new_hex("spider",    3,2,Color,0,1,Spider1),
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
%

% devuelve en Hex una celda en juego en las coordenadas X, Y. En caso de no existir devuelve 0.
find_hex(_, _, [], 0).
find_hex(X, Y, [Hex|Tail], Hex):-
    (get_row(Hex, Row1), 
    get_col(Hex, Col1), 
    Row1 is X, Col1 is Y ); find_hex(X, Y, Tail, Hex).

% Devuelve un Hex adjacente a H.
get_adjacent(_, [], 0).
get_adjacent(Hex, [Adj|Tail], Adj):- adjacents(Hex, Adj); get_adjacent(Hex, Tail, Adj).

free_bug_place(_, _, []):-1 is 2.
free_bug_place(T, Color, [Hex|Tail]):-
    (get_type(Hex, T1), get_onGame(Hex, OnGame), get_color(Hex, C),
    C is Color, T1 is T, OnGame is 0) ; free_bug_place(T, Color, Tail).

ocuppied( _, _, []).
occupied(X, Y, [Hex|Tail]):-
    (get_row(Hex, R1), get_col(Hex, C1),
    R1 is X, C1 is Y) ; occupied(X, Y, Tail).

valid_place(Color).

% Cells es celdas en juegos de ambos players
can_place_hex(Turn, Type, X, Y, Color, Cells):-
    not(occupied(X, Y, Cells)),
    free_bug_place(Type, Color, Cells), 
    valid_place(Color).

place_hex(Type, X, Y, Color, Place, Other, Player1_R, Player2_R):-
    append(Place, Other, Cells),
    can_place_hex(Type, X, Y, Color, Cells).

% DFS stuffs
neighbours(_, [], []).
neighbours(Hex, [Nb|Tail], Nbs):- (adjacents(Hex, Nb), neighbours(Hex, Tail, Nbs_), append([Nb], Nbs_, Nbs)); neighbours(Hex, Tail, Nbs).
%% dfs starting from a root 
dfs(Root, OnGameCells, Result):-
    dfs([Root], OnGameCells, [], Result).
%% Done, all visited
dfs([], _, Result, Result):- length(Result, Length), write(Length).
%% Skip elements that are already visited
dfs([Hex|Tail], OnGameCells, Visited, Result):-
    member(Hex, Visited),
    dfs(Tail, OnGameCells, Visited, Result).
%% add all adjacents
dfs([H|T], L, Visited, T1):-
    not(member(H, Visited)),
    move(H, L, Nbs),
    append(Nbs, T, ToVisit),
    dfs(ToVisit, L, [H|Visited], T1).

parse_input_put(Raw_input, Type, Col, Row):-
    split_string(Raw_input,"\s","\s",Input),
    nth0(0,Input,Type),
    nth0(1,Input,C1),
    nth0(2,Input,R1),
    atom_number(C1,Col),
    atom_number(R1,Row).

place_hex(Type,Row,Col,Color,Player1,Player2,NewPlayer1).

init_game():-
    players(Player1,Player2),
    game(Player1,Player2).

game(Player1,Player2):-
    turn_player1(Player1, Player2, NewPlayer1),
    turn_player2(Player1, Player2, NewPlayer2),
    game(NewPlayer1,NewPlayer2).

turn_player1(Player1, Player2, NewPlayer1):-
    read_line_to_string(user_input, Raw_input),
    split_string(Raw_input,"\s","\s",Input),
    ( % caso poner ficha
    (length(Input,L1), L1 is 3,
    parse_input_put(Raw_input,Type,Row,Col),
    % printall([Type,Row,Col]),
    place_hex(Type,Row,Col,1,Player1,Player2,NewPlayer1) );
    
    % caso mover ficha
    (length(Input,L2), L2 is 2);
    
    % caso no válido
    ( write("Invalid input, please try again\n") )
    ).

turn_player2(Player1, Player2, NewPlayer2):-
    read_line_to_string(user_input, Raw_input),
    split_string(Raw_input,"\s","\s",Input),
    ( % caso poner ficha
    (length(Input,L1), L1 is 3,
    parse_input_put(Raw_input,Type,Row,Col),
    % printall([Type,Row,Col]),
    place_hex(Type,Row,Col,2,Player1,Player2,NewPlayer2) );
    
    % caso mover ficha
    (length(Input,L2), L2 is 2);
    
    % caso no válido
    ( write("Invalid input, please try again\n") )
    ).
