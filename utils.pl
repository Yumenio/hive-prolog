:- module(utils, 
    [
    get_type/2, get_row/2, get_col/2, get_color/2, get_height/2, get_onGame/2, get_blocked/2, get_all/8, 
    init_player1/2, init_player2/2, parse_input_place/4, parse_input_move/5, parse_input_special/7,
    path_greater_than_2/1, have_adjacent/3, get_converted_cells/2, all_same_color/2, occupied/3,
    concat_list/2, get_first_letter/2, get_color_letter/2, successor/2, predecessor/2, resta/2,
    check_color/2, get_coordinates/2, unblock/2, check_for_highests/3, empty_neighbours/4, 
    is_nb/4, boku_no_adj/4, boku_no_adj/3, valid_paths/4, reachable/3, valid_path_end/2,
    higher/3, replace_nth0/5, reverse_all/2, sum/2, printall/1, write_all/1, new_hex/8,
    adjacents/4, adjacents/2, players/2, is_on_game/1, onGameCells/3, 
    

    there_is_a_path/3, path_of_length_3/1
    ]).

adjacents(hex(_, Row1, Col1, _, _, _, _), hex(_, Row2, Col2, _, _, _, _)):- 
    ((Col1 is Col2, (Row1 is Row2-1 ; Row1 is Row2+1) );
    (Col1 is Col2+1, (Row1 is Row2 ; Row1 is Row2-1) );
    (Col1 is Col2-1, ( Row1 is Row2 ; Row1 is Row2+1) )).

players(Player1,Player2):-
    player1(Player1),
    player2(Player2).

player1(List):- init_player1(1, List).
player2(List):- init_player2(2, List).

onGameCells(Player1, Player2, Result):-
    include(is_on_game(), Player1, OnGamePlayer1),
    include(is_on_game(), Player2, OnGamePlayer2),
    append(OnGamePlayer1, OnGamePlayer2, Result).

is_on_game(Hex):- get_onGame(Hex, OG), OG is 1.

get_adjacent(_, [], 0).
get_adjacent(Hex, [Adj|Tail], Adj):- adjacents(Hex, Adj); get_adjacent(Hex, Tail, Adj).

occupied( _, _, []):- false().
occupied(X, Y, [hex(_, Row, Col, _, _, _, _)|Tail]):-
    Row is X, Col is Y  ; occupied(X, Y, Tail).

all_same_color(_, []).
all_same_color(C, [hex(_, _, _, Color, _, _, _)|Tail]):-
    Color is C, all_same_color(Color, Tail).

convert_cells(hex(Type, Row, Col, Color, _, _, _), Converted_Hex):-
    get_color_letter(Color, C), get_first_letter(Type, Letter), 
    concat(C, Letter, Name),  Converted_Hex = [Row, Col, Name].

get_converted_cells(Cells, Converted_Cells):-
    check_for_highests(Cells, Cells, CC),
    maplist(convert_cells, CC, Converted_Cells).

path_of_length_3(X):- length(X, L), L = 4. % 4 because length of a path is |Path|-1
path_greater_than_2(X) :- length(X, L), L > 2.

there_is_a_path(_, _, []):- false().
there_is_a_path(X, Y, [H|T]):-
    (last(H, L), nth0(0, L, HX), HX = X, nth0(1, L, HY), HY = Y, ! );
    there_is_a_path(X, Y, T).

true_path(X, Y,  Head):-
    length(Head, L), predecessor(L, P), nth0(P, Head, [X1, Y1]),
    X1 is X, Y1 is Y.

valid_paths(X, Y, Paths, ValidPaths):-
    include(true_path(X,Y), Paths, ValidPaths).

valid_path_end(Path, DestHex):-
    last(Path, Last), Last = DestHex.

have_adjacent(_, _, []):-false().
have_adjacent(X, Y, [hex(_, Row, Col, _, _, _, _)|Tail]):-
    adjacents(X, Y, Row, Col); have_adjacent(X, Y, Tail).

reachable( [From_x, From_y], [To_x, To_y], OnGameCells):-
    adjacents(CommonAdj_x, CommonAdj_y, From_x, From_y),
    adjacents(CommonAdj_x, CommonAdj_y, To_x, To_y),
    not(occupied(CommonAdj_x, CommonAdj_y, OnGameCells)).

boku_no_adj([X, Y], [ [HX, HY]|_], Stack, [HX, HY]):-
    not(member([HX, HY], Stack)),
    adjacents(X, Y, HX, HY).
boku_no_adj([X, Y], [_|T], Stack, Adj):- 
    boku_no_adj([X, Y], T, Stack, Adj).

% Hex definition
boku_no_adj(Hex, [Adj|_], Adj):-
    adjacents(Hex, Adj).
boku_no_adj(Hex, [_|T], Adj):-
    boku_no_adj(Hex,T, Adj).

new_hex(Type, Row, Col, Color, Height, OnGame, Blocked, hex(Type, Row, Col, Color, Height, OnGame, Blocked)).

check_color(hex(_, _, _, Color, _, _, _), [hex(_, _, _, Color, _, _, _)|_]).

get_coordinates(hex(_, Row, Col, _, _, _, _), [Row, Col]).

get_type(Hex, Type):-       arg(1,Hex, Type).
get_row(Hex, Row):-         arg(2,Hex, Row).
get_col(Hex, Col):-         arg(3,Hex, Col).
get_color(Hex, Color):-     arg(4,Hex, Color).
get_height(Hex, Height):-   arg(5,Hex, Height).
get_onGame(Hex, OnGame):-   arg(6,Hex, OnGame).
get_blocked(Hex, Blocked):- arg(7,Hex, Blocked).

reduce_block(hex(Type, Row, Col, Color, Height, OnGame, X), hex(Type, Row, Col, Color, Height, OnGame, XB)):- X > 0, XB is X+1.
reduce_block(hex(Type, Row, Col, Color, Height, OnGame,0), hex(Type, Row, Col, Color, Height, OnGame,0)).

unblock(BlockedHex, UnblockedHex):- maplist(reduce_block(), BlockedHex, UnblockedHex).

init_player1(Color, [Queen, Ant1, Ant2, Ant3, Grasshoper1, Grasshoper2, Grasshoper3, Beetle1, 
    Beetle2, Spider1, Spider2, Mosquito, PillBug, Ladybug]):-
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
    new_hex("ladybug",   _,_,Color,0,0,0,Ladybug).

init_player2(Color, [Queen, Ant1, Ant2, Ant3, Grasshoper1, Grasshoper2, Grasshoper3, Beetle1, 
    Beetle2, Spider1, Spider2, Mosquito, PillBug, Ladybug]):-
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
    new_hex("ladybug",   _,_,Color,0,0,0,Ladybug).

check_coordinates(X, Y, hex(_, Row, Col, _, _, _, _)):-
    Row is X, Col is Y.
check_for_highests([], _, []).
check_for_highests([hex(_, Row, Col, _, _, _, _)|Tail], L, Result):-
    include(check_coordinates(Row, Col), L, Filtered),
    length(Filtered, Len), Len is 1, check_for_highests(Tail, L, R), 
    append(Filtered, R, Result).
check_for_highests([hex(_, Row, Col, _, _, _, _)|Tail], L, Result):-
    include(check_coordinates(Row, Col), L, Filtered),
    length(Filtered, Len), Len > 1, nth0(0, Filtered, Current_Higher),
    higher(Filtered, Current_Higher, Higher),
    check_for_highests(Tail, L, R), 
    append([Higher], R, Result).

get_all(Hex, T, X, Y, C, H, O, B):- 
    get_type(Hex,T), get_row(Hex,X),
    get_col(Hex,Y), get_color(Hex,C),
    get_height(Hex,H), get_onGame(Hex,O),
    get_blocked(Hex,B).

get_first_letter(T, L):- sub_atom(T, 0, 1, _, Letter), string_upper(Letter, L).
get_color_letter(C, L):- C is 1, L = "W".
get_color_letter(C, L):- C is 2, L = "B".
%falta cambiarle el nombre a estos
vecino(hex(_, Row, Col, _, _, _, _), Cells, Voids, [X, Y]):-
    adjacents(X, Y, Row, Col), not(occupied(X, Y, Cells)),
    not(member([X, Y], Voids)).

vecinos(Hex,  Empties, Cells, Vecinos):-
    findall(Nb, vecino(Hex, Cells, Empties, Nb), Vecinos).

empty_neighbours([],_, _, []).
empty_neighbours([Hex|Tail], Empties, Cells, V):-
    vecinos(Hex, Empties, Cells, V1),
    append(Empties, V1, Empties1),
    empty_neighbours(Tail, Empties1, Cells, V2),
    append(V1, V2, V).

higher([], Ch, Ch).
higher([Head|Tail], Current_Higher, Higher):-
    get_height(Current_Higher, H), get_height(Head, H1), 
    (
    (H1 >= H, higher(Tail, Head, High1), get_height(High1, Higher1), 
    ((H1 >= Higher1, Higher = Head); (Higher = High1)));

    (H >= H1, higher(Tail, Current_Higher, High1), get_height(High1, Higher1), 
    ((H >= Higher1, Higher = Current_Higher); (Higher = High1)))
    ).

adjacents(Row1, Col1, Row2, Col2):- 
    ((Col1 is Col2, (Row1 is Row2-1 ; Row1 is Row2+1) );
    (Col1 is Col2+1, (Row1 is Row2 ; Row1 is Row2-1) );
    (Col1 is Col2-1, ( Row1 is Row2 ; Row1 is Row2+1) )).

is_nb(X, Y, Nb, Visited):-
    nth0( 0, Nb, X1),
    nth0( 1, Nb, Y1),
    not(member([X1,Y1], Visited)),
    adjacents(X, Y, X1, Y1).

replace_nth0(List, Index, OldElem, NewElem, NewList) :-
    nth0(Index,List,OldElem,Transfer),
    nth0(Index,NewList,NewElem,Transfer).

reverssed([X], [X]).
reverssed([X|Y], L2):-
    reverssed(Y, L2R),
    append(L2R, [X], L2).

reverse_all(L, R):-
    maplist(reverse(),L,R).

concat_list([], "").
concat_list([Head|Tail], Concatenated):- 
    concat_list(Tail, Concatenated_), concat(Head, Concatenated_, Concatenated).

add(X, Y, Z):- Z is X + Y.
same(X,X).
successor(X, Y):- Y is X + 1.
predecessor(X, Y):- Y is X - 1.

sum(X, Y, R):- R is X + Y.
sum([X], X).
sum([X|Y], R):- sum(Y, R1), sum(X, R1, R).
resta(X, Y, R):- R is X - Y.
resta([X], X).
resta([X|Y], R):- resta(Y, R1), resta(X, R1, R).

parse_input_place(Raw_input, Type, Row, Col):-
    split_string(Raw_input,"\s","\s",Input),
    nth0(0,Input,Type),
    nth0(1,Input,R1),
    nth0(2,Input,C1),
    atom_number(R1,Row),
    atom_number(C1,Col).

parse_input_move(Raw_input, R1, C1, R2, C2):-
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









% prints
write_all([]):-write("\n").
write_all([Head|Tail]):-
    write(Head), write("\n"), write_all(Tail).

printall([]):-
    write("\n").
printall([X|T]):-
    write(X),
    write(" "),
    printall(T).