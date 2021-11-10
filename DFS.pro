get_type(Hex,Type):-       arg(1,Hex,Type).
get_row(Hex,Row):-         arg(2,Hex,Row).
get_col(Hex,Col):-         arg(3,Hex,Col).
get_color(Hex,Color):-     arg(4,Hex,Color).
get_height(Hex,Height):-   arg(5,Hex,Height).
get_onGame(Hex,OnGame):-   arg(6,Hex,OnGame).


adjacents(Hex1,Hex2):-
    get_row(Hex1,Row1),
    get_row(Hex2,Row2),
    get_col(Hex1,Col1),
    get_col(Hex2,Col2),
    ((Col1 is Col2, (Row1 is Row2-1 ; Row1 is Row2+1) );
    (Col1 is Col2+1, (Row1 is Row2 ; Row1 is Row2-1) );
    (Col1 is Col2-1, (Row1 is Row2 ; Row1 is Row2+1) ) ).




new_hex(Type,Row,Col,Color,Height,OnGame, hex(Type,Row,Col,Color,Height, OnGame)).
%new_hex("ant",       _,_,1,0,0,ant1).
%new_hex("ant",       _,_,1,0,0,ant2).
%new_hex("ant",       _,_,1,0,0,ant3).
%new_hex("grasshoper",_,_,1,0,0,grasshoper1).
%new_hex("grasshoper",_,_,1,0,0,grasshoper2).
%new_hex("grasshoper",_,_,1,0,0,grasshoper3).
%new_hex("beetle",    _,_,1,0,0,beetle1).
%new_hex("beetle",    _,_,1,0,0,beetle2).
%new_hex("spider",    _,_,1,0,0,spider1).
%new_hex("spider",    _,_,1,0,0,spider2).
%new_hex("mosquito",  _,_,1,0,0,mosquito).
%new_hex("pilebough", _,_,1,0,0,pilebough).
%new_hex("ladybug",   _,_,1,0,0,ladybug).

% queen(Queen) :- new_hex("queen",     0,0,1,0,0, Queen).
% ant(Ant) :- new_hex("ant",     0,0,1,0,0, Ant).
% ?- ant(Ant), get_color(Ant, Type), write(Type).

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

player1(List):- init_player1(1, List).
player2(List):- init_player2(2, List).

list_print([]):- write("\n").
list_print([H|T]):- write(H), write(" "), list_print(T).

onGameSingle([], []).
onGameSingle([H|T], List):- get_onGame(H, OnGame), ((OnGame is 1, onGameSingle(T, L), append([H], L, List)) ; onGameSingle(T, L), append([], L, List)).

onGameCells(List):-player1(L1), player2(L2), onGameSingle(L1, L3), 
                            onGameSingle(L2, L4), append(L3, L4, List).

llamar_dfs(V):- onGameCells(L), nth0(0, L, R), dfs(R, L, V).


move(_, [], _).
move(H, [L|R], Nbs):- (adjacents(H, L), move(H, R, Nbs_), append([L], Nbs_, Nbs)); move(H, R, Nbs).
%% dfs starting from a root 
dfs(Root, L, T):-
    dfs([Root], L, [], T).
%% Done, all visited
dfs([], _, L1, L1):- length(L1, T), write(T).
%% Skip elements that are already visited
dfs([H|T], L, Visited, T1):-
    member(H, Visited),
    dfs(T, L, Visited, T1).
%% add all adjacents
dfs([H|T], L, Visited, T1):-
    not(member(H, Visited)),
    move(H, L, Nbs),
    append(Nbs, T, ToVisit),
    dfs(ToVisit, L, [H|Visited], T1).
