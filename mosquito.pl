
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