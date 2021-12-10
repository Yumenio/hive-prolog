:- module(ia, [minimax/4, commit_movement/4]).

:- use_module(utils).


% depth = 2, always
minimax(2, Player, Opponent, BestMove):-
  playerMovements(Player, Opponent, AllMovements),
  maplist(evaluate_movements(Player, Opponent), AllMovements, AllValues),
  maplist(minimax(1, Player, Opponent), AllMovements, AllValues, R2Values),
  delete(R2Values, [], R2FixedValues),
  maplist(find_best_opponent_move, R2FixedValues, BestOpponentMovePerPiece),
  (
    (BestOpponentMovePerPiece = [], BestMove = []);
    (
      maplist(get_pair_value, BestOpponentMovePerPiece, PlayerMoveValues),
      max_member(MaxValue, PlayerMoveValues), nth0(Index, PlayerMoveValues, MaxValue),
      % printall(["All possible plays:"]), write_all(BestOpponentMovePerPiece),
      nth0(Index, BestOpponentMovePerPiece, BestMove)
    )
  ).


minimax(2, _, _, 0).

% depth = 1, always
minimax(1, Player, Opponent, PlayerMovement, MovementValue, OpponentValue):-
  commit_movement(Player, Opponent, PlayerMovement, Player_R),
  playerMovements(Opponent, Player_R, AllOpponentMovements),
  (
    (
      AllOpponentMovements = [],
      OpponentValue = [ [MovementValue, -10, PlayerMovement, [] ]]
    );

    (
      maplist(evaluate_movements(Opponent, Player_R), AllOpponentMovements, AllOpponentValues),
      maplist(minimax(0, PlayerMovement, MovementValue), AllOpponentMovements, AllOpponentValues, OpponentValue)
    )
  ).
  
minimax(1, _, _, _, MovementValue, [MovementValue, 0]).

minimax(0, FirstPlayerMovement, FirstPlayerValue, SecondPlayerMovement, SecondPlayerValue, [FirstPlayerValue, SecondPlayerValue, FirstPlayerMovement, SecondPlayerMovement]).

% List has the form: [ [Player1Value, Player2Value, Player1Move, Player2Move], and repeat...]
find_best_move(List, Best):-
  maplist(get_pair_value, List, ValueList),
  max_member(MaxValue, ValueList), nth0(Index, ValueList, MaxValue),
  nth0(Index, List, Best).

find_best_opponent_move(List, Best):-
  maplist(get_opponent_value, List, ValueList),
  max_member(MaxValue, ValueList), nth0(Index, ValueList, MaxValue),
  nth0(Index, List, Best).

get_pair_value([P1Move, P2Move|_], Value):- Value is P1Move - P2Move.
get_opponent_value([P1Move, P2Move|_], Value):- Value is P2Move - P1Move.

join_paths([], []).
join_paths([Head|Tail], Result):-
  join_paths(Tail, Result1),
  append(Head, Result1, Result).

playerMovements(Player, Opponent, AllMovements):-
  onGameCells(Player, Opponent, OnGameCells),
  maplist(get_hex_moves(OnGameCells), Player, Result),

  get_pillbug_special(Player, Opponent, PillbugSpecialMoves),
  append(PillbugSpecialMoves, Result, ResultFtPillbug),
  join_paths(ResultFtPillbug, AllMovementsTemp), list_to_set(AllMovementsTemp, MoveSet),
  % printall(["Moveset", MoveSet]),
  maplist(label_move, MoveSet, LabeledMoveSet), delete(LabeledMoveSet, ["move"], LabeledCleanMoveSet),
  get_hex_placement(Player, Opponent, PlayerPlacement),
  append([PlayerPlacement], LabeledCleanMoveSet, AllMovements).

playerMovements(_, _, []).

label_move(Move, ["move"|Move]).

get_hex_placement(Player, Opponent, PlayerPlacement):-
  include(offGame_hex, Player, OffGamePlayerCells),
  random_member(RandomHex, OffGamePlayerCells),
  get_valid_placements(Player, Opponent, ValidPlacements),
  random_member(RandomPlacement, ValidPlacements),
  get_type(RandomHex, Type),
  PlayerPlacement = [ "place", Type, RandomPlacement].

get_valid_placements(Player, Opponent, ValidPlacements):-
  onGameCells(Player, Opponent, OnGameCells),
  include(is_on_game, Player, OnGamePlayerCells),
  include(is_on_game, Opponent, OnGameOpponentCells),
  empty_neighbours(OnGamePlayerCells, [], OnGameCells, FreeCellsPlayer),
  empty_neighbours(OnGameOpponentCells, [], OnGameCells, FreeCellsOpponent),
  include(valid_placement_cell(FreeCellsOpponent), FreeCellsPlayer, ValidPlacements).

valid_placement_cell(OpponentCells, PlayerCell):- not(member(PlayerCell, OpponentCells)).



offGame_hex(hex(_, _, _, _, _, 0, _)).

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
  nth0(0, Movement, "move"),
  nth0(1, Movement, HexPos),
  last(Movement, DestPos),
  (
    (
      find_hex(HexPos, Player, Hex),
      evaluate_hex_movement(Hex, DestPos, Player, Opponent, Value)
    );
    (
      find_hex(HexPos, Opponent, Hex),
      evaluate_hex_movement(Hex, DestPos, Player, Opponent, Value)
    )
  ).

evaluate_movements(Player, Opponent, ["place", Type, Dest], Value):-
  evaluate_hex_placement(Type, Dest, Player, Opponent, Value).

evaluate_hex_placement(Type, [X, Y], Player, Opponent, Value):-
  find_free_bug(Type, Player, 0, Index),
  nth0(Index, Player, hex(Type, _, _, Color, _, _, _)),
  new_hex(Type, X, Y, Color, 0, 1, 2, NewHex),
  replace_nth0(Player, Index, _, NewHex, Player_R),
  
  evaluate_after_before_placement(Player, Opponent, Player_R, Opponent, Value).

evaluate_hex_movement(hex(Type, Row, Col, Color, Height, OnGame, Block), [X, Y], Player, Opponent, Value):-
  (
    find_hex(hex(Type, Row, Col, Color, Height, OnGame, Block), Player, 0, Pos),
    new_hex(Type, X, Y, Color, Height, OnGame, Block,HexTemp),
    replace_nth0(Player, Pos, _, HexTemp, Player_R),
    evaluate_after_before(Player, Opponent, Player_R, Opponent, Value)
  );
  (
    find_hex(hex(Type, Row, Col, Color, Height, OnGame, Block), Opponent, 0, Pos),
    new_hex(Type, X, Y, Color, Height, OnGame, Block,HexTemp),
    replace_nth0(Opponent, Pos, _, HexTemp, Opponent_R),
    evaluate_after_before(Player, Opponent, Player, Opponent_R, Value)
  ).

evaluate_after_before_placement(PlayerBefore, OpponentBefore, PlayerAfter, OpponentAfter, Value):-
  nth0(0,PlayerBefore, hex(_, _, _, PlayerColor, _, _, _)),
  onGameCells(PlayerBefore, OpponentBefore, OnGameCellsBefore),
  onGameCells(PlayerAfter, OpponentAfter, OnGameCellsAfter),
  surrounding_queen_count(PlayerColor, OnGameCellsBefore, BeforePlayerCount, BeforeOpponentCount),
  surrounding_queen_count(PlayerColor, OnGameCellsAfter, AfterPlayerCount, AfterOpponentCount),
  stuck_count(PlayerColor, OnGameCellsBefore, BeforePlayerUnstuck, BeforeOpponentUnstuck),
  stuck_count(PlayerColor, OnGameCellsAfter, AfterPlayerUnstuck, AfterOpponentUnstuck),
  % write("\nEvaluating...\n"), write("Before\n"), write_all(OnGameCellsBefore), write("After\n"), write_all(OnGameCellsAfter),
  % printall(["BeforePlayerUnstuck", BeforePlayerUnstuck, "vs", AfterPlayerUnstuck, "AfterPlayerUnstuck"]),
  % printall(["BeforeOpponentUnstuck", BeforeOpponentUnstuck, "vs", AfterOpponentUnstuck, "AfterOpponentUnstuck"]),
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
          Value is 0 + ( BeforePlayerCount - AfterPlayerCount) + ( AfterOpponentCount - BeforeOpponentCount) + (AfterPlayerUnstuck - BeforePlayerUnstuck) + (BeforeOpponentUnstuck - AfterOpponentUnstuck)
      )
  ).

evaluate_after_before(PlayerBefore, OpponentBefore, PlayerAfter, OpponentAfter, Value):-
  nth0(0,PlayerBefore, hex(_, _, _, PlayerColor, _, _, _)),
  onGameCells(PlayerBefore, OpponentBefore, OnGameCellsBefore),
  onGameCells(PlayerAfter, OpponentAfter, OnGameCellsAfter),
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
  get_row(PlayerQueen, Row), get_col(PlayerQueen, Col),
  % neighbours(PlayerQueen, OnGameCells, QueenSurrounders), !,
  maplist(get_coordinates, OnGameCells, OnGameCellsCoor),
  list_to_set(OnGameCellsCoor, OnGameCellsCoorSet),
  findall([X1, Y1], onGame_adjacents(X1, Y1, OnGameCellsCoorSet, Row, Col), QueenSurrounders),
  length(QueenSurrounders, PlayerCount),
  
  opponent_color(PlayerColor, OpponentColor),
  (
      (   % enemy queen in game
          find_queen(OpponentColor, OnGameCells, OpponentQueen),
          get_row(OpponentQueen, OppRow), get_col(OpponentQueen, OppCol),
          % neighbours(OpponentQueen, OnGameCells, OpponentSurrounders), !,
          findall([X1, Y1], onGame_adjacents(X1, Y1, OnGameCellsCoorSet, OppRow, OppCol), OpponentSurrounders),
          length(OpponentSurrounders, OpponentCount)
      )
      ;
      (   % enemy queen not in game
          OpponentCount = 0    
      )
  
  ).


commit_movement(Player, Opponent, ["move"|Movement], Player_R):-
  last(Movement, [X, Y]), onGameCells(Player, Opponent, OnGameCells), not(occupied(X, Y, OnGameCells)),
  nth0(0, Movement, HexPos),
  (
    (
      find_hex(HexPos, Player, Hex),
      get_all(Hex, Type, _, _, Color, _, _, _),
      new_hex(Type, X, Y, Color, 1, 1, 2, HexTemp),
      find_hex(Hex, Player, 0, Pos),
      replace_nth0(Player, Pos, _, HexTemp, Player_R)
    );
    
    (
      find_hex(HexPos, Opponent, Hex),
      get_all(Hex, Type, _, _, Color, _, _, _),
      new_hex(Type, X, Y, Color, 1, 1, 2, HexTemp),
      find_hex(Hex, Opponent, 0, Pos),
      replace_nth0(Opponent, Pos, _, HexTemp, Player_R)
    )
  ).

commit_movement(Player, Opponent, ["move"|Movement], Player_R):-
  last(Movement, [X, Y]), onGameCells(Player, Opponent, OnGameCells), occupied(X, Y, OnGameCells),
  nth0(0, Movement, HexPos), find_hex(HexPos, Player, Hex), find_hex([X, Y], OnGameCells, HexTemp),
  get_all(Hex, Type, _, _, Color, _, _, _), get_height(HexTemp, DestHeight), succ(DestHeight, DestHeightSucc),
  new_hex(Type, X, Y, Color, DestHeightSucc, 1, 2, NewHex),
  find_hex(Hex, Player, 0, Pos),
  replace_nth0(Player, Pos, _, NewHex, Player_R).

commit_movement(Player, _, ["place", Type, [X, Y]], Player_R):-
  find_free_bug(Type, Player, 0, Index),
  nth0(Index, Player, hex(Type, _, _, Color, _, _, _)),
  new_hex(Type, X, Y, Color, 0, 1, 2, NewHex),
  replace_nth0(Player, Index, _, NewHex, Player_R).

commit_movement(Player, Opponent, Movement, _):-
  write("\nCOULD NOT COMMIT THE MOVEMENT\n"),
  onGameCells(Player, Opponent, OnGameCells),
  printall([OnGameCells, "\n", "Trying to do:", Movement]).

opponent_color(1, 2).
opponent_color(2, 1).

test([_, _], Player, Oponent):-
  playerMovements(Player, Oponent, Allmovements), write_all(Allmovements).


stuck_count(PlayerColor, OnGameCells, PlayerCount, OpponentCount):-
  include(freedom_to_move_reverse(OnGameCells), OnGameCells, UnstuckedOnGameCells),
  include(belong_to(PlayerColor), UnstuckedOnGameCells, PlayerUnstuckedCells),
  opponent_color(PlayerColor, OpponentColor),
  include(belong_to(OpponentColor), UnstuckedOnGameCells, OpponentUnstuckedCells),
  length(PlayerUnstuckedCells, PlayerCount), length(OpponentUnstuckedCells, OpponentCount).


freedom_to_move_reverse(OnGameCells, Hex):-
    not(buried(Hex, OnGameCells)),
    get_all(Hex, Type, X, Y, Color, Height, _, Block),
    queen_on_game(OnGameCells, Color),
    find_hex(Hex, OnGameCells, 0, Pos),
    new_hex(Type, X, Y, Color, Height, 0, Block, HexTemp),
    replace_nth0(OnGameCells, Pos, _, HexTemp, OnGameCellsTemp),
    include(is_on_game(), OnGameCellsTemp, OnGameRemaining),
    maplist(get_coordinates, OnGameRemaining, OnGameRemainingCoor),
    list_to_set(OnGameRemainingCoor, OnGameRemainingCoorSet),
    length(OnGameRemainingCoorSet, L),
    adjacents(AdjX, AdjY, X, Y), occupied(AdjX, AdjY, OnGameCells),
    cc_bfs([AdjX, AdjY], OnGameRemainingCoorSet, CC),
    length(CC, CCNodes), CCNodes is L.

belong_to(Color, hex(_,_,_,Color,_,_,_)).


get_pillbug_special(Player, Opponent, PillbugSpecials):-
  find_pillbug(Player, PillbugHex), get_onGame(PillbugHex, 1),
  onGameCells(Player, Opponent, OnGameCells), maplist(get_coordinates, OnGameCells, OnGameCellsCoor),
  get_row(PillbugHex, Row), get_col(PillbugHex, Col),
  findall([X1, Y1], onGame_adjacents(X1, Y1, OnGameCellsCoor, Row, Col), PillbugOGAdjacents),
  include(free_to_move(OnGameCells), PillbugOGAdjacents, PillbugOGMovableAdjacents),
  findall([X2, Y2], adjacents(X2, Y2, Row, Col), PillbugAdjacents),
  intersection(PillbugOGAdjacents, PillbugAdjacents, AdjacentsToDelete),
  include(not_member(AdjacentsToDelete), PillbugAdjacents, PillbugFreeAdjacents),
  
  get_pairs(PillbugOGMovableAdjacents, PillbugFreeAdjacents, AllMovePairs),
  include(can_carry(PillbugHex, OnGameCells), AllMovePairs, AllAbleMovePairs),
  PillbugSpecials = [AllAbleMovePairs].

get_pillbug_special(_, _, []).

get_pairs(List1, List2, AllPairs):- get_pairs(List1, List2, List2, AllPairs).
get_pairs([H|T], L2, [P|Q], [ [H,P] |R]):- get_pairs([H|T], L2, Q, R).
get_pairs([H|T], L2, [P], [[H,P]|R]):- get_pairs(T, L2, L2, R).
get_pairs([H], _, [P], [[H,P]]).

find_pillbug([Hex|_], Hex):- get_type(Hex, "pillbug").
find_pillbug([_|Tail], Hex):- find_pillbug(Tail, Hex).

not_member(List, Member):- not(member(Member, List)).

free_to_move(OnGameCells, [X, Y]):-
  find_hex([X, Y], OnGameCells, Hex),
  freedom_to_move(Hex, OnGameCells).

can_carry(PillbugHex, OnGameCells, [[FromX, FromY], [ToX, ToY]]):-
  get_all(PillbugHex, _, Row, Col, _, Height, _, Blocked),
  find_hex([FromX, FromY], OnGameCells, CarriedHex), get_height(CarriedHex, 0), get_blocked(CarriedHex, 0),
  maplist(get_coordinates, OnGameCells, OnGameCellsCoor),
  Height is 0, % the hex being moved cannot be part of a stack of pieces
  Blocked is 0, % the pillbug cannot move the last cell the opponent moved
  findall([X1, Y1], onGame_adjacents(X1, Y1, OnGameCellsCoor, Row, Col), Adj1),

  findall([X2, Y2], onGame_adjacents(X2, Y2, OnGameCellsCoor, FromX, FromY), Adj2),
  findall([X3, Y3], onGame_adjacents(X3, Y3, OnGameCellsCoor, ToX, ToY), Adj3),
  intersection(Adj1, Adj2, CommonAdjs12),
  intersection(Adj1, Adj3, CommonAdjs13),
  not(two_common_of_height_two(CommonAdjs12, OnGameCells, [])),
  not(two_common_of_height_two(CommonAdjs13, OnGameCells, [])).
