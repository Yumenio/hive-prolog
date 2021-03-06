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
:- use_module(ia). 

welcome_message:-
  write_all(["Welcome to BokuNoHive!", "‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\n", "To start a new game, type init_game().\n", "You can also type init_game(\"mode\")., where mode is one of the following:"]),
  write_all(["two_players", "vs_ia", "ia_vs_ia"]).

:-welcome_message.

init_game():-
  init_game("two_players").

init_game("two_players"):-
  players(Player1,Player2),
  first_two_places(Player1,Player2,Player1_R,Player2_R),
  game(Player1_R,Player2_R, 2).

init_game("vs_ia"):-
  players(Player1,Player2),
  write("First turn:\n"),
  first_placed(Player1,Player1_R,Hex),
  second_placed_ia(Hex,Player2,Player2_R),
  show_board(Player1_R, Player2_R),
  game_vs_ia(Player1_R,Player2_R, 2).

init_game("ia_vs_ia"):-
  players(IA1, IA2),
  write("First IA turn:\n"),
  first_placed_ia(IA1, NewIA1, FirstHex),
  write("Second IA turn:\n"),
  second_placed_ia(FirstHex, IA2, NewIA2),
  show_board(NewIA1, NewIA2),
  game_ia_vs_ia(NewIA1, NewIA2, 2).


game(Player1,Player2, Turn):-
  write("Turn Player-1:\n"),
  turn_player1(Turn, Player1, Player2, NewPlayer11, NewPlayer21),
  unblock(NewPlayer11, UNewPlayer11), unblock(NewPlayer21, UNewPlayer21),
  show_board(UNewPlayer11, UNewPlayer21),
  game_states(UNewPlayer11, UNewPlayer21), 

  write("Turn Player-2:\n"),
  turn_player2(Turn, UNewPlayer11, UNewPlayer21, NewPlayer22, NewPlayer12),
  unblock(NewPlayer12, UNewPlayer12), unblock(NewPlayer22, UNewPlayer22),
  show_board(UNewPlayer12, UNewPlayer22),
  game_states(UNewPlayer12, UNewPlayer22), 
  
  successor(Turn, Turn1),
  game(UNewPlayer12, UNewPlayer22, Turn1).

game_vs_ia(Player1, Player2, Turn):-
  write("Turn Player-1:\n"),
  turn_player1(Turn, Player1, Player2, NewPlayer11, NewPlayer21),
  unblock(NewPlayer11, UNewPlayer11), unblock(NewPlayer21, UNewPlayer21),
  show_board(UNewPlayer11, UNewPlayer21),
  game_states(UNewPlayer11, UNewPlayer21), 
  

  turn_ia(Turn, UNewPlayer21, UNewPlayer11, NewPlayer22, NewPlayer12),
  unblock(NewPlayer12, UNewPlayer12), unblock(NewPlayer22, UNewPlayer22),
  show_board(UNewPlayer12, UNewPlayer22),
  game_states(UNewPlayer12, UNewPlayer22), 
  
  successor(Turn, Turn1),
  game_vs_ia(UNewPlayer12, UNewPlayer22, Turn1).

game_ia_vs_ia(IA1, IA2, Turn):-
  write("Turn IA-1:\n"),
  % turn_player1(Turn, Player1, Player2, NewPlayer11, NewPlayer21),
  % unblock(NewPlayer11, UNewPlayer11), unblock(NewPlayer21, UNewPlayer21),
  turn_ia(Turn, IA1, IA2, NewIA11, NewIA21),
  unblock(NewIA11, UNewIA11), unblock(NewIA21, UNewIA21),
  show_board(UNewIA11, UNewIA21),
  game_states(UNewIA11, UNewIA21),
  

  turn_ia(Turn, UNewIA21, UNewIA11, NewIA22, NewIA12),
  unblock(NewIA12, UNewIA12), unblock(NewIA22, UNewIA22),
  show_board(UNewIA12, UNewIA22),
  game_states(UNewIA12, UNewIA22), 
  
  successor(Turn, Turn1),
  game_ia_vs_ia(UNewIA12, UNewIA22, Turn1).


turn_player1(Turn, Player1, Player2, NewPlayer1, NewPlayer2):-
  read_line_to_string(user_input, Raw_input),
  split_string(Raw_input,"\s","\s",Input),
  ( % caso ia
  (
  Raw_input = "ia",
  minimax(2, Player1, Player2, BestMove),
    (
      ( % caso sin movimientos válidos, pasar turno
        BestMove = [], NewPlayer1 = Player1, NewPlayer2 = Player2
      );
        
      ( % caso con al menos un movimiento válido
        BestMove = [MyValue, OppValue, MyMove, _],
        printall(["Best \"possible\" move, with a value of", [MyValue, OppValue], "is", MyMove]),
        commit_movement(Player1, Player2, MyMove, NewPlayer1),
        NewPlayer2 = Player2  
      )  
    )
  );
  % pasar turno
  (Raw_input = "pass", write("Passed turn\n"), NewPlayer1 = Player1, NewPlayer2 = Player2);

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
  split_string(Raw_input,"\s","\s",Input),
  
  ( % caso ia 
  (
    Raw_input = "ia",
    minimax(2, Player2, Player1, BestMove), 
    (
      ( % caso sin movimientos posibles
        BestMove = [], NewPlayer2 = Player2, NewPlayer1 = Player1  
      );
      
      ( % caso con al menos 1 movimiento válido
        BestMove = [MyValue, OppValue, MyMove, _],
        printall(["Best \"possible\" move, with a value of", [MyValue, OppValue], "is", MyMove]),
        commit_movement(Player2, Player1, MyMove, NewPlayer2),
        NewPlayer1 = Player1
        )  
      )
  );

  % para pasar turno
  (Raw_input = "pass", write("Passed turn\n"), NewPlayer1 = Player1, NewPlayer2 = Player2);

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

first_placed_ia(PlayerIA, NewPlayerIA, Hex):-
  % random_member(Type, ["ant", "spider", "queen", "beetle", "grasshoper", "ladybug", "pillbug", "mosquito"]),
  new_hex("queen", 1, 1, 1, 0, 1, 0, Hex),
  find_free_bug("queen", PlayerIA, 0, Pos),
  replace_nth0(PlayerIA, Pos, _, Hex, NewPlayerIA).

second_placed_ia(PlayerHex, PlayerIA, NewPlayerIA):-
  get_row(PlayerHex, Row), get_col(PlayerHex, Col),
  adjacents(X, Y, Row, Col),
  find_free_bug("queen", PlayerIA, 0, Pos),
  % random_member(Type, ["ant", "spider", "queen", "beetle", "grasshoper", "ladybug", "pillbug", "mosquito"]),
  new_hex("queen", X, Y, 2, 0, 1, 0, NewHex),
  replace_nth0(PlayerIA, Pos, _, NewHex, NewPlayerIA).
  
  

turn_ia(_, PlayerIA, Opponent, NewPlayerIA, NewOpponent):-
  write("\nPlease, press Enter to finish current turn\n"), read_line_to_string(user_input,_),
  minimax(2, PlayerIA, Opponent, BestMove),
  (
    (
      BestMove = [], NewPlayerIA = PlayerIA, NewOpponent = Opponent  
    );
    (
      BestMove = [MyValue, OppValue, MyMove, _],
      printall(["Best \"possible\" move, with a value of", [MyValue, OppValue], "is", MyMove]),
      commit_movement(PlayerIA, Opponent, MyMove, NewPlayerIA),
      NewOpponent = Opponent
    )  
  ).


first_two_places(Player1,Player2,Player1_R,Player2_R):-
  write("First turn:\n"),
  first_placed(Player1,Player1_R,Hex),
  write("Second turn:\n"),
  second_placed(Hex,Player2,Player2_R).


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


% mover para board la conversion de las celdas
show_board(Player_1, Player_2):-
  include(is_on_game(), Player_1, Player1),
  include(is_on_game(), Player_2, Player2),
  append(Player1, Player2, OnGameCells),
  %get_converted_cells(OnGameCells, Converted),

  board(OnGameCells, Board),
  write(Board).
