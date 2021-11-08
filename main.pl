import(utils, list_utils).

init_board(Board):- Board = [].

init_player(Color,List):-
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
  new_hex("pillbug", _,_,Color,0,0,Pillbug),
  new_hex("ladybug",   _,_,Color,0,0,Ladybug),
  append([],[Queen, Ant1, Ant2, Ant3, Grasshoper1,Grasshoper2,Grasshoper3,Beetle1,Beetle2,Spider1,Spider2,Mosquito,Pillbug,Ladybug],List).

% hex(Row,Col,Color,Height,OnGame)
% row: fila
% col: columna
% color: blancas o negras
% height: altura, x ej los escarabajos pueden escalar sobre otros insectos
% OnGame: 0 si la ficha no está en tablero, 1 otherwise

new_hex(Type,Row,Col,Color,Height,OnGame, hex(Type,Row,Col,Color,Height,OnGame)).

move_hex(hex(Type,_,_,Color,Height), New_row, New_col, hex(Type,New_row,New_col,Color,Height)).


adjacent(Hex1,Hex2):-
  get_row(Hex1,Row1),
  get_row(Hex2,Row2),
  get_col(Hex1,Col1),
  get_col(Hex2,Col2),
  write(Row1),
  write(Col1),
  write(Row2),
  write(Col2),
  ((Col1 is Col2, (Row1 is Row2-1 ; Row1 is Row2+1) );
  (Col1 is Col2+1, (Row1 is Row2 ; Row1 is Row2-1) );
  (Col1 is Col2-1, (Row1 is Row2 ; Row1 is Row2+1) ) ).

test():-
  init_player(1,player1),
  init_player(2,player2),
  turn is 1,
  new_hex("ant",0,0,0,0,1, Cell),
  get_row(Cell,Num),
  write(Num),
  put("\n"),
  move_hex(Cell, 1, 1, Moved_Cell),
  get_row(Moved_Cell, New_Row),
  write(New_Row).

tt():-
  new_hex("ant",1,1,0,0,0,Cell1),
  new_hex("ant",3,1,0,0,0,Cell2),
  % adjacent(Cell1,Cell2),
  % pprint(Cell1).
  init_player(1,player1),
  print_hive(player1).

hh():-
  nth0(1,[1,2,3],J),
  write(J).

:-write("Welcome to pro-hive").