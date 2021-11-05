init_board(Board):- Board = [].

init_player(Color,List):-
  append(List, [
    new_hex("queen",     _,_,Color,0,0,queen),
    new_hex("ant",       _,_,Color,0,0,ant1),
    new_hex("ant",       _,_,Color,0,0,ant2),
    new_hex("ant",       _,_,Color,0,0,ant3),
    new_hex("grasshoper",_,_,Color,0,0,grasshoper1),
    new_hex("grasshoper",_,_,Color,0,0,grasshoper2),
    new_hex("grasshoper",_,_,Color,0,0,grasshoper3),
    new_hex("beetle",    _,_,Color,0,0,beetle1),
    new_hex("beetle",    _,_,Color,0,0,beetle2),
    new_hex("spider",    _,_,Color,0,0,spider1),
    new_hex("spider",    _,_,Color,0,0,spider2),
    new_hex("mosquito",  _,_,Color,0,0,mosquito),
    new_hex("pilebough", _,_,Color,0,0,pilebough),
    new_hex("ladybug",   _,_,Color,0,0,ladybug)
    ], List).


% hex(Row,Col,Color,Height)
% row: fila
% col: columna
% color: blancas o negras
% height: altura, x ej los escarabajos pueden escalar sobre otros insectos

new_hex(Type,Row,Col,Color,Height,OnGame, hex(Type,Row,Col,Color,Height)).

move_hex(hex(Type,_,_,Color,Height), New_row, New_col, hex(Type,New_row,New_col,Color,Height)).

% get_row(hex(Type,Row,Col,Color,Height),Row).
get_type(Hex,Type):-       arg(1,Hex,Type).
get_row(Hex,Row):-         arg(2,Hex,Row).
get_col(Hex,Col):-         arg(3,Hex,Col).
get_color(Hex,Color):-     arg(4,Hex,Color).
get_height(Hex,Height):-   arg(5,Hex,Height).

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

test2():-
  new_hex("ant",1,1,0,0,0,Cell1),
  new_hex("ant",3,1,0,0,0,Cell2),
  adjacent(Cell1,Cell2).

:-write("Welcome to pro-hive").

