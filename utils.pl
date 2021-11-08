:- module(utils, [get_type/2, get_row/2, get_col/2, get_color/2, get_height/2, get_onGame/2, pprint/1,print_hive/1]).

% get_row(hex(Type,Row,Col,Color,Height,OnGame),Row).
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

pprint(Bug):-
  get_type(Bug, Type),  
  get_row(Bug,Row),
  get_col(Bug,Col),
  get_color(Bug,Color),
  get_height(Bug,Height),
  get_onGame(Bug,OnGame),
  printall(["type:",Type,"    row:",Row,"    col:",Col,"    color:",Color,"    height:",Height,"    onGame:",OnGame]).

print_hive([]).
print_hive([Hive|T]):-
  pprint(Hive),
  print_hive(T).