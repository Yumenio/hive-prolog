:- module(board, [board/2]).
:- use_module(utils).

spaces(0, "").
spaces(N, S):-S1 = " ", predecessor(N, N1), spaces(N1, S2), concat(S1, S2, S).

%abs es para el modulo
construct_board(N, N1, Board):-
    length(Board, N),
    length(List1, N1),
    maplist(=(0), List1),
    maplist(=(List1), Board).

% para encontrar el mayor y el menor numero en una lista
higher_at([], _, Ch, Ch).
higher_at([Head|Tail], I, H, Higher):-
    nth0(I, Head, H1), 
    (
    (H1 >= H, higher_at(Tail, I, H1, Higher1), 
    ((H1 >= Higher1, Higher = H1); (Higher = Higher1)));

    (H >= H1, higher_at(Tail, I, H, Higher1), 
    ((H >= Higher1, Higher = H); (Higher = Higher1)))
    ).

lower_at([], _, Ch, Ch).
lower_at([Head|Tail], I, H, Higher):-
    nth0(I, Head, H1), 
    (
    (H1 =< H, lower_at(Tail, I, H1, Higher1), 
    ((H1 =< Higher1, Higher = H1); (Higher = Higher1)));

    (H =< H1, lower_at(Tail, I, H, Higher1), 
    ((H =< Higher1, Higher = H); (Higher = Higher1)))
    ).
% da el board en forma de array bidimensional 
board(OnGameCells, NewBoard):-
    get_converted_cells(OnGameCells, Cells1), 

    empty_neighbours(OnGameCells, [], OnGameCells, Free_Cells),
    maplist(add_empty_type, Free_Cells, EmptyCells), append(Cells1, EmptyCells, Cells),

    nth0(0, Cells, Tupla1), nth0(0, Tupla1, W1), nth0(1, Tupla1, H1), 
    higher_at(Cells, 0, W1, Higher1), higher_at(Cells, 1, H1, Higher2),
    lower_at(Cells, 0, W1, Lower1), lower_at(Cells, 1, H1, Lower2),
    resta([Higher1, Lower1], R1), successor(R1, Width), 
    resta([Higher2, Lower2], R2), successor(R2, Heigth),
    %write(Heigth), write(" "), write(Width), write("\n"),
    construct_board(Width, Heigth, Board), !, 
    update_rows(Cells, Higher1, Higher2, Width, Heigth, Board, Printable_Board), 
    print_board(Printable_Board, Lower1, Lower2, NewBoard).

%existe concat(X, Y, XY).
update_rows([], _, _, _, _, NewBoard, NewBoard).
update_rows([[X, Y, T]|Tail], Higher1, Higher2, Len1, Len2, Board, NewBoard):-
    % indice de la fila en el board
    resta([Higher1, X], R1), resta([Len1, R1], R),
    % indice de la columna en el board
    resta([Higher2, Y], R2), resta([Len2, R2], C),
    %la fila que se va a escoger
    predecessor(R, R0), predecessor(C, C0),
    nth0(R0, Board, Row), update_single_row(Row, C0, T, NewRow),
    replace_nth0(Board, R0, _, NewRow, Board1),
    update_rows(Tail, Higher1, Higher2, Len1, Len2, Board1, NewBoard).

update_single_row(Row, Index, X, NewRow):-
    replace_nth0(Row, Index, _, X, NewRow).


print_board(L, LX, LY, TopCenterBottom):-
    length(L, Len), Len is 1, nth0(0, L, Head),
    top(Head, Top), center(Head, LX, LY, Center), bottom(Head, Bottom),
    concat(Top, Center, TopCenter), concat(TopCenter, Bottom, TopCenterBottom).

print_board(L, LX, LY, TopCenterBottom):-
    length(L, Len), Len > 1, nth0(0, L, Head),
    top(Head, Top), pretty_print_board(L, LX, LY, 0, CenterBottom),
    concat(Top, CenterBottom, TopCenterBottom).

pretty_print_board(L, LX, LY, Indent, CenterBottom):-
    length(L, Len), Len is 1, nth0(0, L, Head),
    center(Head, LX, LY, C), bottom(Head, B), S is Indent*3, spaces(S, Spaces),
    concat(Spaces, C, Center), concat(Spaces, B, Bottom), concat(Center, Bottom, CenterBottom).
pretty_print_board([Head1, Head2|Tail], LX, LY, Indent, CenterBottom):-
    center(Head1, LX, LY, C), bottom(Head1, Head2, 0, B), S is Indent*3, spaces(S, Spaces), 
    concat(Spaces, C, Center), concat(Spaces, B, Bottom), concat(Center, Bottom, CB),
    successor(Indent, Next_Indent), successor(LX, LX1),
    pretty_print_board([Head2|Tail], LX1, LY, Next_Indent, CB1), concat(CB, CB1, CenterBottom).

%Funciones para conformar el string del hive
center([], _, _, "\n").
center([Head|Tail], LX, LY, Row):- 
    not(Head is 0), LX >= 0, LY >= 0,
    successor(LY, LY1), center(Tail, LX, LY1, Result),
    mdl(LX, 10,  LX_), mdl(LY, 10,  LY_),
    concat_list(["|", LX_, "  ", LY_, "|", Result], Row).
%
center([Head|Tail], LX, LY, Row):- 
    not(Head is 0), LX < 0, LY < 0,
    successor(LY, LY1), center(Tail, LX, LY1, Result),
    mdl(LX, 10,  LX_), mdl(LY, 10,  LY_),
    concat_list(["|", LX_, LY_, "|", Result], Row).
%
center([Head|Tail], LX, LY, Row):- 
    not(Head is 0), 
    successor(LY, LY1), center(Tail, LX, LY1, Result),
    mdl(LX, 10, LX_), mdl(LY, 10,  LY_),
    concat_list(["|", LX_, " ", LY_, "|", Result], Row).
%
center([Head|Tail], LX, LY, Row):- 
    Head is 0, successor(LY, LY1),
    center(Tail, LX, LY1, Result),
    concat_list(["      ", Result], Row).

%‾‾
top_first([], "\n").
top_first([Head|Tail], Row):- 
    not(Head is 0), top_first(Tail, Result), 
    concat_list(["  __  ", Result], Row).
%
top_first([Head|Tail], Row):- 
    Head is 0, top_first(Tail, Result), 
    concat_list(["      ", Result], Row).
%
top_second([], "\n").
top_second([Head|Tail], Row):- 
    not(Head is 0), top_second(Tail, Result), 
    concat_list([" /", Head, "\\ ", Result], Row).
%
top_second([Head|Tail], Row):- 
    Head is 0, top_second(Tail, Result), 
    concat_list(["      ", Result], Row).
%
top(L, Top):- 
    top_first(L, Top_first), 
    top_second(L, Top_second),
    concat(Top_first, Top_second, Top).
%
bottom([], "\n").
bottom([Head|Tail], Row):- 
    not(Head is 0), bottom(Tail, Result), 
    concat_list([" \\__/ ", Result], Row).
%
bottom([Head|Tail], Row):- 
    Head is 0, bottom(Tail, Result), 
    concat_list(["      ", Result], Row).
% cuando la casilla esta en juego 
bottom([], _, _, "\n").
% ultimo con ambos adyacentes abajo ocupados
bottom([Head|Tail], L, Index, Row):-
    not(Head is 0), length(L, Len), Len1 is Len-1, Index is Len1, 
    successor(Index, Next_Index), predecessor(Index, Pre_Index),
    nth0(Index, L, Head1),  nth0(Pre_Index, L, Pre),
    not(Head1 is 0), not(Pre is 0), sub_atom(Pre, 1, 1, _, Char),
    bottom(Tail, L, Next_Index, Result), 
    concat_list([Char, "\\__/", Head1, "\\", Result], Row).
% ultimo con solo el adyacente abajo
bottom([Head|Tail], L, Index, Row):-
    not(Head is 0), length(L, Len), Len1 is Len-1, Index is Len1, 
    nth0(Index, L, Head1), not(Head1 is 0), 
    successor(Index, Next_Index), bottom(Tail, L, Next_Index, Result), 
    concat_list([" \\__/", Head1, "\\", Result], Row).
% el ultimo sin adyacentes entra en el caso mas general
% el primero con adyacente abajo
bottom([Head|Tail], L, Index, Row):-
    not(Head is 0), Index is 0, nth0(Index, L, Head1), 
    not(Head1 is 0), sub_atom(Head1, 0, 1, _, Char),
    successor(Index, Next_Index), bottom(Tail, L, Next_Index, Result), 
    concat_list([" \\__/", Char, Result], Row).
% el primero sin adyacente abajo
bottom([Head|Tail], L, Index, Row):-
    not(Head is 0), Index is 0, successor(Index, Next_Index), 
    bottom(Tail, L, Next_Index, Result), 
    concat_list([" \\__/ ", Result], Row).
% caso intermedio con ambos adyacentes abajo 
bottom([Head|Tail], L, Index, Row):-
    not(Head is 0), nth0(Index, L, Head1), predecessor(Index, Pre_Index), 
    nth0(Pre_Index, L, Pre), not(Head1 is 0), not(Pre is 0), 
    sub_atom(Pre, 1, 1, _, Char1), sub_atom(Head1, 0, 1, _, Char2),
    successor(Index, Next_Index), bottom(Tail, L, Next_Index, Result), 
    concat_list([Char1, "\\__/", Char2, Result], Row).
% caso intermedio con solo adyacente abajo directo
bottom([Head|Tail], L, Index, Row):-
    not(Head is 0), nth0(Index, L, Head1), not(Head1 is 0),  
    sub_atom(Head1, 0, 1, _, Char), successor(Index, Next_Index), 
    bottom(Tail, L, Next_Index, Result), 
    concat_list([" \\__/", Char, Result], Row).
% caso intermedio con solo adyacente abajo detras
bottom([Head|Tail], L, Index, Row):-
    not(Head is 0), predecessor(Index, Pre_Index), nth0(Pre_Index, L, Pre), 
    not(Pre is 0),  sub_atom(Pre, 1, 1, _, Char), 
    successor(Index, Next_Index), bottom(Tail, L, Next_Index, Result), 
    concat_list([Char, "\\__/ ", Result], Row).
% caso mas general
bottom([Head|Tail], L, Index, Row):-
    not(Head is 0), successor(Index, Next_Index), 
    bottom(Tail, L, Next_Index, Result), 
    concat_list([" \\__/ ", Result], Row).
% cuando la casilla no esta en juego

% ultimo con ambos adyacentes abajo ocupados
bottom([Head|Tail], L, Index, Row):-
    Head is 0, successor(Index, Next_Index), predecessor(Index, Pre_Index),
    length(L, Len), Len1 is Len-1, Index is Len1,
    nth0(Pre_Index, L, Pre), nth0(Index, L, Down), 
    not(Pre is 0), not(Down is 0), sub_atom(Pre, 1, 1, _, Char), 
    bottom(Tail, L, Next_Index, Result), 
    concat_list([Char, "\\__/", Down,"\\ ", Result], Row).
% ultimo con solo el adyacente abajo
bottom([Head|Tail], L, Index, Row):-
    Head is 0, successor(Index, Next_Index), length(L, Len),
    Len1 is Len-1, Index is Len1, nth0(Index, L, Down),
    not(Down is 0), bottom(Tail, L, Next_Index, Result), 
    concat_list(["    /", Down, "\\ ", Result], Row).
% ultimo con adyacente abajo atras
bottom([Head|Tail], L, Index, Row):-
    Head is 0, successor(Index, Next_Index), predecessor(Index, Pre_Index), 
    length(L, Len), Len1 is Len-1, Index is Len1, 
    nth0(Pre_Index, L, Pre), not(Pre is 0), 
    sub_atom(Pre, 1, 1, _, Char), bottom(Tail, L, Next_Index, Result), 
    concat_list([Char, "\\ ", Result], Row).
% primero con adyacente abajo
bottom([Head|Tail], L, Index, Row):-
    Head is 0, successor(Index, Next_Index), Index is 0,
    nth0(Index, L, Down), sub_atom(Down, 0, 1, _, Char), 
    not(Down is 0), bottom(Tail, L, Next_Index, Result), 
    concat_list(["    /", Char, Result], Row).
% caso general con ambos adyacente abajo
bottom([Head|Tail], L, Index, Row):-
    Head is 0, successor(Index, Next_Index), predecessor(Index, Pre_Index),
    nth0(Pre_Index, L, Pre), nth0(Index, L, Down),
    not(Pre is 0), not(Down is 0), sub_atom(Pre, 1, 1, _, Char1), 
    sub_atom(Down, 0, 1, _, Char2), bottom(Tail, L, Next_Index, Result), 
    concat_list([Char1, "\\__/", Char2, Result], Row).
% caso general con adyacente abajo directo
bottom([Head|Tail], L, Index, Row):-
        Head is 0, successor(Index, Next_Index),
        nth0(Index, L, Down), not(Down is 0),
        sub_atom(Down, 0, 1, _, Char),
        bottom(Tail, L, Next_Index, Result), 
        concat_list(["    /", Char, Result], Row).
% caso general con adyacente abajo atras
bottom([Head|Tail], L, Index, Row):-
    Head is 0, successor(Index, Next_Index),
    predecessor(Index, Pre_Index), nth0(Pre_Index, L, Pre), 
    not(Pre is 0), sub_atom(Pre, 1, 1, _, Char), 
    bottom(Tail, L, Next_Index, Result), 
    concat_list([Char, "\\    ", Result], Row).
% caso general sin adyacentes
bottom([Head|Tail], L, Index, Row):-
    Head is 0, successor(Index, Next_Index),
    bottom(Tail, L, Next_Index, Result), 
    concat_list(["      ", Result], Row).
%









%  __    __    __    __
% /  \  /  \  /  \  /  \ 
%|    || WQ || WS || WA | 
% \__/  \__/  \  /  \  /  \
%   | BQ || BS ||    || WB |
%    \__/  \__/  \__/  \__/
%      |    || BA || BS |
%       \__/  \__/  \__/
% print_board([[0, "WQ", "WS", "WA"],["BQ", "BS", 0, "WB"],[0, "BA", "BS", 0]], TopCenterBottom).
%  
%  
% 