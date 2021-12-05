:- module(board, [board/2]).

printLine([]):-write("\n").
printLine([Head|Tail]):- write(Head), printLine(Tail).

spaces(0, "").
spaces(N, S):-S1 = " ", N1 is N-1, spaces(N1, S2), concat(S1, S2, S).

%abs es para el modulo

replace_nth0(List, Index, OldElem, NewElem, NewList) :-
    nth0(Index,List,OldElem,Transfer),
    nth0(Index,NewList,NewElem,Transfer).

construct_board(N, N1, Board):-
    length(Board, N),
    length(List1, N1),
    maplist(=(0), List1),
    maplist(=(List1), Board).

sum(X, Y, R):- R is X + Y.
sum([X], X).
sum([X|Y], R):- sum(Y, R1), sum(X, R1, R).
resta(X, Y, R):- R is X - Y.
resta([X], X).
resta([X|Y], R):- resta(Y, R1), resta(X, R1, R).

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
board(Cells, NewBoard):- 
    nth0(0, Cells, Tupla1), nth0(0, Tupla1, W1), nth0(1, Tupla1, H1), 
    higher_at(Cells, 0, W1, Higher1), higher_at(Cells, 1, H1, Higher2),
    lower_at(Cells, 0, W1, Lower1), lower_at(Cells, 1, H1, Lower2),
    resta([Higher1, Lower1], R1), sum([R1, 1], Width), 
    resta([Higher2, Lower2], R2), sum([R2, 1], Heigth),
    %write(Heigth), write(" "), write(Width), write("\n"),
    construct_board(Width, Heigth, Board), !, 
    update_rows(Cells, Higher1, Higher2, Width, Heigth, Board, Printable_Board), 
    print_board(Printable_Board, NewBoard).

print_squared_board([]).
print_squared_board([Head|Tail]):-write(Head), write("\n"), print_squared_board(Tail).
%existe concat(X, Y, XY).
update_rows([], _, _, _, _, NewBoard, NewBoard).
update_rows([Head|Tail], Higher1, Higher2, Len1, Len2, Board, NewBoard):-
    nth0(0, Head, X), nth0(1, Head, Y), nth0(2, Head, T),
    % indice de la fila en el board
    resta([Higher1, X], R1), resta([Len1, R1], R),
    % indice de la columna en el board
    resta([Higher2, Y], R2), resta([Len2, R2], C),
    %la fila que se va a escoger
    R0 is R-1, C0 is C-1,
    nth0(R0, Board, Row), update_single_row(Row, C0, T, NewRow),
    replace_nth0(Board, R0, _, NewRow, Board1),
    update_rows(Tail, Higher1, Higher2, Len1, Len2, Board1, NewBoard).

update_single_row(Row, Index, X, NewRow):-
    replace_nth0(Row, Index, _, X, NewRow).
% board([[2, 3, "WQ"], [1, 4, "WS"], [4, 4, "BS"], [3, 4, "BQ"], [0, 4, "WB"]], B).

center([], "\n").
center([Head|Tail], Row):- 
    not(Head is 0), concat("| ", Head, S1), concat(S1, " |", S2), 
    center(Tail, S3), concat(S2, S3, Row).
center([Head|Tail], Row):- 
    Head is 0, S1 = "      ", center(Tail, S2), concat(S1, S2, Row).

top_first([], "\n").
top_first([Head|Tail], Row):- 
    not(Head is 0), S1 = "  __  ", 
    top_first(Tail, S2), concat(S1, S2, Row).
top_first([Head|Tail], Row):- 
    Head is 0, S1 = "      ", top_first(Tail, S2), concat(S1, S2, Row).

top_second([], "\n").
top_second([Head|Tail], Row):- 
    not(Head is 0), S1 = " /‾‾\\ ", 
    top_second(Tail, S2), concat(S1, S2, Row).
top_second([Head|Tail], Row):- 
    Head is 0, S1 = "      ", top_second(Tail, S2), concat(S1, S2, Row).

top(L, Top):- 
    %top_first(L, Top_first), 
    % top_second(L, Top_second),
    top_second(L, Top).
    % concat(Top_first, Top_second, Top).

bottom([], "\n").
bottom([Head|Tail], Row):- 
    not(Head is 0), S1 = " \\__/ ",
    bottom(Tail, S2), concat(S1, S2, Row).
bottom([Head|Tail], Row):- 
    Head is 0, S1 = "      ", bottom(Tail, S2), concat(S1, S2, Row).
bottom([], _, _, "\n").
bottom([Head|Tail], L, Index, Row):-
    not(Head is 0), length(L, Len), Len1 is Len-1, Index is Len1, 
    nth0(Index, L, Head1), 
    not(Head1 is 0), 
    S1 = " \\__/  \\",
    Next_Index is Index + 1, bottom(Tail, L, Next_Index, S2), concat(S1, S2, Row).
bottom([Head|Tail], L, Index, Row):-
    not(Head is 0), S1 = " \\__/ ",
    Next_Index is Index + 1, bottom(Tail, L, Next_Index, S2), concat(S1, S2, Row).
bottom([Head|Tail], L, Index, Row):-
    Head is 0, Next_Index is Index + 1, Pre_Index is Index-1, length(L, Len),
    Len1 is Len-1, Index is Len1,
    nth0(Pre_Index, L, Pre), nth0(Index, L, Down), 
    not(Pre is 0), not(Down is 0), S1 = " \\__/‾‾\\ ",
    bottom(Tail, L, Next_Index, S2), concat(S1, S2, Row).
bottom([Head|Tail], L, Index, Row):-
    Head is 0, Next_Index is Index + 1, Pre_Index is Index-1,
    nth0(Pre_Index, L, Pre), nth0(Index, L, Down), 
    not(Pre is 0), not(Down is 0), S1 = " \\__/ ",
    bottom(Tail, L, Next_Index, S2), concat(S1, S2, Row).
bottom([Head|Tail], L, Index, Row):-
    Head is 0, Next_Index is Index + 1, Pre_Index is Index-1, length(L, Len),
    Len1 is Len-1, Index is Len1,
    nth0(Pre_Index, L, Pre), not(Pre is 0), S1 = "  ‾‾\\ ",
    bottom(Tail, L, Next_Index, S2), concat(S1, S2, Row).
bottom([Head|Tail], L, Index, Row):-
        Head is 0, Next_Index is Index + 1, Pre_Index is Index-1,
        nth0(Pre_Index, L, Pre), not(Pre is 0), S1 = " \\    ",
        bottom(Tail, L, Next_Index, S2), concat(S1, S2, Row).
bottom([Head|Tail], L, Index, Row):-
        Head is 0, Next_Index is Index + 1,
        nth0(Index, L, Down), not(Down is 0), S1 = "    / ",
        bottom(Tail, L, Next_Index, S2), concat(S1, S2, Row).
bottom([Head|Tail], L, Index, Row):-
    Head is 0, Next_Index is Index + 1, S1 = "      ",
    bottom(Tail, L, Next_Index, S2), concat(S1, S2, Row).
% bottom([], _, _,  "\n").
% bottom([Head|Tail], L, I, Row):- 
%     not(I is 0), nth0(I, L, Head1), Index is I + 1,
%     not(Head1 is 0), nth0(0, Tail, Next), not(Next is 0),
%     not(Head is 0), S1 = " \\__/ ",
%     bottom(Tail, L, Index, S2), concat(S1, S2, Row).

% bottom([Head|Tail], L, I, Row):- 
%     not(I is 0), nth0(I, L, Head1), Index is I + 1,
%     not(Head1 is 0), nth0(0, Tail, Next), Next is 0,
%     not(Head is 0), S1 = " \\__/ ",
%     bottom(Tail, L, Index, S2), concat(S1, S2, Row).
% bottom([Head|Tail], L, I, Row):- 
%     not(I is 0), nth0(I, L, Head1), Index is I + 1,
%     not(Head1 is 0), not(nth0(Index, L, _)), 
%     not(Head is 0), S1 = " \\__/  \\ ",
%     bottom(Tail, L, Index, S2), concat(S1, S2, Row).

% % bottom([Head|Tail], L, I, Row):- 
% %     not(I is 0), 
% %     nth0(I, L, Head1), not(Head1 is 0), 
% %     not(Head is 0), S1 = " \\__/  \\", Index is I + 1,
% %     bottom(Tail, L, Index, S2), concat(S1, S2, Row).

% bottom([Head|Tail], L, I, Row):- 
%     not(Head is 0), S1 = " \\__/ ", Index is I + 1,
%     bottom(Tail, L, Index, S2), concat(S1, S2, Row).

% bottom([Head|Tail], L, I, Row):- 
%     not(I is 0),
%     Head is 0, I2 is I-1, Index is I + 1,
%     nth0(I2, L, Head1), nth0(I, L, Head2), 
%     not(Head1 is 0), not(Head2 is 0),
%     S1 = " \\__/ ", 
%     bottom(Tail, L, Index, S2), concat(S1, S2, Row).


% bottom([Head|Tail], L, I, Row):- 
%     not(I is 0),
%     Head is 0, I2 is I-1, Index is I + 1,
%     nth0(I2, L, Head1), nth0(I, L, Head2),
%     not(Head1 is 0), Head2 is 0,
%     S1 = " \\    ", 
%     bottom(Tail, L, Index, S2), concat(S1, S2, Row).

% bottom([Head|Tail], L, I, Row):- 
%     not(I is 0),
%     Head is 0, I2 is I-1, Index is I+1, 
%     nth0(I2, L, Head1), nth0(I, L, Head2), 
%     not(Head2 is 0), Head1 is 0,
%     S1 = "    / ",
%     bottom(Tail, L, Index, S2), concat(S1, S2, Row).

% bottom([Head|Tail], L, I, Row):- 
%     I is 0, 
%     Index is I + 1,
%     nth0(I, L, Head1), not(Head1 is 0), Head is 0,
%     S1 = "    / ",
%     bottom(Tail, L, Index, S2), concat(S1, S2, Row).
% bottom([Head|Tail], L, I, Row):- 
%     I is 0, 
%     Index is I + 1,
%     nth0(I, L, Head1), Head1 is 0, Head is 0,
%     S1 = "      ",
%     bottom(Tail, L, Index, S2), concat(S1, S2, Row).
% center([0, "BQ", 0, 0, "WQ"], A).
% bottom([0, "BQ", 0, 0, "WQ"], A).

test:-print_board([[0, "WQ", "WS", "WA"],["BQ", 0, 0, "WB"],["BA", 0, "BS", 0]], TopCenterBottom), 
write(TopCenterBottom), !.

print_board(L, TopCenterBottom):-
    length(L, Len), Len is 1, nth0(0, L, Head),
    top(Head, Top), center(Head, Center), bottom(Head, Bottom),
    concat(Top, Center, TopCenter), concat(TopCenter, Bottom, TopCenterBottom).

print_board(L, TopCenterBottom):-
    length(L, Len), Len > 1, nth0(0, L, Head),
    top(Head, Top), pretty_print_board(L, 0, CenterBottom),
    concat(Top, CenterBottom, TopCenterBottom).

pretty_print_board(L, Indent, CenterBottom):-
    length(L, Len), Len is 1, nth0(0, L, Head),
    center(Head, C), bottom(Head, B), S is Indent*3, spaces(S, Spaces),
    concat(Spaces, C, Center), concat(Spaces, B, Bottom), concat(Center, Bottom, CenterBottom).
pretty_print_board([Head1, Head2|Tail], Indent, CenterBottom):-
    center(Head1, C), bottom(Head1, Head2, 0, B), S is Indent*3, spaces(S, Spaces), 
    concat(Spaces, C, Center), concat(Spaces, B, Bottom), concat(Center, Bottom, CB),
    Next_Indent is Indent + 1, 
    pretty_print_board([Head2|Tail], Next_Indent, CB1), concat(CB, CB1, CenterBottom).








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