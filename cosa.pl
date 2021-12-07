init_game().
queen 1 1
queen 2 1
ant 1 0
ant 2 2
1 0 2 0


init_game().
queen 1 1
queen 2 1
1 1 1 2


init_game().
queen 1 1
queen 2 1
spider 1 0
spider 2 2
1 0 3 1


init_game().
queen 1 1
queen 2 1
spider 1 0
spider 2 2

init_game().
queen 1 1
queen 2 1
ant 1 0
ant 2 2
1 0 3 2


init_game().
queen 1 1
queen 1 2
grasshoper 1 0
ant 2 2
1 0 1 3


init_game().
queen 1 1
queen 2 1
beetle 1 0
beetle 2 2
1 0 1 1
2 2 2 1
1 1 2 1


init_game().
queen 1 1
queen 2 1
ladybug 1 0
ladybug 2 2
1 0 2 3


init_game().
pillbug 1 1
pillbug 2 1
queen 1 0
queen 2 2
1 1 1 0 1 2
2 1 1 1 2 0
2 0 1 1


init_game().
ant 1 1
ladybug 2 1
1 1 1 2


init_game().
ant 0 1
queen 1 0
queen 2 0
ant 2 1
ant 1 2
ant 0 2
0 1 1 1

init_game().
queen 1 1
queen 1 2
ant 1 0

init_game().
ant 0 1
queen 1 0
queen 2 0
ant 2 1
ant 1 2
ant 0 2
0 1 1 1


bottom([Head|Tail], L, Index, Row):-
    Head is 0, Next_Index is Index + 1, Pre_Index is Index-1,
    nth0(Pre_Index, L, Pre), nth0(Index, L, Down), 
    not(Pre is 0), not(Down is 0), S1 = " \\__/ ",
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