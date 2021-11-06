:- module(list_utils, [concatenar/3, contains_x/2, subset/2, replace_nth0/5]).

concatenar([],L,L).
concatenar([Head|Tail], List, [Head|Rest]):- concatenar(Tail,List,Rest).

contains_x(X,[Y|Z]):-X=Y;contains_x(X,Z).

reverse([],[]).
reverse([X|Y],L):-reverse(Y,L1),concatenar(L1,[X],L).

palindrome(X):-reverse(X,X).

subset([],_).
subset([X|Xs],Y):-contains_x(X,Y),subset(Xs,Y).


replace_nth0(List, Index, OldElem, NewElem, NewList) :-
  nth0(Index,List,OldElem,Transfer),
  nth0(Index,NewList,NewElem,Transfer).