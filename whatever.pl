test():-
  get_pairs([1,2], [3,4], [3,4], Pairs),
  write(Pairs), write("\n").


get_pairs([H|T], L2, [P|Q], [ [H,P] |R]):- get_pairs([H|T], L2, Q, R).
get_pairs([H|T], L2, [P], [[H,P]|R]):- get_pairs(T, L2, L2, R).
get_pairs([H], _, [P], [[H,P]]).