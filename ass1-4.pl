%findhead
findhead([],[]).
findhead([A],[A]).
findhead([A,B|X],Tail):-
    A-1 =:= B,
    findhead([B|X],Tail).
findhead([A,B|X],[A|T]):-
    A-1 =\= B,
    findhead([B|X],T).
%findtail
findtail([],[]).
findtail([A],[A]).
findtail([A,B|X],Tail):-
    A+1 =:= B,
    findtail([B|X],Tail).
findtail([A,B|X],[A|T]):-
    A+1 =\= B,
    findtail([B|X],T).
%reverse
accRev([H|T],A,R):-
       accRev(T,[H|A],R).
accRev([],A,A).
%findheadlist
findheadlist(L1,L2):-
    accRev(L1,[],L3),
    findhead(L3,L4),
    accRev(L4,[],L2).
%findheadlist =L1
%findtail = L2
bond([A],[A],[A]).
bond([A],[B],[[A,B]]):-
    A \=B.
bond([H1|T1],[H2|T2],[H1|T]):-
    H1=H2,
    bond(T1,T2,T).
bond([H1|T1],[H2|T2],[[H1,H2]|T]):-
    H1 \= H2,
    bond(T1,T2,T).
%main program     
chop_up(L,Result):-
        findheadlist(L,L1),
        findtail(L,L2),
        bond(L1,L2,Result).        
 
