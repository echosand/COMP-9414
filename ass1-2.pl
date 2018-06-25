likes(mary, apple).
likes(mary, pear).
likes(mary, grapes).
likes(tim, mango).
likes(tim, apple).
likes(jane, apple).
likes(jane, mango).


all_likes_all([Person|Restperson],Fruit):-
        all_likes_all(Person,Fruit),
        all_likes_all(Restperson,Fruit).
all_likes_all(Person,[H|T]):-
        all_likes_all(Person,T),
        likes(Person,H).
all_likes_all([],_).
all_likes_all(_,[]).
