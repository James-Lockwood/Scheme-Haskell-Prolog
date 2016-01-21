%% James Lockwood 12305466
%%Question 1
%% We need to iterate over the lists in some fashion to interleave X and Y into Z or disassemble L3 into L1 and L2. 
%% We use recursive predicates to implement loop-like behaviour. 
%% Every recursive predicate needs some anchor.
interleave([],[],[]).
interleave([X|T1],[Y|T2],[X,Y|T3]) :- interleave(T1,T2,T3).

%%Question 2(with exmaples)
female(jessica).
female(ann).
female(dorothy).
parent(simon, james).
parent(simon, jessica).
parent(ann, james).
parent(ann, jessica).
parent(john, ann).
parent(dorothy, simon).
female(sophie).
female(claire).
male(tom).
parent(tom, claire).
parent(sophie, claire).
parent(john, tom).
parent(dorothy, tom).
male(simon).
male(john).
male(james).
father(X, Y) :- parent(X, Y), male(X).
mother(X, Y) :- parent(X, Y), female(X).
grandfather(X, Z) :- father(X, Y), parent(Y, Z).
cousin(X, Y) :- grandfather(Z, X), grandfather(Z,Y).
%%% this will always say that siblings are cousins and that someone is their own cousin
%%% to solve this you have to check to see if they have the same parents or not, if that's false, then happy days
