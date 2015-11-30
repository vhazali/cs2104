father(john, mary).
father(john, tom).
father(kevin, john).
mother(eva, mary).
mother(eva, tom).
mother(cristina, john).
mother(cristina, kelly).
mother(kelly, alloy).
male(john).
male(kevin).
male(tom).
male(alloy).
female(X):-not(male(X)).
parent(X,Y) :- father(X,Y).
parent(X,Y) :- mother(X,Y).
grandparent(X,Y) :- parent(X,Z),parent(Z,Y).
daughter(X,Y) :- female(X),parent(Y,X).
sibling(X,Y) :- parent(Z,X),parent(Z,Y),X\==Y.
ancestor(X,Y) :- parent(X,Y).
ancestor(X,Y) :- parent(X,Z),ancestor(Z,Y).
/* Q1 */
append([],Y,Y).
append([X|Xs],Y,[X|Rs]):-append(Xs,Y,Rs).
rev([],[]).
rev([X|Xs],Y) :- rev(Xs,Y2),append(Y2,[X],Y).
rev2([],[]).
rev2(.(X,Xs),Y) :- rev2(Xs,Y2),append(Y2,[X],Y).
fact(0,1).
fact(N,R) :- N>0, M is N-1, fact(M,R1), R is N*R1.
select2(X,[X]).
select2(X,[_|T]):-select2(X,T).
/*
grandparent(G,mary).
ancestor(A,mary)
sibling(S,mary)
*/
/* Q2
   aunt(X,Y) means X is aunt of Y
   cousin(X,Y) means X is cousin of Y
   nephew(X,Y) means X is nephew of Y
*/
aunt(X,Y) :- parent(Z,Y),sibling(X,Z),female(X).
cousin(X,Y) :- parent(A,X),parent(B,Y),sibling(A,B).
nephew(X,Y) :- male(X),parent(Z,X),sibling(Z,Y).

/* Q3
 last(Xs,X) :- X is the last element of Xs
 len(Xs,N) - N is the length of Xs
 nth(Xs,I,X) - X is the I-th element of Xs, starting from 0.
 occurs(Ys,X,N) - X occurs N-times in Ys.
 You may use the finite constraint solving package.
*/
last([X],R) :- R = X.
last([X,Y|Rs],R) :- last([Y|Rs],R).

:- use_module(library(clpfd)).
len([],0).
len([_|Xs],N) :- M #= N-1, N #> 0, len(Xs,M).

nth(Xs,N,X) :- fail.

occurs(Ys,X,N) :- fail.


/* Q5
   Hint: you may use mod operation
   ?- divisors(30,X).
	X = [1,2,3,5,6,10,15,30]
*/

divisors(M,X):-integer(M),fail.






