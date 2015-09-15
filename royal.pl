
female(camilla).
female(diana).
female(elizabeth).
female(louise).
female(sophie).

male(charles).
male(edward).
male(george).
male(harry).
male(james).
male(philip).
male(william).

wife(camilla,charles).
wife(diana,charles).
wife(elizabeth,philip).
wife(sophie,edward).

parent(charles,harry).
parent(charles,william).
parent(diana,harry).
parent(diana,william).
parent(elizabeth, charles).
parent(elizabeth, edward).
parent(philip, charles).
parent(george, elizabeth).
parent(philip, edward).
parent(edward, james).
parent(edward, louise).
parent(sophie, james).
parent(sophie, louise).

husband(Man, Woman):- wife(Woman, Man).

person(P):-male(P).
person(P):-female(P).

mother(Mother, Child):- female(Mother), parent(Mother, Child).

father(Father, Child):- male(Father), parent(Father, Child).

sibling(S1, S2, Parent):- parent(Parent, S1), parent(Parent, S2), \+ member(S1, [S2]).

sib(S1, S2, Parent):-  parent(Parent, S1), parent(Parent, S2).


lgg_term(X,Y, X, List,List) :- X == Y, !.
lgg_term(X,Y, G, List,[s(X,Y,G)|List]) :- var(X); var(Y). 

lgg_term(X,Y, G, List,List) :- member(s(A,B,G), List), A == X, B == Y, !.

lgg_term(X,Y, G, In,Out) :- X =.. [P|Xa], Y =.. [P|Ya], lgg_list(Xa,Ya, Ga, In,Out), G =.. [P|Ga], !.

lgg_term(X,Y, G, List,[s(X,Y,G)|List]).

% LGG on lists of equal length
lgg_list([], [],[], Lst,Lst).
lgg_list([H1|T1],[H2|T2], [G|Rest], Beg,End):- lgg_term(H1,H2,G,Beg,Mid), lgg_list(T1,T2,Rest,Mid,End).

daughter(louise, edward).
daughter(elizabeth, george).

target(daughter,2).

language(parent,2).
language(female,1).

body_lit(Z):- language(X, Y), functor(Z, X, Y), call(Z).

head_lit(Z):- target(X, Y), functor(Z, X, Y), call(Z).

all_clauses(X):- findall(Y, head_lit(Y), HeadLit), findall(Z, body_lit(Z), BodyLit), spoj(HeadLit, BodyLit, X).

spojD(X, Y, cl(X, Y)). % :- append(X, [Y], R).

spoj([],_,[]).
spoj([H|T], Y, [R1|R2]):- spojD(H, Y, R1), spoj(T, Y, R2).

carthesian([], _, []).
carthesian([X|Y], Z, E):- dvojice(X, Z, C), carthesian(Y, Z, D), append(C, D, E).

dvojice(_, [], []).
dvojice(X, [H|T], [[X, H]|P]):-dvojice(X, T, P).

filter_compat(X, Out):- include(check, X, Out).

check([H|[T|_]]):-functor(H, A, B), functor(T, C, D), B=:=D, member(A, [C]).

lgg_clause(cl(H1, B1), cl(H2, B2), cl(G, R)):- lgg_term(H1, H2, G, [], Out), carthesian(B1, B2, E), filter_compat(E, F), ro(F, R, Out).

%moje([A|[B|_]], L, G, M):- lgg_term(A, B, L, M, G). %% tak tady dostanu [wife(), wife()]

ro([],[],_).
ro([[A|B]|T], [G|Res], L):-lgg(A, B, G, L, Out), ro(T, Res, Out).

%da([A|[B|_]], InL, OutL, Res):- lgg_term(A, B, Res, [], OutL).
%ro([A,B], Res):-da(A, [], Out, G), da(B, Out, OutOut, R), append([G], [R], Res).
%ro([A|B], Res):- da(A, [], O, G), ro(B, Res).





% lgg_clause(cl(husband(charles,camilla),[wife(camilla,charles)]),cl(husband(philip,elizabeth),[wife(elizabeth,philip),wife(diana,charles)]),A).




%parsuj([A|B], R):- moje(A, [], R, _).
%moje([], _, [], []).
%moje1([[A,B]|T], L, [G|R], K):- prvek(A, B, G, L, M), moje1(T, M, R, K).
%priprava([], _, []).
%priprava([A, L, G):- moje(A, L, G, _), label(A).
%priprava([H|T], L, [G|R]):- moje(H, L, G, M), priprava(T, M, R).
%prvek(A, B, L, M, G):- lgg_term(A, B, G, L, M).














