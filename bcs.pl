


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Target predicate:
target(daughter,2).

% Available body predicates:
language(parent,2).
language(female,1).

% Positive examples:
pos(daughter(louise, edward)).
pos(daughter(louise, sophie)).

% Negative examples:
neg(daughter(louise, elizabeth)).
neg(daughter(diana, charles)).
neg(daughter(harry, diana)).
neg(daughter(diana, sophie)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

exec_body([]).
exec_body([H|T]) :- call(H), exec_body(T).


examples_satisfied(cl(H,_)) :- \+ pos(H),!,fail. 
examples_satisfied(cl(H, B)) :- neg(H), exec_body(B), !, fail. 
examples_satisfied(cl(H, B)) :- pos(H), cor(B),!, fail. 
examples_satisfied(cl(_,B)) :- \+ cor(B).

cor(B):- \+ exec_body(B).



