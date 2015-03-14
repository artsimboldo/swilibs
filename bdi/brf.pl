/*------------------------------------------------------------------------------
 brf : belief revision function
 Auteur: rv
 Date: 2013-04-08
 simple brf with contraction and expansion
 no reasoning, consolidation or merging
------------------------------------------------------------------------------*/

:- module(brf, [update/4]).
:- use_module('../common/list').
:- use_module('../sc/effects', [replaces/4, adds/3, dels/3]).

% ------------------------------------------------------------------------------
% Fluents templates
percepts_fluents([
 isa_region(_), isa_room(_), at(_,_), path(_,_,_), % regions
 isa_actor(_), health(_, _), % actors
 isa_prop(_), is_visible(_), isa_food(_, _), has(_,_), on(_,_), is_takeable(_), is_eatable(_), in_state(_,_)] % props
 ).

% ------------------------------------------------------------------------------
% UPDATE BELIEFS
%  1. perceive new beliefs X in current situation S
%  2. expand current beliefs B in Temp with X
%  3. perceive revised beliefs Y in Temp
%  4. compare Y with X, concat extra beliefs in Temp if exist, final result in B1
update(Agent, B, B1, S) :-
 perceive_beliefs(Agent, X, S),
 expand(X, B, Temp),
 perceive_beliefs(Agent, Y, Temp),
 concat(Y, X, Temp, B1),
 debug(brf, 'brf: beliefs           = ~w~n', [B]),
 debug(brf, 'brf: new beliefs       = ~w~n', [X]),
 debug(brf, 'brf: update beliefs    = ~w~n', [B1]).

% expand : simply add belief if it doesn't exist in current base
expand([], B, B).
expand([H|T], B, B1) :-
 \+member(H, B), !,
 adds([H], B, Temp),
 debug(brf, 'brf: add[~w]~n', [H]),
 expand(T, Temp, B1).
expand([_|T], B, B1) :- expand(T, B, B1).

% concat : del old belief if it doesn't exist in updated base
concat([], _, B, B).
concat([H|T], L, B, B1) :-
 \+member(H, L), !,
 dels([H], B, Temp),
 debug(brf, 'brf: del[~w]~n', [H]),
 concat(T, L, Temp, B1).
concat([_|T], L, B, B1) :- concat(T, L, B, B1).

% ------------------------------------------------------------------------------
% Retrieve new beliefs NewBeliefs from situation S
perceive_beliefs(Agent, NewBeliefs, S) :-
 member(at(Agent, Loc), S),
% region around
 perceive_fluents([Loc], [], Temp, S),
% actors around
 findall(X, rule_perceive_actors(Loc, X, S), L),
 perceive_fluents(L, Temp, Temp1, S),
% props around
 findall(X, rule_perceive_props(Loc, X, S), I),
 perceive_fluents(I, Temp1, Temp2, S),
% filter result to remove potential duplicata
 filter(Temp2, NewBeliefs).

rule_perceive_actors(Loc, X, S) :-
 member(isa_actor(X), S),
 member(is_visible(X), S),
 member(at(X, Loc), S).

rule_perceive_props(Loc, X, S) :-
 member(isa_prop(X), S),
 member(is_visible(X), S),
 at(X, Loc, S).

at(X, Y, S) :- member(at(X, Y), S).
at(X, Y, S) :-
 member(has(Z, X), S),
 member(is_visible(Z), S), % Z has me AND is visible
 at(Z, Y, S).

perceive_fluents([], B, B, _).
perceive_fluents([H|T], B, B1, S) :-
 percepts_fluents(L),
 perceive_fluent(H, L, B, Temp, S), !,
 perceive_fluents(T, Temp, B1, S).

% no more fluent to compare, end.
perceive_fluent(_, [], B, B, _).

% fluent's arity is 1
perceive_fluent(O, [H|T], B, B1, S) :-
 functor(H, _, 1),
 arg(1, H, O),
 member(H, S),
% format("brf: perceiving ~w~n", [H]),
 perceive_fluent(O, T, [H|B], B1, S).

% fluent's arity is 2
% check bijective relations (e.g. path(O,X) an path(X, O))
perceive_fluent(O, [H|T], B, B1, S) :-
 functor(H, F, 2),
 F1 =.. [F, O, X],
 F2 =.. [F, X, O],
 findall(F1, (member(F1, S), checkVisibility(X, S)), L1),
 findall(F2, (member(F2, S), checkVisibility(X, S)), L2), !,
% format("brf: perceiving ~w, ~w~n", [L1, L2]),
 conc(L1, L2, L3),
 conc(L3, B, Temp),
 perceive_fluent(O, T, Temp, B1, S).

% fluent's arity is 3
% process as arity 2 but ignore third parameter
perceive_fluent(O, [H|T], B, B1, S) :-
 functor(H, F, 3),
 F1 =.. [F, O, X, _],
 F2 =.. [F, X, O, _],
 findall(F1, (member(F1, S), checkVisibility(X, S)), L1),
 findall(F2, (member(F2, S), checkVisibility(X, S)), L2), !,
% format("brf: perceiving ~w, ~w~n", [L1, L2]),
 conc(L1, L2, L3),
 conc(L3, B, Temp),
 perceive_fluent(O, T, Temp, B1, S).

% else ignore fluent
perceive_fluent(O, [_|T], B, B1, S) :- perceive_fluent(O, T, B, B1, S).

checkVisibility(X, S) :-
% format("checkVisibility: ~w~n", [X]),
 \+member(isa_prop(X), S) ; (member(isa_prop(X), S), member(is_visible(X), S)).

/*------------------------------------------------------------------------------
 Unit tests
------------------------------------------------------------------------------*/
:- begin_tests(brf).
test(1) :-
 S = [
  isa_region(room),
  isa_region(exit),
  path(room, exit, 1),
  path(exit, room, 1),

  isa_prop(banana),
  is_visible(banana),
%  is_takeable(banana),

  isa_prop(fruit),
%  is_visible(fruit),
  has(banana, fruit),

  isa_prop(chair),
  at(chair, room),
  is_visible(chair),

  isa_prop(ceiling),
  at(ceiling, room),
  is_visible(ceiling),
  has(ceiling, banana),

  isa_actor(monkey),
  is_visible(monkey),
  at(monkey, room),
  agent(monkey, [], [], [], []),

  isa_actor(gorilla),
  is_visible(gorilla),
  at(gorilla, exit),
  agent(gorilla, [], [], [], [])
 ],
 debug(brf, '~n1:~n', []),
 update(monkey, [], B, S),
 debug(brf, 'Test 2.~n', []),
 dels([at(monkey, room)], S, S1),
 adds([at(monkey, exit)], S1, S2),
 update(monkey, B, B1, S2),
 debug(brf, 'Test 3:~n', []),
 dels([isa_region(room), at(monkey, exit), at(gorilla, exit), has(banana, fruit), is_visible(ceiling), path(room, exit, 1)], S2, S3),
 adds([isa_room(room), at(monkey, room), at(gorilla, room)], S3, S4),
 update(monkey, B1, _, S4),
 !.

:- end_tests(brf).
:- run_tests(brf).
