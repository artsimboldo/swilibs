% ------------------------------------------------------------------------------
% ------------------------------------------------------------------------------
% ------------------------------------------------------------------------------
% EFFECTS : add, delete or replace fluents from a situation S resulting in S1
% Auteur: rv
% Date: 2013-04-20
% ------------------------------------------------------------------------------
% ------------------------------------------------------------------------------
% ------------------------------------------------------------------------------

:- module(effects, [replaces/4, adds/3, dels/3]).

% ------------------------------------------------------------------------------
% replace X by Y from L in Result
replaces([], _, _, []).
replaces([X|T], X, Y, [Y|Result]) :- replaces(T, X, Y, Result), !.
replaces([H|T], X, Y, [H|Result]) :- replaces(T, X, Y, Result).

% ------------------------------------------------------------------------------
% adds [fluents] to L1 resulting in L2
% check for doublons, can easily occur if users are selecting the same actions several times
adds([], L, L).

% TODO : find a simpler way...
adds([H|T], L1, [H1|L2]) :-
 functor(H, add, 2), !,
 % retrieve fluent and value to add
 arg(1, H, Fluent),
 arg(2, H, Value), number(Value),
 % query base value in L1, depending on it's a simple value or a compound/1
 functor(Fluent, Fun, A),
 (A == 0 ->
  Query =.. [Fluent, Base],
  H1 =.. [Fluent, NewValue] ;
  arg(1, Fluent, Arg),
  Query =.. [Fun, Arg, Base],
  H1 =.. [Fun, Arg, NewValue]),
 % get base value
 member(Query, L1), number(Base),
 % remove base value
 delete(L1, Query, L3),
 % unify new value
 NewValue is Base + Value,
% format("adds: Base ~w + Value ~w = NewValue ~w~n", [Base, Value, NewValue]),
 adds(T, L3, L2).

adds([H|T], L1, L2) :- member(H, L1), !, adds(T, L1, L2). % avoid doublons
adds([H|T], L1, [H|L2]) :- adds(T, L1, L2).

% ------------------------------------------------------------------------------
% delete [fluents] to L1 resulting in L2
% doesn't check if fluent doesn't exists, will fail and indicate a probable default in logic
dels([], L, L).
%dels([H|T], L1, L2) :- \+member(H, L1), !, dels(T, L1, L2). % robustness if fluent doesn't exist
dels([H|T], L1, L2) :- delete(L1, H, Temp), dels(T, Temp, L2).

% ------------------------------------------------------------------------------
% ------------------------------------------------------------------------------
% Unit tests
% ------------------------------------------------------------------------------
% ------------------------------------------------------------------------------

:- begin_tests(effects).

test(replace) :- replaces([a], a, b, L), L = [b], !.
test(adds) :- adds([a], [], S), /*test no doublons:*/ adds([a], S, S1), S1 = [a], !.
test(adds_value) :- adds([add(a, 1), add(health(a), 1)], [a(1), health(a, 1)], S1), S1 = [a(2), health(a, 2)], !.
test(dels) :- S = [a, b, c], dels([b], S, S1), S1 = [a, c], !.

:- end_tests(effects).
:- run_tests(effects).
