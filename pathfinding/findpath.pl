% ------------------------------------------------------------------------------
% ------------------------------------------------------------------------------
% ------------------------------------------------------------------------------
% Simple brute-force pathfinding for testing
% Date: 2012-08-11
% Auteur:
% ------------------------------------------------------------------------------
% ------------------------------------------------------------------------------
% ------------------------------------------------------------------------------

:- module(findpath, [findpath/5]).

% ------------------------------------------------------------------------------
% ------------------------------------------------------------------------------
% ------------------------------------------------------------------------------
% findpath(): brute force path finding using negation as failure on Cost
% ex: start(S), findpath(r1, r3, S, P, Cost).
findpath(A, Z, S, P, MinCost) :-
 subpath(A, [Z], 0, S, P, MinCost),
 debug(findpath, "Found plan ~w with cost ~w~n", [P, MinCost]),
 not(subpath(A, [Z], 0, S, _, Cost), Cost < MinCost), !.

subpath(A, [A|P1], Cost1, _, [A|P1], Cost1).

subpath(A, [Y|P1], Cost1, S, P, Cost) :-
 adjacent(X, Y, CostXY, S),
 \+member(X, P1),
 Cost2 is Cost1 + CostXY,
 subpath(A, [X, Y|P1], Cost2, S, P, Cost).

%adjacent(X, Y, C, S) :- member(path(X, Y, C), S) ; member(path(Y, X, C), S).
adjacent(X, Y, C, S) :- member(path(X, Y, C), S).

not(P, Q) :- P, Q, !, fail ; true.

% ------------------------------------------------------------------------------
% ------------------------------------------------------------------------------
% Unit test
:- begin_tests(findpath).

test(1) :-
 S = [path(a, b, 1), path(b, a, 1), path(b, d, 2), path(b, c, 1), path(c, d, 1)],
 findpath(a, d, S, P, _),
 format("Test 1 : Plan = ~w, ", [P]), P = [a, b, d], !.

test(2) :-
 S = [
  path(a, b, 1), path(a, d, 1), path(a, e, 1),
  path(b, a, 1), /*path(b, c, 1),*/ path(b, e, 1),
  path(c, b, 1), path(c, d, 1), path(c, e, 1),
  path(d, c, 1), path(d, a, 1), path(d, e, 1),
  path(e, a, 1), path(e, b, 1), path(e, c, 1), path(e, d, 1)
 ],
 findpath(b, c, S, P, _),
 format("Test 2 : Plan = ~w, ", [P]), P = [b,e,c], !.
 
test(3) :-
 S = [path(a, b, 1), path(b, a, 1), path(b, d, 2), path(b, c, 1), path(c, d, 1)],
 findpath(b, b, S, P, _),
 format("Test 3 : Plan = ~w~n", [P]), P = [b], !.
 
:- end_tests(findpath).

% ------------------------------------------------------------------------------
% ------------------------------------------------------------------------------
:- run_tests(findpath).