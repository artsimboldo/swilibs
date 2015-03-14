/*******************************************************************************
PROLOG STATECHARTS
 Auteur: rv, derived from E. Zimanyi, Statecharts and object-oriented
             development : a CASE perspective [1995]
 2012-12-08: creation
 2012-12-31: v1
 Done:
  - hierarchical states
  - exclusive / default states
  - concurrent states
  - simple transitions
  - entering, transition actions
 Todo:
  - splitting, merging transitions
  - conditions checking
  - history
  - exiting actions
  - situation calculus

*******************************************************************************/

:- multifile statechart/1, transition/5, state/2, concurrent/2, default_state/3.
%:- style_check(-singleton).

/* Statechart test 1 : a simple light switch
   Exclusive states only
   Default and transition actions
*/
statechart(light-switchSC).
 state(light-on, light-switchSC).
 state(light-off, light-switchSC).
 default_state(entering_action, light-off, light-switchSC).

transition(on, no_cond, lighting_on_action, light-off, light-on).
transition(off, no_cond, no_action, light-on, light-off).

/* Statechart test 2 : a simple coffee machine
   Exclusive and concurrent states
*/
statechart(coffee-machineSC).
 state(espresso, coffee-machineSC).
 state(espresso-with-sugar, coffee-machineSC).
 default_state(no_action, espresso, coffee-machineSC).

statechart(espresso-with-sugar).
 concurrent(coffee, espresso-with-sugar).
 concurrent(sugar, espresso-with-sugar).

statechart(sugar).
 state(single, sugar).
 state(double, sugar).
 default_state(no_action, single, sugar).

transition(select-sugar, no_cond, display_sugar_single, espresso, espresso-with-sugar).
transition(select-espresso, no_cond, display_no_sugar, espresso-with-sugar, espresso).
transition(select-double, no_cond, display_sugar_double, single, double).
transition(select-single, no_cond, display_sugar_single, double, single).
transition(select-double, no_cond, display_sugar_double, espresso, double).

/* Statechart test 3 : a digital watch
   Exclusive, Concurrent states
   Concurrent, Splitting and Merging transitions
*/
statechart(watchSC).
 state(dead, watchSC).
 state(alive, watchSC).
 default_state(no_action, alive, watchSC).

statechart(alive).
 state(hour, alive).
 state(stopwatch, alive).
 default_state(no_action, stopwatch, alive).

statechart(stopwatch).
 state(reset, stopwatch).
 state(operation, stopwatch).
 default_state(no_action, operation, stopwatch).

statechart(operation).
 concurrent(timer, operation).
 concurrent(display, operation).

statechart(timer).
 state(on, timer).
 state(off, timer).
 default_state(no_action, on, timer).

statechart(display).
 state(normal, display).
 state(insec, display).
 default_state(no_action, normal, display).

transition(b2, no_cond, no_action, off, on).
transition(b4, in(off), no_action, operation, dead).
transition(b4, not(in(off)), no_action, operation, reset).
sp_transition(b6, no_cond, no_action, hour, [on, normal]).
mg_transition(b6, no_cond, no_action, [insec, off], hour).
conc_transition(b6, no_cond, beep, [normal, on], [off, insec]).

% ------------------------------------------------------------------------------
/* unit test */
:- begin_tests(statecharts).
/*
test(create) :-
 format("~n*******************************************************~nTEST 0:~n"),
 clean(switch1),
 create(switch1, light-switchSC),
 show(switch1),
 clean(machine1),
 create(machine1, coffee-machineSC),
 show(machine1).
*/
test(lightswitch) :-
 format("~n*******************************************************~nTEST 1:~n"),
 clean(switch1),
 create(switch1, light-switchSC),
 show(switch1),
 sendEvent(on, switch1),
 show(switch1),
 sendEvent(on, switch1),
 show(switch1),
 sendEvent(off, switch1),
 show(switch1),
 sendEvent(off, switch1),
 show(switch1).

test(coffee_machine) :-
 format("~n*******************************************************~nTEST 2:~n"),
 clean(machine1),
 create(machine1, coffee-machineSC),
 show(machine1),
 sendEvent(select-sugar, machine1),
 show(machine1),
 sendEvent(select-espresso, machine1),
 show(machine1),
 sendEvent(select-double, machine1),
 show(machine1).

/*
test(arrive_leave) :-
 format("~n*******************************************************~nTEST 3:~n"),
 %
 clean(switch1),
 create(switch1, light-switchSC),
 arrive_state(switch1, on),
 show(switch1),
 leave_state(switch1, on),
 %
 clean(machine1),
 create(machine1, coffee-machineSC),
 arrive_state(machine1, espresso-with-sugar),
 show(machine1),
 leave_state(machine1, espresso-with-sugar).
*/

/*
test(included_eq) :-
 format("~n*******************************************************~nTEST 4:~n"),
 clean(watch1), create(watch1, watchSC),
 state(SubState, alive), included_eq(timer, SubState), !,
 format("~nSelected substate from '~w' to '~w' is '~w'~n", [alive, timer, SubState]),
 SubState == stopwatch.
*/

:- end_tests(statecharts).

% ------------------------------------------------------------------------------
/* Statechart creation */
create(SC, TopState) :-
 assert(initialized(SC, TopState)),
 compile_statechart(TopState),
 arrive_state(SC, TopState).

/* */
clean(SC) :-
 retractall(initialized(SC, _)),
 retractall(in_state(SC, _, _)),
 retractall(path(_,_)).

/* compile_statediag */
compile_statechart(State) :- compute_path(State, [State]).
compile_statechart(_).

compute_path(State, Path) :-
 (state(SubState, State) ; concurrent(SubState, State)),
 assert(path(SubState, [SubState|Path])),
% format("asserting path for '~w' = '~w'~n", [SubState, [SubState|Path]]),
 compute_path(SubState, [SubState|Path]).

/* Activate / desactivate states
TODO : take into account history
*/

% activate state
arrive_state(SC, State) :-
 exec_entry_action(SC, State),
 mark_arrival_state(SC, State),
 arrive_substates(SC, State).

arrive_state(_, _).

exec_entry_action(SC, State) :-
 default_state(Action, _, State),
 exec_action(SC, Action), !.

exec_entry_action(_, _).

arrive_substates(SC, State) :-
 concurrent(SubState, State),
% format("Arriving to concurrent substate'~w'.~n", [SubState]),
 mark_arrival_state(SC, SubState),
 arrive_substates(SC, SubState).

arrive_substates(SC, State) :-
 state(SubState, State),
 default_state(_, SubState, State), !,
% format("Arriving to exclusive substate '~w' (default).~n", [SubState]),
 mark_arrival_state(SC, SubState),
 arrive_substates(SC, SubState).

mark_arrival_state(SC, State) :-
 format("'~w' activated.~n", [State]),
 assert(in_state(SC, State, true)).

% desactivate state
leave_state(SC, State) :-
% format("leaving state '~w'.~n", [State]),
 leave_substates(SC, State),
 mark_leaving_state(SC, State).

leave_substates(SC, State) :- leave_substate(SC, State).
leave_substates(_, _).

leave_substate(SC, State) :-
 concurrent(SubState, State),
% format("leaving concurrent substate '~w'.~n", [SubState]),
 mark_leaving_state(SC, SubState),
 leave_substate(SC, SubState).

leave_substate(SC, State) :-
 state(SubState, State),
% format("leaving exclusive substate '~w'.~n", [SubState]),
 mark_leaving_state(SC, SubState),
 leave_substate(SC, SubState).

mark_leaving_state(SC, State) :-
 format("'~w' desactivated.~n", [State]),
 retract(in_state(SC, State, _)).

% ------------------------------------------------------------------------------
% Actions processing
/* execute action if any
*/
exec_action(_, no_action).
exec_action(SC, Action) :- format("'~w' execute action: '~w'.~n", [SC, Action]).

% ------------------------------------------------------------------------------
% Validate conditions
% TODO
check_cond(_, _).

% ------------------------------------------------------------------------------
/* Trigger a transition by sending event Ev to statechart SC
 TODO : merging, splitting events
*/
sendEvent(Ev, SC) :-
 initialized(SC, TopState),
 format("=>Send event '~w' to ~w~n", [Ev, TopState]),
 processEvent(Ev, SC, TopState).

processEvent(Ev, SC, State) :-
 in_state(SC, State, _),
 transition(Ev, Conditions, Action, State, DesState),
 format("Event '~w' triggers '~w' to '~w' in '~w'~n", [Ev, State, DesState, SC]),
 check_cond(SC, Conditions),
 exec_action(SC, Action), !,
 processTransition(SC, DesState, DesState).

/* exclusive states */
processEvent(Ev, SC, State) :-
 state(SubState, State),
 in_state(SC, SubState, _),
 format("Process exclusive active state '~w'~n", [SubState]), !,
 processEvent(Ev, SC, SubState).

/* concurrent states */
processEvent(Ev, SC, State) :-
 concurrent(SubState, State),
 format("Process concurrent state '~w'~n", [SubState]),
 processEvent(Ev, SC, SubState), fail.

processEvent(_, _, _).

/* process transitions */
% if DesState active, make transition
processTransition(SC, DesState, DesState) :-
 in_state(SC, DesState, _), !,
 format("'~w' is activated, make transition to '~w' (processTransition)~n", [DesState, DesState]),
 leave_state(SC, DesState), !,
 makeTransition(SC, DesState, DesState).

% if DesState not active, find Nearest Activ Common Ancestor of the DesState
processTransition(SC, OrState, DesState) :-
 format("'~w' is not activated, find NACADS~n", [DesState]),
 findNACADS(SC, OrState, DesState).

% found NACADS : make transition
findNACADS(SC, OrState, DesState) :-
 in_state(SC, OrState, _),
 format("'~w' is activated, make transition to '~w' (findNACADS)~n", [OrState, DesState]),
 leave_substates(SC, OrState), !,
 makeTransition(SC, OrState, DesState).

% search for upper NACADS
findNACADS(SC, OrState, DesState) :-
 path(OrState, [OrState, SupState|_]), !,
% format("path '~w' '~w' to '~w'~n", [OrState, SupState, DesState]),
 findNACADS(SC, SupState, DesState).

% make transition
makeTransition(SC, DesState, DesState) :-
 format("makeTransition (path NACADS to destination arrived) DesState = '~w'~n", [DesState]),
 !, arrive_state(SC, DesState).

% exclusive states
makeTransition(SC, OrState, DesState) :-
 format("makeTransition (exclusive) OrState = '~w' DesState = '~w'~n", [OrState, DesState]),
 state(_, OrState),
 mark_arrival_state(SC, OrState),
 state(SubState, OrState),
 included_eq(DesState, SubState), !,
 makeTransition(SC, SubState, DesState).

% concurrent states
makeTransition(SC, OrState, DesState) :-
 mark_arrival_state(SC, OrState),
 concurrent(SubState, OrState),
 (included_eq(DesState, SubState) ->
   format("makeTransition (concurrent1) OrState = '~w' DesState = '~w'~n", [OrState, DesState]),
   makeTransition(SC, SubState, DesState)
 ;
   format("makeTransition (concurrent2) OrState = '~w' DesState = '~w'~n", [OrState, DesState]),
   arrive_state(SC, SubState)
 ), fail.

makeTransition(_, _, _).

% validate SubState includes DesState
included_eq(DesState, SubState) :-
 path(DesState, DesPath),
 member(SubState, DesPath).

% ------------------------------------------------------------------------------
/* Show statechart status */
show(SC) :-
 initialized(SC, State),
 writeln('-------------------------------------------------------------------'),
 format("Statechart '~w':~n", [SC]),
 displayState(SC, State),
 showSubState(SC, State).

show(_) :-
 writeln('-------------------------------------------------------------------').

showSubState(SC, State) :-
 (state(SubState, State) ; concurrent(SubState, State)),
 displayState(SC, SubState),
 showSubState(SC, SubState).

displayState(SC, State) :-
 in_state(SC, State, _), !,
 format(" '~w' activated~n", [State]).

displayState(_, _).

/* */
:- run_tests(statecharts).
