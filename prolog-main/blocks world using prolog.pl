% Blocks World Problem Solver in Prolog
% State representation: [on(Block, Location), clear(Block), handempty, holding(Block)]

% Main solve predicate
solve(InitialState, GoalState, Plan) :-
    search([[InitialState, []]], GoalState, Plan).

% Breadth-First Search
search([[State, Actions]|_], Goal, Actions) :-
    satisfies_goal(State, Goal).

search([[State, Actions]|Rest], Goal, Plan) :-
    findall(
        [NewState, NewActions],
        (
            applicable_action(State, Action),
            apply_action(State, Action, NewState),
            \+ member([NewState, _], [[State, Actions]|Rest]),
            append(Actions, [Action], NewActions)
        ),
        NewPaths
    ),
    append(Rest, NewPaths, NewQueue),
    search(NewQueue, Goal, Plan).

% Check if current state satisfies all goal conditions
satisfies_goal(State, []).
satisfies_goal(State, [Goal|RestGoals]) :-
    member(Goal, State),
    satisfies_goal(State, RestGoals).

% ============= ACTION DEFINITIONS =============

% Action 1: PICKUP - Pick a block from the table
applicable_action(State, pickup(Block)) :-
    member(on(Block, table), State),
    member(clear(Block), State),
    member(handempty, State).

% Action 2: PUTDOWN - Put held block on the table
applicable_action(State, putdown(Block)) :-
    member(holding(Block), State).

% Action 3: UNSTACK - Remove a block from another block
applicable_action(State, unstack(Block, FromBlock)) :-
    member(on(Block, FromBlock), State),
    member(clear(Block), State),
    member(handempty, State),
    FromBlock \= table.

% Action 4: STACK - Place held block on another block
applicable_action(State, stack(Block, OnBlock)) :-
    member(holding(Block), State),
    member(clear(OnBlock), State),
    Block \= OnBlock.

% ============= ACTION EFFECTS =============

% Apply PICKUP action
apply_action(State, pickup(Block), NewState) :-
    delete_all(State, [on(Block, table), clear(Block), handempty], TempState),
    append(TempState, [holding(Block)], NewState).

% Apply PUTDOWN action
apply_action(State, putdown(Block), NewState) :-
    delete_all(State, [holding(Block)], TempState),
    append(TempState, [on(Block, table), clear(Block), handempty], NewState).

% Apply UNSTACK action
apply_action(State, unstack(Block, FromBlock), NewState) :-
    delete_all(State, [on(Block, FromBlock), clear(Block), handempty], TempState),
    append(TempState, [holding(Block), clear(FromBlock)], NewState).

% Apply STACK action
apply_action(State, stack(Block, OnBlock), NewState) :-
    delete_all(State, [holding(Block), clear(OnBlock)], TempState),
    append(TempState, [on(Block, OnBlock), clear(Block), handempty], NewState).

% ============= UTILITY PREDICATES =============

% Delete multiple elements from a list
delete_all(List, [], List).
delete_all(List, [H|T], Result) :-
    delete_one(List, H, TempList),
    delete_all(TempList, T, Result).

% Delete one element from a list
delete_one([], _, []).
delete_one([H|T], H, T) :- !.
delete_one([H|T], X, [H|R]) :-
    delete_one(T, X, R).

% ============= DISPLAY PREDICATES =============

% Display the plan
display_plan([]) :-
    write('Plan complete!'), nl.
display_plan([Action|Rest]) :-
    write('Action: '), write(Action), nl,
    display_plan(Rest).

% Display state in readable format
display_state([]) :- nl.
display_state([Fact|Rest]) :-
    write('  '), write(Fact), nl,
    display_state(Rest).

% ============= EXAMPLE PROBLEMS =============

% Example 1: Simple stack - Build A on B on C
example1(Initial, Goal) :-
    Initial = [
        on(a, table), on(b, table), on(c, table),
        clear(a), clear(b), clear(c),
        handempty
    ],
    Goal = [
        on(a, b), on(b, c), on(c, table)
    ].

% Example 2: Reverse stack - Convert A-B-C to C-B-A
example2(Initial, Goal) :-
    Initial = [
        on(a, b), on(b, c), on(c, table),
        clear(a),
        handempty
    ],
    Goal = [
        on(c, b), on(b, a), on(a, table)
    ].

% Example 3: Rearrange - Move blocks to different configuration
example3(Initial, Goal) :-
    Initial = [
        on(a, b), on(b, table), on(c, table),
        clear(a), clear(c),
        handempty
    ],
    Goal = [
        on(a, table), on(b, c), on(c, table)
    ].

% Example 4: Complex - Four blocks rearrangement
example4(Initial, Goal) :-
    Initial = [
        on(a, table), on(b, table), on(c, d), on(d, table),
        clear(a), clear(b), clear(c),
        handempty
    ],
    Goal = [
        on(d, c), on(c, b), on(b, a), on(a, table)
    ].

% ============= USAGE EXAMPLES =============
%
% Run Example 1:
% ?- example1(I, G), solve(I, G, Plan), display_plan(Plan).
%
% Run Example 2:
% ?- example2(I, G), solve(I, G, Plan), display_plan(Plan).
%
% Run Example 3:
% ?- example3(I, G), solve(I, G, Plan), display_plan(Plan).
%
% Custom problem:
% ?- Initial = [on(a,table), on(b,table), clear(a), clear(b), handempty],
%    Goal = [on(a,b)],
%    solve(Initial, Goal, Plan),
%    display_plan(Plan).
%
% Show state transitions:
% ?- example1(I, G), solve(I, G, Plan), 
%    write('Initial: '), nl, display_state(I),
%    write('Plan: '), nl, display_plan(Plan),
%    write('Goal: '), nl, display_state(G).