% 8-Puzzle Solver in Prolog
% State representation: list of 9 numbers [0-8], where 0 is empty space
% Layout: [Pos0, Pos1, Pos2, Pos3, Pos4, Pos5, Pos6, Pos7, Pos8]
%         which represents:
%         Pos0 Pos1 Pos2
%         Pos3 Pos4 Pos5
%         Pos6 Pos7 Pos8

% Goal state
goal([1,2,3,4,5,6,7,8,0]).

% Main solve predicate using BFS
solve(Start, Solution) :-
    bfs([[Start]], ReversedPath),
    reverse(ReversedPath, Solution).

% Breadth-First Search
bfs([[State|Path]|_], [State|Path]) :-
    goal(State).

bfs([[State|Path]|Queue], Solution) :-
    findall(
        [Next, State|Path],
        (move(State, Next), \+ member(Next, [State|Path])),
        NewPaths
    ),
    append(Queue, NewPaths, NewQueue),
    bfs(NewQueue, Solution).

% Find position of empty tile (0)
empty_pos(State, Pos) :-
    nth0(Pos, State, 0).

% Valid moves: swap empty tile with adjacent tile
move(State, NewState) :- move_left(State, NewState).
move(State, NewState) :- move_right(State, NewState).
move(State, NewState) :- move_up(State, NewState).
move(State, NewState) :- move_down(State, NewState).

% Move empty tile left
move_left(State, NewState) :-
    empty_pos(State, Pos),
    Pos mod 3 > 0,
    LeftPos is Pos - 1,
    swap_positions(State, Pos, LeftPos, NewState).

% Move empty tile right
move_right(State, NewState) :-
    empty_pos(State, Pos),
    Pos mod 3 < 2,
    RightPos is Pos + 1,
    swap_positions(State, Pos, RightPos, NewState).

% Move empty tile up
move_up(State, NewState) :-
    empty_pos(State, Pos),
    Pos > 2,
    UpPos is Pos - 3,
    swap_positions(State, Pos, UpPos, NewState).

% Move empty tile down
move_down(State, NewState) :-
    empty_pos(State, Pos),
    Pos < 6,
    DownPos is Pos + 3,
    swap_positions(State, Pos, DownPos, NewState).

% Swap two positions in the state
swap_positions(State, Pos1, Pos2, NewState) :-
    nth0(Pos1, State, Val1),
    nth0(Pos2, State, Val2),
    replace_at(State, Pos1, Val2, Temp),
    replace_at(Temp, Pos2, Val1, NewState).

% Replace element at position
replace_at([_|T], 0, X, [X|T]).
replace_at([H|T], Pos, X, [H|R]) :-
    Pos > 0,
    Pos1 is Pos - 1,
    replace_at(T, Pos1, X, R).

% Display a single state
display_state([A,B,C,D,E,F,G,H,I]) :-
    write_tile(A), write(' '), write_tile(B), write(' '), write_tile(C), nl,
    write_tile(D), write(' '), write_tile(E), write(' '), write_tile(F), nl,
    write_tile(G), write(' '), write_tile(H), write(' '), write_tile(I), nl, nl.

% Write tile (use space for 0)
write_tile(0) :- write('_').
write_tile(N) :- N > 0, write(N).

% Display entire solution
display_solution([]) :-
    write('Solution complete!'), nl.
display_solution([State|Rest]) :-
    display_state(State),
    display_solution(Rest).

% Check if puzzle is solvable
solvable(State) :-
    inversions(State, Count),
    Count mod 2 =:= 0.

% Count inversions (for solvability check)
inversions(State, Count) :-
    inversions(State, 0, Count).

inversions([], Count, Count).
inversions([0|Rest], Acc, Count) :-
    inversions(Rest, Acc, Count).
inversions([H|Rest], Acc, Count) :-
    H > 0,
    count_smaller(H, Rest, N),
    NewAcc is Acc + N,
    inversions(Rest, NewAcc, Count).

count_smaller(_, [], 0).
count_smaller(X, [0|Rest], Count) :-
    count_smaller(X, Rest, Count).
count_smaller(X, [H|Rest], Count) :-
    H > 0,
    (H < X -> 
        count_smaller(X, Rest, Count1), Count is Count1 + 1
    ;
        count_smaller(X, Rest, Count)
    ).

% Example queries:
%
% Easy (1 move):
% ?- solve([1,2,3,4,5,6,7,0,8], Sol), display_solution(Sol).
%
% Medium (4 moves):
% ?- solve([1,2,3,4,0,6,7,5,8], Sol), display_solution(Sol).
%
% Harder (8 moves):
% ?- solve([1,3,0,4,2,6,7,5,8], Sol), display_solution(Sol).
%
% Check if solvable:
% ?- solvable([1,2,3,4,5,6,8,7,0