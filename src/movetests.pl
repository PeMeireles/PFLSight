% Runs a series of tests to verify the correctness of the move/3 predicate.  
% If all tests pass, outputs a success message 'move tests passed'.
move_tests :-
  move_valid_tests2,
  move_valid_tests3,
  move_invalid_test,
  write('move tests passed'),nl.

% Tests the move/3 predicate for general valid moves involving stacking and simple piece movement.
move_valid_tests2 :-
  testBoard(2,Testboard),

  move([Testboard,a,0,0], [[2,4],[3,4]],[NewBoard, NewNext,_,_]),
  NewBoard = [[-,-,-,-,-],[-,a1,a1,-,-],[-,-,a2,-,-],[-,-,b1,b1,-],[-,-,-,-,-]],
  NewNext = b,

  move([Testboard,b,0,0], [[0,0],[4,4]], [NewBoard2, NewNext2,_,_]),
  NewNext2 = a,
  NewBoard2 = [[-,-,-,-,-],[-,a2,-,b1,-],[-,-,a1,-,-],[-,-,b1,b2,-],[-,-,-,-,-]].

% Tests the move/3 predicate for diagonal movements and position correctness.
move_valid_tests3 :-

  testBoard(3, Testboard),
  move([Testboard,a,0,0], [[2,3],[2,2]],[NewBoard, _,_,_]),
  NewBoard = [[-,a2,-,-,b3], 
              [-,a2,-,-,-],
              [-,a2,a2,-,-],
              [-,a1,b1,b1,-],
              [-,b3,-,-,-]],
  move([Testboard, a,0,0], [[2,3],[1,3]], [NewBoard2, _,_,_]),
  NewBoard2 =[[-,a2,-,-,b3], 
              [-,a3,-,-,-],
              [a1,a2,a1,-,-],
              [-,-,b1,b1,-],
              [-,b3,-,-,-]].

% Tests the move/3 predicate with move_valid_tests2 board but for invalid moves
move_invalid_test :-
  testBoard(2,Testboard),
  % wrong move type
  \+ move([Testboard,a,0,0], [[0,0],[1,1]],[_,_,_,_]),
  \+ move([Testboard,b,0,0], [[3,2],[3,1]],[NewBoard,_,_,_]),

  %wrong stack/non adjacent
  \+ move([Testboard,a,0,0], [[5,5],[3,4]],[_,_,_,_]),
  \+ move([Testboard,a,0,0], [[2,4],[5,5]],[_,_,_,_]).
  
%=======================================================================================================%


% Runs a series of tests for different board visibility scenarios to ensure correctness of in_sight/3.
% If all tests pass, outputs a success message 'all sight tests passed'.
in_sight_test :-
  in_sight4,
  in_sight5,
  test_edges_sight,
  test_corners_sight,
  test_full_box_sight,
  test_intersections_sight,
  write('all sight tests passed'), nl.

% Tests the in_sight/3 predicate for visibility of pieces from a given position general case and non adjacent pieces.
in_sight4 :-
  testBoard(4,Testboard),
  in_sight([Testboard, a,0,0], [3,3], Locations),
  sort(Locations, Temp),
  Temp = [[2,2],[2,4],[3,5],[4,2],[4,4]],
  in_sight([Testboard, a,0,0], [3,2], Locations2),
  sort(Locations2, Temp2),
  Temp2 = [[2,2],[3,5],[4,2]].

% Tests the in_sight/3 predicate for visibility when an oposite piece is in the way.
in_sight5 :-
  testBoard(si1,Testboard),
  in_sight([Testboard, b,0,0], [3,5], Locations),
  sort(Locations, Temp),
  Temp = [[3,4]].

% Tests visibility from positions near the board edges, with pieces only on diagonals using in_sight/3.
test_edges_sight :-
  testBoard(s1, Board),
  in_sight([Board, a, 0,0], [3,1], Loc1),
  sort(Loc1,Temp),
  Temp = [[1,1],[2,2],[3,5],[4,2],[5,1]],
  in_sight([Board, a,0,0], [1,3], Loc2),
  sort(Loc2, Temp2),
  Temp2 = [[1,1],[1,5],[2,2],[2,4],[5,3]].

% Tests visibility from corner positions, with pieces only on diagonals using in_sight/3.
test_corners_sight :-
  testBoard(s2, Board),
  in_sight([Board, a,0,0], [1,1], Loc1),
  sort(Loc1,Temp),
  Temp = [[1,3],[2,2],[3,1]],
  in_sight([Board, a,0,0], [5,5], Loc2),
  sort(Loc2,Temp2),
  Temp2 = [[3,5],[4,4],[5,3]],
  in_sight([Board, a,0,0], [5,2], Loc3),
  sort(Loc3,Temp3),
  Temp3 = [[4,2],[5,1],[5,3]].

% Tests visibility within a box-shaped region using in_sight/3.
test_full_box_sight :-
<<<<<<< HEAD
  testBoard(s3, Board),
  in_sight([Board, a,0,0], [3,3], Loc1),
  sort(Loc1, Temp),
  Temp = [[2,2],[2,3],[2,4],[3,2],[3,4],[4,2],[4,3],[4,4]],
  in_sight([Board, a,0,0], [3,4], Loc2),
  sort(Loc2, Temp2),
  Temp2 = [[2,4],[3,3],[3,5],[4,4]].

% Tests visibility from intersection points using in_sight/3.
test_intersections_sight :-
  testBoard(s4, Board),
  in_sight([Board, a,0,0], [2,2], Loc1),
  sort(Loc1, Temp),
  Temp = [[1,1],[1,3],[3,1],[3,3]],
  in_sight([Board, a,0,0], [3,2], Loc2),
  sort(Loc2, Temp2),
  Temp2 = [[3,1],[3,3]].
=======
    testBoard(s3, Board),
    in_sight([Board, a,0,0], [3,3], Loc1), %intersection
    sort(Loc1, Temp),
    Temp = [[2,2],[2,3],[2,4],[3,2],[3,4],[4,2],[4,3],[4,4]],
    in_sight([Board, a,0,0], [3,4], Loc2), %non intersection
    sort(Loc2, Temp2),
    Temp2 = [[2,4],[3,3],[3,5],[4,4]].

% Tests visibility from intersection points using in_sight/3.
test_intersections_sight :-
    testBoard(s4, Board),
    in_sight([Board, a,0,0], [2,2], Loc1), %intersection
    sort(Loc1, Temp),
    Temp = [[1,1],[1,3],[3,1],[3,3]],
    in_sight([Board, a,0,0], [3,2], Loc2), % non intersection
    sort(Loc2, Temp2),
    Temp2 = [[3,1],[3,3]]. % right above and bellow
>>>>>>> 4a42e3a95b328b31bb636d8748ea2e4b87fe07a1

%=======================================================================================================%
% Runs a series of tests for different board scenarios to ensure correctness of valid_moves/2.
% If all tests pass, outputs a success message 'all valid_moves tests passed'.
valid_moves_test :-
  valid_moves1,
  valid_moves3,
  valid_moves2,
  valid_moves4,
  write('all valid_moves tests passed'),nl.

% % Tests valid_moves/2 with a board containing no pieces.
valid_moves1 :- 
  testBoard(5,Testboard),
  valid_moves([Testboard,b,0,0], Positions),
  Positions = [[[0,0],[1,1]],[[0,0],[2,1]],[[0,0],[3,1]],[[0,0],[4,1]],[[0,0],[5,1]],[[0,0],[1,2]],[[0,0],[2,2]],[[0,0],[3,2]],[[0,0],[4,2]],[[0,0],[5,2]],[[0,0],[1,3]],[[0,0],[2,3]],[[0,0],[4,3]],[[0,0],[5,3]],[[0,0],[1,4]],[[0,0],[2,4]],[[0,0],[3,4]],[[0,0],[4,4]],[[0,0],[5,4]],[[0,0],[1,5]],[[0,0],[2,5]],[[0,0],[3,5]],[[0,0],[4,5]],[[0,0],[5,5]]].

% Tests valid_moves/2 and find_biggest_stacks/4 on testBoard(2) (1-a2 2-b1 1-a1 pieces) to validate move generation and stack detection.
valid_moves2 :-
  testBoard(2,Testboard),
  find_biggest_stacks(Testboard, a, Positions, _),
  Positions = [[2,4]],
  find_biggest_stacks(Testboard, b, Positions2, _),
  Positions2 = [[3,2],[4,2]],

  valid_moves([Testboard, b,0,0], Positions3),
  Positions3 = [[[0,0],[1,1]],[[0,0],[2,1]],[[0,0],[3,1]]
,[[0,0],[4,1]],[[0,0],[5,1]],[[0,0],[1,2]]
,[[0,0],[2,2]],[[0,0],[5,2]],[[0,0],[1,3]]
,[[0,0],[2,3]],[[0,0],[4,3]],[[0,0],[5,3]]
,[[0,0],[1,4]],[[0,0],[3,4]],[[0,0],[4,4]]
,[[0,0],[5,4]],[[0,0],[1,5]],[[0,0],[2,5]]
,[[0,0],[3,5]],[[0,0],[4,5]],[[0,0],[5,5]]],
  valid_moves([Testboard, a,0,0], Positions4),
  Positions4 = [[[2,4],[1,3]],[[2,4],[1,4]],[[2,4],[1,5]]
,[[2,4],[2,3]],[[2,4],[2,5]],[[2,4],[3,4]],[[2,4],[3,5]]].

% Verifies specific move generation on testBoard(3) (1-a3, 2-b3, 5-other pieces) for both players 'a' and 'b'.
valid_moves3 :-
  testBoard(3,Testboard),
  valid_moves([Testboard, a,0,0], Positions),
  Positions = [[[2,3],[1,3]],[[2,3],[2,2]]],
  valid_moves([Testboard, b,0,0], Positions2),
  Positions2 = [[[2,1],[1,1]],[[2,1],[2,2]],[[2,1],[3,1]],[[5,5],[4,4]],[[5,5],[4,5]],[[5,5],[5,4]]].

% Tests valid_moves/2 on testBoard(8) (1-a2 not on intersection) to validate move generation.
valid_moves4 :-
  testBoard(8,Testboard),
  valid_moves([Testboard, a, _,_], Positions),
  Positions = [[[3,4],[2,4]],[[3,4],[3,3]],[[3,4],[3,5]],[[3,4],[4,4]]].
