move_tests :-
  move_valid_tests2,
  move_valid_tests3,
  write('Move tests passed'),nl.
  %move_invalid_tests,

% General Tests
move_valid_tests2 :-
  testBoard(2,Testboard),
  % General test from stack
  move([Testboard,a], [[2,4],[3,4]],[NewBoard, NewNext]),
NewBoard = [[-,-,-,-,-],[-,a1,a1,-,-],[-,-,a2,-,-],[-,-,b1,b1,-],[-,-,-,-,-]],
  NewNext = b,
  % General test no stacks
  move([Testboard,b], [[0,0],[4,4]], [NewBoard2, NewNext2]),
  NewNext2 = a,
  NewBoard2 = [[-,-,-,-,-],[-,a2,-,b1,-],[-,-,a1,-,-],[-,-,b1,b2,-],[-,-,-,-,-]].

move_valid_tests3 :-
  % Move with 1 diagonal sitght
  testBoard(3, Testboard),
  move([Testboard,a], [[2,3],[2,2]],[NewBoard, _]),
  NewBoard = [[-,a2,-,-,b3], 
              [-,a2,-,-,-],
              [-,a2,a2,-,-],
              [-,a1,b1,b1,-],
              [-,b3,-,-,-]],
  % Test if its not skipping the starting position
  move([Testboard, a], [[2,3],[1,3]], [NewBoard2, _]),
  NewBoard2 =[[-,a2,-,-,b3], 
              [-,a3,-,-,-],
              [a1,a2,a1,-,-],
              [-,-,b1,b1,-],
              [-,b3,-,-,-]].




% -----------------------------------------------------------
in_sight_col3 :-
  testBoard(3,Testboard),
  in_sight_col([Testboard, a], [2,2],Locations),
  Locations = [[2,3]],
  in_sight_col([Testboard, b], [4,4],Locations2),
  Locations2 = [[4,2]].

in_sight_row3 :-
  testBoard(3,Testboard),
  in_sight_row([Testboard, a], [4,4],Locations),
  Locations = [[2,4]],
  in_sight_row([Testboard, a], [5,3],Locations2),
  Locations2 = [[3,3]].

in_sight_diagonal3 :-
  testBoard(3,Testboard),
  in_sight_diagonal([Testboard, a], [4,4], Locations, 0),
  Locations = [[3,3]],
  in_sight_diagonal([Testboard, a], [3,3], Locations2, 0),
  Locations2 = [[2,4]].

in_sight4 :-
  testBoard(4,Testboard),
  in_sight([Testboard, a], [3,3], Locations),
  Locations = [[3,5],[4,4],[4,2],[2,4],[2,2]],
  in_sight([Testboard, a], [3,2], Locations2),
  Locations2 = [[2,2],[4,2],[3,5]].

in_sight_test :-
  in_sight_diagonal3,
  in_sight_row3,
  in_sight_col3,
  in_sight4,
  test_edges_sight,
  test_corners_sight,
  test_full_box_sight,
  test_intersections_sight,
  write('All Sight tests passed!'), nl.


test_edges_sight :-
    testBoard(s1, Board),
    in_sight([Board, a], [3,1], Loc1),
    Loc1 = [[1,1],[5,1],[3,5],[4,2],[2,2]],
    in_sight([Board, a], [1,3], Loc2),
    Loc2 = [[5,3],[1,1],[1,5],[2,4],[2,2]].

test_corners_sight :-
    testBoard(s2, Board),
    in_sight([Board, a], [1,1], Loc1),
    Loc1 = [[3,1],[1,3],[2,2]],
    in_sight([Board, a], [5,5], Loc2),
    Loc2 = [[3,5],[5,3],[4,4]],
    in_sight([Board, a], [5,2], Loc3),
    Loc3 = [[4,2],[5,1],[5,3]].

test_full_box_sight :-
    testBoard(s3, Board),
    in_sight([Board, a], [3,3], Loc1),
    Loc1 = [[2,3],[4,3],[3,2],[3,4],[4,4],[4,2],[2,4],[2,2]],
    in_sight([Board, a], [3,4], Loc2),
    Loc2 = [[2,4],[4,4],[3,3],[3,5]].

test_intersections_sight :-
    testBoard(s4, Board),
    in_sight([Board, a], [2,2], Loc1),
    Loc1 = [[3,3],[3,1],[1,3],[1,1]],
    in_sight([Board, a], [3,2], Loc2),
    Loc2 = [[3,1],[3,3]].

