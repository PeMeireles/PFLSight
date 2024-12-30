:- consult(game).

run :-
  boardaccess,
  write('Board Access done'),nl,
  % display_tests,
  % write('display_tests done'),nl,
  valid_moves3,
  write('valid_moves3 done'),nl,
  
  valid_moves2,
  write('valid_moves2 done'),nl,

  in_sight_test,
  move_tests,
  !.



boardaccess :-
  testBoard(2,Testboard),
  nth1(1, Testboard, Row),
  nth1(1, Row, Elem),
  Elem = '-',
  nth1(2, Testboard, Row2),
  nth1(2, Row2, Elem2),
  Elem2 = 'a2',
  access_board(Testboard, [3,2],Val),
  Val = 'b1',
  access_board(Testboard, [2,4],Val2),
  Val2 = 'a2'.

valid_moves3 :-
  testBoard(3,Testboard),
  valid_moves([Testboard, a], Positions),
  Positions = [[1,3],[2,2]],
  valid_moves([Testboard, b], Positions2),
  Positions2 = [[1,1],[2,2],[3,1],[4,4],[4,5],[5,4]].

valid_moves2 :-
  testBoard(2,Testboard),
  find_biggest_stacks(Testboard, a, Positions, _),
  Positions = [[2,4]],
  find_biggest_stacks(Testboard, b, Positions2, _),
  Positions2 = [[3,2],[4,2]],

  valid_moves([Testboard, b], Positions3),
  Positions3 = [[1,1],[2,1],[3,1],[4,1],[5,1],[1,2],[2,2]
,[5,2],[1,3],[2,3],[4,3],[5,3],[1,4],[3,4]
,[4,4],[5,4],[1,5],[2,5],[3,5],[4,5],[5,5]],
  valid_moves([Testboard, a], Positions4),
  Positions4 = [[1,3],[1,4],[1,5],[2,3],[2,5],[3,4],[3,5]
].

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

move_tests :-
  move_valid_tests,
  write('Move tests passed'),nl.
  %move_invalid_tests,

move_valid_tests :-
  testBoard(2,Testboard),
  move([Testboard,a], [3,4],[NewBoard, NewNext]),
  NewBoard = [[-,-,-,-,-],[-,a3,a1,-,-],[-,-,a2,-,-],[-,-,b1,b1,-],[-,-,-,-,-]],
  NewNext = b,
  display_rows(NewBoard),nl.
  %display_rows(NewBoard).


display_tests :-
  fontTest,
  testBoard(2,Testboard),
  display_rows(Testboard).


fontTest :-
  lettermap(a1,Val),
  write('1 white piece:  '),
  write(Val),nl,
  lettermap(a2,Val2),
  write('2 white pieces: '),
  write(Val2),nl,
  lettermap(a3,Val3),
  write('3 white pieces: '),
  write(Val3),nl,
  lettermap(a4,Val4),
  write('4 white pieces: '),
  write(Val4),nl,
  lettermap(b1,Val5),
  write('1 black piece:  '),
  write(Val5),nl,
  lettermap(b2,Val6),
  write('2 black pieces: '),
  write(Val6),nl,
  lettermap(b3,Val7),
  write('3 black pieces: '),
  write(Val7),nl,
  lettermap(b4,Val8),
  write('4 black pieces: '),
  write(Val8),nl.



testBoard(1, [[-,-,-,-,-], 
              [-,-,-,-,-],
              [-,-,-,-,-],
              [-,-,-,-,-],
              [-,-,-,-,-]]).

testBoard(2, [[-,-,-,-,-], 
              [-,a2,-,-,-],
              [-,-,a1,-,-],
              [-,-,b1,b1,-],
              [-,-,-,-,-]]).

testBoard(3, [[-,a2,-,-,b3], 
              [-,a2,-,-,-],
              [-,a3,a1,-,-],
              [-,-,b1,b1,-],
              [-,b3,-,-,-]]).

testBoard(4, [[a4,-,a2,-,a4], 
              [-,a4,-,a3,-],
              [-,-,-,-,-],
              [a4,a4,-,a4,a4],
              [a4,-,-,-,a4]]).


% Removed numbers since those boards are exclusive to in_sight
testBoard(s1,[[a,-,a,-,a], 
              [-,a,-,a,-],
              [a,-,-,-,a],
              [-,a,-,a,-],
              [a,-,a,-,a]]).

testBoard(s2,[[a,-,a,-,a], 
              [-,a,-,a,-],
              [a,-,a,-,a],
              [-,a,-,a,-],
              [a,-,a,-,a]]).

testBoard(s3,[[a,a,a,a,a], 
              [a,a,a,a,a],
              [a,a,a,a,a],
              [a,a,a,a,a],
              [a,a,a,a,a]]).

testBoard(s4,[[a,-,a,-,a], 
              [a,-,-,-,a],
              [a,-,a,-,a],
              [-,-,-,-,-],
              [a,-,a,-,a]]).

