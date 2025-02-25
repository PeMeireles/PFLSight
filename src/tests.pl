:- consult(game).
:- consult(movetests).
:- consult(valuetests).
:- consult(choose_movetests).

% Main predicate that runs all tests in sequence. 
run :-
  boardaccess,
  write('Board Access done'),nl,
  display_tests,
  write('display_tests done'),nl,
  valid_moves_test,

  in_sight_test,
  move_tests,
  value_tests,
  game_over_test1,
  game_over_test2,
  game_over_test3,
  game_over_test4,
  write('game_over_test done'),nl,

  run_choose_move_tests,
  
  !.


% Verifies that elements in a predefined test board can be accessed correctly using access_board/3.
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


% Tests game_over/2 with no winner scenario using testBoard(g0).
game_over_test1 :-
  testBoard(g0, TestBoard),
  game_over([TestBoard, a,0,0], 0).

% Tests game_over/2 with player 'b' winning using testBoard(g1).
game_over_test2 :-
  testBoard(g1, TestBoard),
  game_over([TestBoard, a,0,0], b).

% Tests game_over/2 with no winner scenario using testBoard(g2).
game_over_test3 :-
  testBoard(g2, TestBoard),
  game_over([TestBoard, b,0,0], 0).

% Tests game_over/2 with no winner scenario using testBoard(g3).
game_over_test4 :-
  testBoard(g3, TestBoard),
  game_over([TestBoard, a,0,0], 0).

% Runs tests to display and print the board representation of the current game state.
display_tests :-
  fontTest,
  testBoard(2,Testboard),
  display_rows(Testboard),nl,
  board(5,Testboard2),
  display_rows(Testboard2),nl.

% Tests the mapping of letters to specific pieces on the board (e.g., a1, a2, b1, b2).
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


% Testboards used for testing most of them are unreachable
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

testBoard(5, [[-,-,-,-,-], 
              [-,-,-,-,-],
              [-,-,a1,-,-],
              [-,-,-,-,-],
              [-,-,-,-,-]]).

testBoard(6,[[a3,-,-,-,-], 
              [-,-,a2,-,-],
              [a3,-,-,-,b3],
              [-,-,b2,-,-],
              [-,-,-,-,b2]]).

testBoard(7, [[a1,-,-,-,-], 
              [-,-,-,-,-],
              [-,-,-,-,-],
              [-,-,-,b4,b4],
              [a1,-,-,-,b4]]).

testBoard(8, [[a1,-,-,-,-], 
              [-,-,a2,-,-],
              [-,-,-,-,-],
              [-,-,-,b4,b4],
              [a1,-,-,-,b4]]).

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

% Impossible state but its only used for gameover
testBoard(g0,[[a2,b1,-,-,-],
              [b1,-,-,-,-],
              [-,-,-,-,-],
              [-,-,-,-,-],
              [-,-,-,-,-]]).

testBoard(g1,[[a2,b1,-,-,-],
              [b1,b1,-,-,-],
              [-,-,-,-,-],
              [-,-,-,-,-],
              [-,-,-,-,-]]).

testBoard(g2,[[a1,-,-,-,-],
              [-,-,-,-,-],
              [-,-,-,-,-],
              [-,-,-,-,-],
              [-,-,-,-,-]]).

testBoard(g3,[[b1,-,-,-,-],
              [-,-,-,-,-],
              [-,-,-,-,-],
              [-,-,-,-,-],
              [-,-,-,-,-]]).

testBoard(si1,[[b1,a1,-,-,-],
              [-,-,b1,-,-],
              [-,-,a1,-,-],
              [-,-,b1,-,-],
              [-,-,-,-,-]]).



