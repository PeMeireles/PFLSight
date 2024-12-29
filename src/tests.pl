:- consult(game).
:- use_module(library(lists)).

run :-
  boardaccess,
  write('Board Access done'),nl,
  display_tests,
  write('display_tests done'),nl.
  
  validate_move1,
  validate_move2,
  
  !.

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


boardaccess :-
  testBoard(2,Testboard),
  nth1(1, Testboard, Row),
  nth1(1, Row, Elem),
  Elem = '-',
  nth1(2, Testboard, Row2),
  nth1(2, Row2, Elem2),
  Elem2 = 'a2',
  access_board(Testboard, [3,2],Val),
  Val = 'b1'.
  access_board(Testboard, [2,2],Val),
  Val = 'a2'.

  

validate_move1:-
  testBoard(1,TestBoard),
  validate_move([TestBoard, 1],[5,1]),
  write('validatemove1 done'),
  

validate_move2:-
  testBoard(2,TestBoard),
  validate_move([TestBoard, 1],[5,1]),
  write('validatemove2.1 done'),
  \+ validate_move([TestBoard, 1],[5,2]),
  write('validatemove2.2 done').





testBoard(1, [[-,-,-,-,-], 
              [-,-,-,-,-],
              [-,-,-,-,-],
              [-,-,-,-,-],
              [-,-,-,-,-]]).

testBoard(2, [[-,-,-,-,-], 
              [-,a2,-,-,-],
              [-,-,-,-,-],
              [-,-,b1,b1,-],
              [-,-,-,-,-]]).
