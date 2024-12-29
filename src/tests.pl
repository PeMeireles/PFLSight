:- consult(game).
:- use_module(library(lists)).

run :-
  boardaccess.
   
  validate_move1,
  validate_move2,
  
  !.


boardaccess :-
  testBoard(2,Testboard),
  nth1(1, Testboard, Row),
  nth1(1, Row, Elem),
  write(Elem),
  Elem = '-',
  nth1(2, Testboard, Row2),
  nth1(2, Row2, Elem2),
  write(Elem2),
  Elem2 = 'a2'.

  

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
