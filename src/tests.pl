:- consult(game).
:- use_module(library(lists)).

run :-
  boardaccess,
  write('Board Access done'),nl,
  % display_tests,
  % write('display_tests done'),nl,

  valid_moves2,
  write('valid_moves2 done'),nl,
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

  

valid_moves2 :-
  testBoard(2,Testboard),
  find_biggest_stacks(Testboard, a, Positions, MaxNum),
  Positions = [[2,4]],
  find_biggest_stacks(Testboard, b, Positions2, MaxNum2),
  Positions2 = [[3,2],[4,2]],

  valid_moves([Testboard, b], Positions3),
  Positions3 = [[1,1],[2,1],[3,1],[4,1],[5,1],[1,2],[2,2]
,[5,2],[1,3],[2,3],[4,3],[5,3],[1,4],[3,4]
,[4,4],[5,4],[1,5],[2,5],[3,5],[4,5],[5,5]],

  valid_moves([Testboard, a], Positions4),
  Positions4 = [[1,3],[1,5],[3,5]].
  

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


