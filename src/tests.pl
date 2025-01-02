:- consult(game).
:- consult(movetests).
:- consult(valuetests).
run :-
  boardaccess,
  write('Board Access done'),nl,
  display_tests,
  write('display_tests done'),nl,
  valid_moves3,
  write('valid_moves3 done'),nl,
  
  valid_moves2,
  write('valid_moves2 done'),nl,

  in_sight_test,
  move_tests,
  value_tests,
  game_over_test1,
  game_over_test2,
  write('game_over_test done'),nl,
  
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
  Positions = [[[2,3],[1,3]],[[2,3],[2,2]]],
  valid_moves([Testboard, b], Positions2),
  Positions2 = [[[2,1],[1,1]],[[2,1],[2,2]],[[2,1],[3,1]],[[5,5],[4,4]],[[5,5],[4,5]],[[5,5],[5,4]]].

valid_moves2 :-
  testBoard(2,Testboard),
  find_biggest_stacks(Testboard, a, Positions, _),
  Positions = [[2,4]],
  find_biggest_stacks(Testboard, b, Positions2, _),
  Positions2 = [[3,2],[4,2]],

  valid_moves([Testboard, b], Positions3),
  Positions3 = [[[0,0],[1,1]],[[0,0],[2,1]],[[0,0],[3,1]]
,[[0,0],[4,1]],[[0,0],[5,1]],[[0,0],[1,2]]
,[[0,0],[2,2]],[[0,0],[5,2]],[[0,0],[1,3]]
,[[0,0],[2,3]],[[0,0],[4,3]],[[0,0],[5,3]]
,[[0,0],[1,4]],[[0,0],[3,4]],[[0,0],[4,4]]
,[[0,0],[5,4]],[[0,0],[1,5]],[[0,0],[2,5]]
,[[0,0],[3,5]],[[0,0],[4,5]],[[0,0],[5,5]]],
  valid_moves([Testboard, a], Positions4),
  Positions4 = [[[2,4],[1,3]],[[2,4],[1,4]],[[2,4],[1,5]]
,[[2,4],[2,3]],[[2,4],[2,5]],[[2,4],[3,4]],[[2,4],[3,5]]].

% Check no winner
game_over_test1 :-
    testBoard(g0, TestBoard),
    \+ game_over([TestBoard, a], Winner).

game_over_test2 :-
    testBoard(g1, TestBoard),
    game_over([TestBoard, a], Winner),
    Winner = b.
    

display_tests :-
  fontTest,
  testBoard(2,Testboard),
  display_rows(Testboard),nl,
  board(5,Testboard2),
  display_rows(Testboard2),nl.


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

% Impossibel state but its only used for gameover
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

