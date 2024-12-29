%% z -> / a trema 0666D -> ٭ Ú   
%% /uniE001 -> \ 
%% Z -> 1 preta -> z
%% Q -> 2 pretas -> q
%% J -> 3 pretas -> j
%% X -> 4 pretas -> x
%% K -> 1 branca -> k
%% V -> 2 brancas -> v
%% B -> 3 brancas -> b
%% P -> 4 brancas -> p

:- consult(display).

play :- 
  board(5,Board),
  display_rows(Board),nl,
  display_menu,
  read(Choice),
  handle_choice(Choice).

display_menu :-
  write('please choose what mode you want to play:'),
  write('1 - player vs player'),nl,
  write('2 - player vs Computer'),nl,
  write('3 - Computer vs Computer'),nl.


handle_choice(1).

handle_choice(2) :-
  write('Would you like the Computer to be:'),nl,
  write('1 - Random'), nl,
  write('2 - Smart'), nl,
  read(Choice).

handle_choice(3) :-
  write('Would you like the first Computer to be:'),nl,
  write('1 - Random'), nl,
  write('2 - Smart'), nl,
  read(Choice1),
  write('Would you like the second Computer to be:'),nl,
  write('1 - Random'), nl,
  write('2 - Smart'), nl,
  read(Choice2).



validate_move([Board, Next], [X,Y]) :-



  

