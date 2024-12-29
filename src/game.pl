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

board(5,[
        [-,-,-,-,-],
        [-,-,-,-,-],
        [-,-,-,-,-],
        [-,-,-,-,-],
        [-,-,-,-,-]
        ]).

display_game([Board,Next]) :-
  display_title(Next),
  display_rows(Board),
  display_options(Board).

display_options(Board).

display_title(Next):- 
  write('Its '),
  write(Next),
  write('Turn').

display_rows(Board) :-
  display_rowsAux(Board, 5).


display_rowsAux([Row | Rest], 1) :-
    display_row(Row), !.

display_rowsAux([Row | Rest], N):-
    N > 0,
    display_row(Row), nl,
    write('| \\ | / | \\ | / |'), nl,
    N1 is N - 1,
    display_rowsAux(Rest, N1).

display_row([]).
display_row([-| Rest]):-
  write(' '),
  (Rest \= [] -> write('---') ; true ),
  display_row(Rest).

display_row([Char | Rest]):-
  write(Char),
  (Rest \= [] -> write('---') ; true ),
  display_row(Rest).



  

