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

