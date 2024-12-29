board(5,[
        [-,-,-,-,-],
        [-,-,-,-,-],
        [-,-,-,-,-],
        [-,-,-,-,-],
        [-,-,-,-,-]
        ]).

lettermap(a1,'K').
lettermap(a2,'V').
lettermap(a3,'B').
lettermap(a4,'P').
lettermap(b1,'Z').
lettermap(b2,'Q').
lettermap(b3,'J').
lettermap(b4,'X').

display_game([Board,Next]) :-
  display_title(Next),
  display_rows(Board),nl,
  display_options(Board).

display_options(Board).

display_title(Next):- 
  write('Its '),
  write(Next),
  write('Turn').

display_rows(Board) :-
  display_rowsAux(Board, 5, 0).


display_rowsAux([Row | _], 1, _) :-
    display_row(Row), !.

display_rowsAux([Row | Rest], N, 0):-
    N > 0,
    display_row(Row), nl,
    write('| \\ | / | \\ | / |'), nl,
    N1 is N - 1,
    display_rowsAux(Rest, N1, 1).

display_rowsAux([Row | Rest], N, 1):-
    N > 0,
    display_row(Row), nl,
    write('| / | \\ | / | \\ |'), nl,
    N1 is N - 1,
    display_rowsAux(Rest, N1, 0).

display_row([]).
display_row([-| Rest]):-
  write(' '),
  (Rest \= [] -> write('---') ; true ),
  display_row(Rest).

display_row([Char | Rest]):-
  lettermap(Char, Val),
  write(Val),
  (Rest \= [] -> write('---') ; true ),
  display_row(Rest).

