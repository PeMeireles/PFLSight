% board(+Size, -Board)
% Generates empty board of Size x Size dimensions
% Size must be odd number (7, 9, 11, etc.)
board(Num, Board) :-
  Num mod 2 =:= 1,
  genRow(Num, Row, []),
  genBoard(Num, Board, Row, []).

genRow(0, Row, Row).
genRow(Num, Row, Acc) :-
  Num > 0,
  N1 is Num -1,
  genRow(N1, Row, ['-'|Acc]).

genBoard(0, Board, Row, Board).
genBoard(Num, Board, Row, Acc) :-
  Num > 0,
  N1 is Num -1,
  genBoard(N1, Board, Row, [Row|Acc]).


lettermap(a1,'K').
lettermap(a2,'V').
lettermap(a3,'B').
lettermap(a4,'P').
lettermap(b1,'Z').
lettermap(b2,'Q').
lettermap(b3,'J').
lettermap(b4,'X').

% wait_for_enter/0
% Pauses execution until user presses Enter
wait_for_enter :-
  write('Click enter to continue.'),nl,
  get_char(_),
  get_char(_).

% clear_screen/0
% Clears the terminal screen using ANSI escape sequences
% \e[H moves cursor to top-left corner
% \e[2J erases entire screen
clear_screen :-
    write('\e[H\e[2J').

% -----------------------------------------------------

% validate_choice(+Choice, +Choices)
% Validates if given choice is member of valid choices list
% Choice: User input to validate
% Choices: List of valid options
% Fails with error message if choice is invalid
% TODO ver se precisa cut
validate_choice(Choice, Choices) :-
    member(Choice, Choices).
validate_choice(_,_) :-
    write('Invalid choice. Please select 1-4.'), nl,
    wait_for_enter,
    false.

validate_board_choice([X,Y], _) :-
    board_position(X,Y).
validate_board_choice(_,_) :-
    write('Invalid board position.'), nl,
    false.

display_start_menu :-
  write('What mode you want to play?'),nl,
  write('1 - player vs player'),nl,
  write('2 - player vs computer'),nl,
  write('3 - computer vs computer'),nl,
  write('4 - test font'),nl.

display_menu(computer) :-
  write('The computer should be:'),nl,
  write('1 - random'), nl,
  write('2 - smart'), nl.

display_new_piece :-
  write('Since you have no stacks, you can place a new piece anywhere.'), nl,
  write('Input e.g.: (a1).'), nl.

display_stack_drop :-
  write('Since you have one or more stacks, please select one of them so you can move the piece on top.'),nl,
  write('Input e.g.: (a1).'),nl,
  write('Hint: You can type \'v\' to see what moves you can do.'), nl.

 % --------------------------- 
display_game([Board,Next, _,_]) :-
  display_title(Next),nl,
  display_rows(Board),nl.



display_options(Moves):-
    display_stack_drop.


display_target_menu :-
  write('Now select where to move it to.'),nl,
  write('Note: it has to be an adjacent slot.'),nl,
  write('Input e.g.: (a1).'), nl.



display_title(Next):- 
  clear_screen, 
  write('Type \'x\' to close at any time.'), nl,
  write(' '),nl,
  write('It\'s '),
  write(Next),
  write('\'s'),
  write(' turn!').

display_moves(Moves) :-
  write('Here are your moves:'),nl,
  display_movesAux(Moves).

display_movesAux([]).
display_movesAux([[[0,0], [_,_]] | _]) :-
    write('Put a piece wherever you like.'),nl.

display_movesAux([[[OX,OY], [X,Y]] | Rest]) :-
    codeletter(OX, Letter1),
    codeletter(X, Letter2),
    write('From '), write(Letter1), write(OY), write(' to '), write(Letter2), write(Y), nl,
    display_movesAux(Rest).

display_winner(Winner) :-
    write(Winner),
    write('wins!'),nl.

  %---------------------------------------------------
display_rows(Board) :-
  write('  '),
  display_column_letters_aux(5), nl,
  display_rowsAux(Board, 5, 0).

display_column_letters_aux(0).
display_column_letters_aux(N) :-
    N1 is N - 1,
    display_column_letters_aux(N1),
    codeletter(N, Char),
write(Char), write('   ').


display_rowsAux([Row | _], 1, _) :-
    write('1 '), display_row(Row), !.

display_rowsAux([Row | Rest], N, 0):-
    N > 0,
    write(N), write(' '), display_row(Row), nl,
    write('| | \\ | / | \\ | / |'), nl,
    N1 is N - 1,
    display_rowsAux(Rest, N1, 1).

display_rowsAux([Row | Rest], N, 1):-
    N > 0,
    write(N), write(' '), display_row(Row), nl,
    write('| | / | \\ | / | \\ |'), nl,
    N1 is N - 1,
    display_rowsAux(Rest, N1, 0).

display_row([]).
display_row([-| Rest]):-
  write(' '),
  (Rest \= [] -> write('---') ; true ),
  display_row(Rest).

display_row([Char | Rest]):-
  lettermap(Char,Val),
  write(Val),
  (Rest \= [] -> write('---') ; true ),
  display_row(Rest).

% lettermap(+Val, -Char)
codeletter(Val, Char) :-
    Code is Val + 96,
    char_code(Char, Code).



