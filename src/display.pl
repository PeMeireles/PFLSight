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
  write('Click enter to continue'),nl,
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
  write('please choose what mode you want to play:'),nl,
  write('1 - player vs player'),nl,
  write('2 - player vs Computer'),nl,
  write('3 - Computer vs Computer'),nl,
  write('4 - Test Font'),nl.

display_menu(computer) :-
  write('Would you like the Computer to be:'),nl,
  write('1 - Random'), nl,
  write('2 - Smart'), nl.

display_new_piece :-
  write('Since you have no stacks you can place a new piece anywhere'), nl.

display_stack_drop :-
  write('Since you have one or more stacks, please select one of them so you can move the piece on top'),nl.

 % --------------------------- 
display_game([Board,Next], Moves) :-
  display_title(Next),nl,
  display_rows(Board),nl,
  display_options(Moves).

display_options(Moves):-
    check_no_stacks(Moves),
    display_new_piece, !.
  
display_options(Moves):-
    display_stack_drop.


display_target_menu :-
  write('Now select where to move it to'),nl,
  write('Note: it has to be an adjacent slot'),nl.


display_title(Next):- 
  clear_screen, 
  write('It\'s '),
  write(Next),
  write('\'s'),
  write(' Turn').


display_winner(Winner).


  %---------------------------------------------------
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

