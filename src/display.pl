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

genBoard(0, Board, _, Board).

genBoard(Num, Board, Row, Acc) :-
  Num > 0,
  N1 is Num -1,
  genBoard(N1, Board, Row, [Row|Acc]).

playerName(a,'player 1').
playerName(b,'player 2').

lettermap(a1,'K').
lettermap(a2,'V').
lettermap(a3,'B').
lettermap(a4,'P').
lettermap(b1,'Z').
lettermap(b2,'Q').
lettermap(b3,'J').
lettermap(b4,'X').

% Extreme cases, could happen but I never reached them.
lettermap(a5,'PK').
lettermap(a6,'PV').
lettermap(b5,'XZ').
lettermap(b6,'XQ').

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
% Fails with error message if choice is invalid
validate_choice(Choice, Choices) :-
  member(Choice, Choices).

validate_choice(_,_) :-
  write('Invalid choice. Please select 1-4.'), nl,
  wait_for_enter,
  false.

% Options text
display_start_menu :-
  write('What mode you want to play?'),nl,
  write('1 - player vs player'),nl,
  write('2 - player vs computer'),nl,
  write('3 - computer vs player'),nl,
  write('4 - computer vs computer'),nl,
  write('5 - test font'),nl.

display_menu(computer) :-
  write('The computer should be:'),nl,
  write('1 - random'), nl,
  write('2 - smart'), nl.

% display_board_size_menu/0
% Displays board size selection menu
display_board_size_menu :-
  write('Select board size (odd number between 5 and 9):'), nl,
  write('5  - 5x5 board'), nl,
  write('7  - 7x7 board'), nl,
  write('9  - 9x9 board'), nl.

display_new_piece :-
  write('Since you have no stacks, you can place a new piece anywhere.'), nl,
  write('Input e.g.: (a1).'), nl.

display_stack_drop :-
  write('Since you have one or more stacks, please select one of them so you can move the piece on top.'),nl,
  write('Input e.g.: (a1).'),nl,
  write('Hint: You can type \'v\' to see what moves you can do.'), nl.

 % --------------------------- 
% display_game/0
% Turn interface
display_game([Board,Next, _,_]) :-
  display_title(Next),nl,
  display_rows(Board),nl.


% display_target_menu/0
% Second menu after stack move
display_target_menu :-
  write('Now select where to move it to.'),nl,
  write('Note: it has to be an adjacent slot.'),nl,
  write('Input e.g.: (a1).'), nl.

% display_title(+Next)
% Show's who turn is it
display_title(Next):- 
  playerName(Next,PlayerName),
  clear_screen, 
  write('Type \'x\' to close at any time.'), nl,
  write(' '),nl,
  write('It\'s '),
  write(PlayerName),
  write('\'s'),
  write(' turn!').

% display_moves(+Moves)
% Show the valid moves
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

% display_winner(+Winner)
% End screen for the Winner
display_winner(Winner) :-
  playerName(Winner,WinnerName),
  nl, nl,
  write('   '),
  write(WinnerName),
  write(' wins!'),nl.

%---------------------------------------------------
% display_rows(+Board)
% Recursively display the Board with side indications for move coordinates
display_rows(Board) :-
  write('  '),
  length(Board,Size),
  display_column_letters_aux(Size), nl,
  display_rowsAux(Board, Size, 0, Size).

display_column_letters_aux(0).
display_column_letters_aux(N) :-
  N1 is N - 1,
  display_column_letters_aux(N1),
  codeletter(N, Char),
  write(Char), write('   ').

display_rowsAux([Row | _], 1, _, _) :-
  write('1 '), 
  display_row(Row), !.

display_rowsAux([Row | Rest], N, 0, MaxSize):-
  N > 0,
  write(N), write(' '), 
  display_row(Row), nl,
  display_lines_row(MaxSize,0), nl,
  N1 is N - 1,
  display_rowsAux(Rest, N1, 1, MaxSize).

display_rowsAux([Row | Rest], N, 1, MaxSize):-
  N > 0,
  write(N), write(' '), 
  display_row(Row), nl,
  display_lines_row(MaxSize,1), nl,
  N1 is N - 1,
  display_rowsAux(Rest, N1, 0, MaxSize).

display_row([]).

display_row([-| []]) :-
  write(' ').

display_row([-| Rest]) :-
  Rest \= [],
  write(' '),
  write('---'),
  display_row(Rest).

display_row([Char | []]) :-
  lettermap(Char, Val),
  write(Val).

display_row([Char | Rest]) :-
  Rest \= [],
  lettermap(Char, Val),
  write(Val),
  write('---'),
  display_row(Rest).

display_lines_row(Size, StartPattern) :-
  write('|'),
  SizeNo is Size -1,
  display_line_segments(SizeNo, StartPattern),
  write(' |').

display_line_segments(0, _).

display_line_segments(N, Pattern) :-
  N > 0,
  write(' | '),
  display_separator(Pattern),
  NextPattern is 1 - Pattern,
  N1 is N - 1,
  display_line_segments(N1, NextPattern).

% display_separator(+Pattern)
% Displays separator based on current pattern (1 for /, 0 for \)
display_separator(1) :-
  write('/').

display_separator(0) :-
  write('\\').

% lettermap(+Val, -Char)
% Maps the Y cord to a letter
codeletter(Val, Char) :-
  Code is Val + 96,
  char_code(Char, Code).



