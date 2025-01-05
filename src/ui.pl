:- consult(display).
:- use_module(library(lists)).

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

% wait_for_enter/0
% Pauses execution until user presses Enter
wait_for_enter :-
  write('Click enter to continue.'),nl,
  get_char(_),
  get_char(_).

% get_valid_choice(-Choice)
% Gets and validates menu choice
get_valid_choice(Choice) :-
    repeat,
    clear_screen,
    display_start_menu,
    read(Choice),
    validate_choice(Choice, [1,2,3,4,5]), !.

% maybe_get_board_size(-Size)
% Optionally gets board size, defaults to 5
maybe_get_board_size(Size) :-
    write('Change board size? (y/n): '),
    read(Response),
    atom(Response),
    handle_size_response(Response, Size).

% handle_size_response(+Response, -Size)
% Handles user response for board size
handle_size_response(y, Size) :- !,
    get_board_size(Size).
handle_size_response(_, 5).  % default size

% get_board_size(-Size)
% Gets and validates board size selection
get_board_size(Size) :-
    repeat,
    display_board_size_menu,
    read(Size),
    validate_board_size(Size), !.

% get_ai_type(-Type)
% Gets and validates AI type selection
get_ai_type(Type) :-
    repeat,
    display_menu(computer),
    read(Type),
    validate_choice(Type, [1,2,3]), !.

% read_player_input(-Position, +Moves)
% Reads and processes player input
% Position: Resulting board position
% Moves: List of valid moves
read_player_input(Pos, Moves) :-
    read(Input),
    atom(Input),
    handle_input(Input, Pos, Moves).

% handle_input(+Input, -Position, +Moves)
% Processes different types of input (commands or positions)
handle_input(v,Pos, Moves) :-
    display_moves(Moves),
    read_player_input(Pos, Moves), !.

handle_input(x, _, _) :-
    halt.

handle_input(Input,Pos, _) :-
    valid_chess_coord(Input),
    coords_to_pos(Input, Pos).

% coords_to_pos(+Atom, -Tuple)
% Converts chess notation to tuple coordinates
coords_to_pos(ChessCoord, [X,Y]) :-
    atom(ChessCoord),
    atom_chars(ChessCoord, [Letter|NumberChars]),
    char_code(Letter, Code),
    X is Code - 96,
    number_chars(Y, NumberChars).

% move_type(+Moves, -Type)
% Determines move type based on valid moves
% Type: new_piece if placing new piece, stack if moving existing piece
move_type(Moves, new_piece) :-
    check_no_stacks(Moves),!.

move_type(_, stack).

% handle_invalid_move/0
% Handles invalid move input
handle_invalid_move :-
    write('Invalid move. Try again.'), nl,
    wait_for_enter,
    clear_screen.

% valid_chess_letter(+Letter)
% Checks if the given letter is a valid chess coordinate letter
valid_chess_letter(Letter) :-
    char_code(Letter, Code),
    between(97,122,Code).

% valid_chess_coord(+ChessCoord)
% Validates if the input is a valid chess coordinate
valid_chess_coord(ChessCoord) :-
    atom_length(ChessCoord, Length),
    Length =:= 2,
    atom_chars(ChessCoord, [Letter|NumberChars]),
    valid_chess_letter(Letter),
    NumberChars \= [].

% validate_choice(+Choice, +Choices)
% Validates if given choice is member of valid choices list
% Fails with error message if choice is invalid
validate_choice(Choice, Choices) :-
  member(Choice, Choices).

validate_choice(_,_) :-
  write('Invalid choice.'), nl,
  wait_for_enter,
  false.

% validate_board_size(+Size)
% Validates if size is odd and within bounds
validate_board_size(Size) :-
    number(Size),
    Size mod 2 =:= 1,
    between(5, 9, Size).
