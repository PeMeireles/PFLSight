:- consult(display).
:- use_module(library(lists)).

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
    validate_choice(Type, [1,2]), !.

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

% test_piece_display/0
% Tests piece display functionality
test_piece_display :-
    write('If this character is a 4 stack white piece, it\'s working: P'), nl,
    wait_for_enter.
