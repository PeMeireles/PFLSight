:- consult(display).
:- use_module(library(between)).
:- use_module(library(lists)).
:- use_module(library(random)).

% play/0
% Main entry point for the game
play :- 
    get_valid_choice(Choice),
    setup_game_options(Choice, Options),
    start_game(Options).


% get_valid_choice(-Choice)
% Gets and validates menu choice
get_valid_choice(Choice) :-
    repeat,
    clear_screen,
    display_start_menu,
    read(Choice),
    validate_choice(Choice, [1,2,3,4,5]), !.

% setup_game_options(+Choice, -Options)
% Sets up game options based on menu choice
setup_game_options(1, (Size, 0,0)) :-
    maybe_get_board_size(Size).

setup_game_options(2, (Size, 0, AIType)) :-
    maybe_get_board_size(Size),
    get_ai_type(AIType).

setup_game_options(3, (Size, AIType, 0)) :-
    maybe_get_board_size(Size),
    get_ai_type(AIType).

setup_game_options(4, (Size, AI1, AI2)) :-
    maybe_get_board_size(Size),
    get_ai_type(AI1),
    write('And for the second one.'), nl,
    get_ai_type(AI2).

setup_game_options(5, _) :-
    test_piece_display,
    fail.

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

% validate_board_size(+Size)
% Validates if size is odd and within bounds
validate_board_size(Size) :-
    Size mod 2 =:= 1,
    between(5, 9, Size).

% get_ai_type(-Type)
% Gets and validates AI type selection
get_ai_type(Type) :-
    repeat,
    display_menu(computer),
    read(Type),
    validate_choice(Type, [1,2]), !.

% test_piece_display/0
% Tests piece display functionality
test_piece_display :-
    write('If this character is a 4 stack white piece, it\'s working: P'), nl,
    wait_for_enter.

% start_game(+Options)
% Initializes and starts game with given configuration
% Options: (Size, P1Type, P2Type) where:
%   Size: Board size
%   P1Type/P2Type: Player types (0-human, 1-random AI, 2-smart AI)
start_game(Options) :-
    initial_state(Options, GameState),
    game_loop(GameState).

% game_loop(+GameState)
% Main game loop orchestrating game flow
% GameState: [Board, Next, P1, P2], current board state, next player and AITypes
game_loop(Gamestate) :-
    repeat,
        valid_moves(Gamestate, Moves),
        display_game(Gamestate),
        (handle_move(Gamestate, Moves, NewState) ->
            handle_game_state(NewState)
        ;   
            handle_invalid_move,
            game_loop(Gamestate)
        ).

% handle_game_state(+GameState)
% Processes game state after move
% Checks for winner or continues game
handle_game_state([Board, Next, P1, P2]) :-
    game_over([Board, Next, _, _], Winner),
    (Winner \= 0 ->
        display_rows(Board),

        display_winner(Winner), !
    ;   
        game_loop([Board, Next, P1, P2])
    ).

% handle_invalid_move/0
% Handles invalid move input
handle_invalid_move :-
    write('Invalid move. Try again.'), nl,
    wait_for_enter,
    clear_screen.


% handle_move(+GameState, +ValidMoves, -NewState)
% Handles move input based on game state and AIType
handle_move([Board, a, 0, P2], Moves, NewState) :-
    move_type(Moves, Type),
    execute_move(Type, [Board, a, 0, P2], Moves, NewState).

handle_move([Board, b, P1, 0], Moves, NewState) :-
    move_type(Moves, Type),
    execute_move(Type, [Board, b, P1, 0], Moves, NewState).
    
handle_move([Board, a, P1, P2], Moves, NewState) :-
    choose_move([Board, a, P1, P2], P1 , Move),
    move([Board, a, P1, P2], Move,NewState),
    wait_for_enter.


handle_move([Board, b, P1, P2], Moves, NewState) :-
    choose_move([Board, b, P1, P2], P2 , Move),
    move([Board, b, P1, P2], Move,NewState),
    wait_for_enter.

    

% handle_new_piece(+GameState, +ValidMoves, -NewState)
% Handles moves when no stacks present
execute_move(new_piece, Gamestate, Moves, NewState) :-
    display_new_piece,
    read_player_input(Pos1, Moves),
    move_pre_val(Gamestate, [[0,0], Pos1], NewState, Moves).


% handle_regular_move(+GameState, +ValidMoves, -NewState)
% Handles moves when stacks are present
execute_move(stack, Gamestate, Moves, NewState) :-
    display_stack_drop,
    read_player_input(Pos1, Moves),
    validate_valid_stack(Pos1, Moves),
    display_target_menu,
    read_player_input(Pos2, Moves),
    move_pre_val(Gamestate, [Pos1, Pos2], NewState, Moves).
 
% validate_valid_stack(+Origin, +ValidMoves)
% Validates if Origin position is a valid stack to move from
% Origin: [Row,Col] position to validate
% ValidMoves: List of [[From,To]] valid moves
% Fails with message if Origin is not a valid stack position
validate_valid_stack(_, []) :-
  write('Select a valid stack'),nl,
  false.

validate_valid_stack(Origin, [[Origin, _] | _ ]) :-!.
validate_valid_stack(Origin, [_ | T]) :-
    validate_valid_stack(Origin, T).

% initial_state(+Config, -GameState)
% Creates initial game state from configuration
% Config: (Size, P1, P2) where:
%   Size: Board size
%   P1: Player 1 type (0-human, 1-random AI, 2-smart AI)
%   P2: Player 2 type (0-human, 1-random AI, 2-smart AI)
% GameState: [Board, CurrentPlayer, P1Type, P2Type]
initial_state((Size, P1, P2), [Board, Next, P1, P2]) :-
  board(Size,Board),
  Next = a. 

% access_board(+Board, +Position, -Value)
% Accesses value at given position on game board
% Board: List of lists representing game state
% Position: [X,Y] where:
%   - X is column (1-5)
%   - Y is row (1-5) from bottom to top
% Value: Content at position ('-' for empty, or 'PlayerNumber' Ex: 'a1')
%
% Note: Internal board representation is top-to-bottom (Y1 = 6-Y),
% while game logic uses bottom-to-top coordinates
access_board(Board,[X,Y], Val) :-
  length(Board, Size),
  Y1 is Size +1 - Y,
  board_position(Board,X,Y),
  nth1(Y1, Board, Row),
  nth1(X,Row,Val).

% check_board_position(+Size, -Row, -Col)
% Generates valid board coordinates
% Succeeds for all positions within 5x5 board
check_board_position(Size, Row, Col) :-
    between(1, Size, Row),
    between(1, Size, Col).

% board_position(+Board, -Row, -Col)
% Generates valid positions for given board
% Board: Current game board
% Row: Valid row coordinate
% Col: Valid column coordinate
board_position(Board, Row, Col):-
    length(Board, Size),
    check_board_position(Size, Row, Col).

% valid_moves(+GameState, -ValidMoves)
% Determines all valid moves for current player
% GameState: [Board, Next, P1, P2], current board state, next player and AITypes
% ValidMoves: List of [[StartRow,StartCol], [TargetRow,TargetCol]] valid moves
valid_moves([Board, Next, _, _], Positions) :-
  % Check for the biggest stack
  find_biggest_stacks(Board, Next, StackPositions, Max),
  valid_movesAux(Board, Max, StackPositions, Positions).

% valid_movesAux(+Board, +MaxValue, +StackPositions, -ValidMoves)
% Determines valid moves based on stack value and positions
% Board: Current game board state
% MaxValue: Value of the tallest stack for current player
% StackPositions: List of positions with maximum value stacks
% ValidMoves: List of [[StartRow,StartCol], [TargetRow,TargetCol]]
%
% Cases:
% - Height 0 or 1: Moves from [0,0] to any empty position
% - Height > 1: Moves from stack positions to adjacent empty positions
valid_movesAux(Board, 0,_, Positions) :-
  % Every Position without a piece is valid 
  empty_moves(Board, Positions).

valid_movesAux(Board, 1,_, Positions) :-
  % Every Position without a piece is valid 
  empty_moves(Board, Positions).

valid_movesAux(Board, _, StackPositions, Positions) :-
    % Every Position without a piece is valid near stack Positions
    findall([[StackRow, StackCol],[Row,Col]],
        (member([StackRow,StackCol], StackPositions),
         adjacent(StackRow, StackCol, Row, Col),
         board_position(Board,Row,Col),
         access_board(Board,[Row,Col], Elem),
         Elem = '-'),
        PositionsWithDuplicates),
    % Remove duplicates from the list
    sort(PositionsWithDuplicates, Positions).

% empty_moves(+Board, -Moves)
% Finds all possible moves from outside of Board aka new piece [0,0]
% to empty board positions
empty_moves(Board, Positions) :-
  findall([[0,0],[Row,Col]],
      (board_position(Board,Col,Row),
      access_board(Board,[Row,Col], Elem),
      Elem = '-'), Positions).
  
% find_biggest_stacks(+Board, +Next, -Positions, -Max)
% Finds positions of pieces with highest stack value for given player
% Board: Current game board state
% Next: Player identifier ('a' or 'b')
% Positions: List of [Row,Col] positions with maximum stack value
% Max: The maximum stack value found
find_biggest_stacks(Board, Next, Positions, Max) :-
    player_pieces(Board, Next, AllPositions),
    maximum_value(AllPositions, Max),
    positions_with_value(AllPositions, Max, Positions).

% player_pieces(+Board, +Next, -Positions)
% Collects all positions and their values for a given player
% Returns list of (Row, Col, Value) tuples for all player's pieces
player_pieces(Board, Next, Positions) :-
    findall((Row, Col, Value),
        (board_position(Board,Row, Col),
         piece_value(Board, [Row, Col], Next, Value)),
        Positions).

% piece_value(+Board, +Position, +Player, -Value)
% Extracts value of a player's piece
% Position: [Row, Col] coordinates
% Value: Numeric value at position
piece_value(Board, [Row, Col], Player, Value) :-
    access_board(Board, [Row, Col], Piece),
    Piece \= '-',
    atom_chars(Piece, [Player|NumChars]),
    number_chars(Value, NumChars).

% maximum_value(+Positions, -Max)
% Finds the highest stack value among all positions
% Positions: List of (Row, Col, Value) tuples
% Max: Highest value found
maximum_value([],0).
maximum_value(Positions, Max) :-
    findall(Value, member((_, _, Value), Positions), Values),
    max_member(Max, Values).

% positions_with_value(+AllPositions, +Value, -Positions)
% Finds all positions that have a specific value
% AllPositions: List of (Row, Col, Value) tuples
% Value: Target value to match
% Positions: List of [Row, Col] positions with matching value
positions_with_value(AllPositions, Value, Positions) :-
    findall([Row, Col],(member((Row, Col, Value), AllPositions)), Positions).

% adjacent(+Row, +Col, -NewRow, -NewCol)
% See if its a valid adjacent positions on the game board
% Row, Col: Current position coordinates
% NewRow, NewCol: Adjacent position coordinates
%
% A position is adjacent if:
% 1. It's orthogonally connected (up, down, left, right)
% 2. It's diagonally connected (only on intersection points where Row+Col is even)
adjacent(Row, Col, NewRow, NewCol) :-
    steps_patterns(Row,Col,Steps),
    member([SRow, SCol], Steps),
    NewRow is Row + SRow,
    NewCol is Col + SCol.

steps_patterns(Row, Col, Steps) :-
    (Row + Col) mod 2 =:= 0, 
    Steps = [[0,1],[0,-1],[1,0],[-1,0],[1,1],[1,-1],[-1,1],[-1,-1]].

steps_patterns(Row, Col, Steps) :-
    Steps = [[0,1], [0,-1], [1,0], [-1,0]].

% in_sight(+GameState, +Position, -Locations)
% Finds all positions that are in sight of the given position
% GameState: [Board, Next, P1, P2], current board state, next player and AITypes
% Position: [Row,Col] coordinates to check sight from
% Locations: List of [Row,Col] positions that are in sight
%
% A position is in sight if:
% 1. It's in the same row, column, or diagonal (if position is on a diagonal intersection)
% 2. It contains the next player's piece
% 3. It's the first non-empty space encountered in that direction
in_sight(Gamestate, [Row,Col], Locations) :-
  steps_patterns(Row, Col, Steps),
  findall(Location,
    (member(Step,Steps),
    first_sight(Gamestate, [Row, Col], Step, Location)),
    Locations).

% Auxiliar function of in_sight
% first_sight(+GameState, +Position, +Direction, -FoundPosition)
% Finds the first non-empty position from the same player in the given direction, going step by step
% GameState: [Board, Next, P1, P2], current board state, next player and AITypes
% Position: [Row,Col] starting position
% Direction: [DirectionX,DirectionY] direction to search
% FoundPosition: [FRow,FCol] first position found with player's piece
%
% Searches in the specified direction from starting position until:
% - Finding first piece belonging to the current player
% - Reaching board edge (1-5)
% - Finding opponent's piece
% Uses cut (!) to ensure only the first matching piece is returned
first_sight([Board, Next, _, _], [Row, Col], [DirectionX, DirectionY], [FRow, FCol]) :-
  length(Board,Size),
  between(1,Size,Step),
  FRow is Row + (DirectionX * Step),
  FCol is Col + (DirectionY * Step),
  board_position(Board,FRow,FCol),
  access_board(Board, [FRow, FCol], Piece),
  Piece \= '-', !,
  atom_chars(Piece, [Next|_]).

switch_player(a, b).
switch_player(b, a).

firstpiece(a, a1).
firstpiece(b, b1).

% move(+GameState, +Move, -NewGameState)
% Executes a game move and updates the game state
% GameState: [Board, Next, P1, P2], where Board is current board and Next is next player (the one taking action) and AITypes
% Move: [Origin,Target] representing start and end positions of the move
% NewGameState: [NewBoard, NewNext] resulting state after move execution
%
% The predicate succeeds if:
% 1. Move is valid according to game rules
% 2. Target position is within sight
% 3. All board updates are successful
move([Board, Next, P1, P2], [Origin,Target],[NewBoard, NewNext, P1, P2]) :-
  valid_moves([Board, Next, P1, P2], ValidPos),
  member([Origin,Target], ValidPos),
  in_sight([Board, Next, P1, P2], Target, SightPos),
  add1topos(Board,SightPos, Board2),
  firstpiece(Next, Piece),
  replace_on_board(Board2, Target, Piece, Board3),
  change_start_piece(Board3, Origin, NewBoard),
  switch_player(Next, NewNext).

% same as move but validPos is pre calculated
move_pre_val([Board, Next, P1, P2], [Origin,Target],[NewBoard, NewNext, P1, P2], ValidPos) :-
  member([Origin,Target], ValidPos),
  in_sight([Board, Next, _, _], Target, SightPos),
  add1topos(Board,SightPos, Board2),
  firstpiece(Next, Piece),
  replace_on_board(Board2, Target, Piece, Board3),
  change_start_piece(Board3, Origin, NewBoard),
  switch_player(Next, NewNext).
  
% add1topos(+Board, +PosList, -FinalBoard)
% Increments the numeric part of cell values at given positions
% Board: Current game board state
% PosList: List of [X,Y] positions to increment
% FinalBoard: Resulting board after all increments
%
% Each spot in format 'PlayerNumber'
add1topos(Board, [], Board).
add1topos(Board, [[X,Y]|Rest], FinalBoard) :-
    access_board(Board, [X,Y], Val),
  % Non elegant solution but I dont know how to convert it another way
    atom_chars(Val, [Player|NumC]),
    number_chars(Num, NumC),
    NewNum is Num + 1,
    number_chars(NewNum, NewNumC),
    atom_chars(NewVal, [Player|NewNumC]),

    replace_on_board(Board, [X,Y], NewVal, TempBoard),
    add1topos(TempBoard, Rest, FinalBoard).
  
% Similar to add1topos
% When position [0,0] no changes
change_start_piece(Board,[0,0], Board).
change_start_piece(Board, [X,Y], FinalBoard) :-
  access_board(Board, [X,Y], Val),

  atom_chars(Val, [Player|NumC]),
  number_chars(Num, NumC),
  NewNum is Num - 2,
  number_chars(NewNum, NewNumC),
  atom_chars(NewVal, [Player|NewNumC]),

  replace_on_board(Board, [X,Y], NewVal, FinalBoard).

% replace_on_board(+Board, +Position, +Value, -NewBoard)
% Replaces a value at specified position in the game board
% Board: Current game board state (list of lists)
% Position: [X,Y] coordinates where X is column (1-5) and Y is row (1-5)
% Val: New value to place at position
% FinalBoard: Resulting board after replacement
% Note: Y coordinate is inverted (6-Y) due to board representation, no need to check if X and Y are between (1-5) since its validated before
replace_on_board(Board, [X,Y], Val, FinalBoard) :-
  length(Board, Size),
  Y1 is Size +1 -Y,
  nth1(Y1, Board, OldRow),
  replace_nth(OldRow, Val, NewRow, X),
  replace_nth(Board, NewRow, FinalBoard, Y1).

% replace_nth(+List, +NewValue, -NewList, +Position)
% Replaces element at Position in List with NewValue
% Aux function for replace_on_board
% Row1: Original list
% NewValue: Value to insert
% NewRow: Resulting list after replacement
% X: Index where to place new value (1-based)
% Uses append to split and rejoin list around the replacement point based on list_slice/del practical class 3
replace_nth(Row1, NewVal, NewRow, X) :-
  X1 is X -1,
  length(Pref,X1),
  append(Pref, [_ | Suff],Row1),
  append(Pref, [NewVal | Suff], NewRow).
  
% value(+GameState, +Player, -Value)
% Receives the current game state and returns a value measuring how good/bad the current game state is to the given Player, by calculating different metrics
% PlayerPieceCount: Number of pieces the player has on the board
% OpponentPieceCount: Number of pieces the opponent has on the board
% Amountvalue: Value of a current game state according to amount of pieces
% PlayerStackValue: Overall value of stacks (overall "powerfulness" of stacks) the player has on the board
% OpponentStackValue: Overall value of stacks (overall "powerfulness" of stacks) opponent has on the board
% StackValue: Value of a current game state according to overall value of stacks
% PlayerMobility: Number of available valid moves the player has on the board
% OpponentMobility: Number of available valid moves the opponent has on the board
% MobilityValue: Value of a current game state according to amount of available valid moves
% Value: Overall value of a current game state (using the previously calculated values, with different levels of "importance")


value([Board, Next, _, _], Player, Value) :-
    player_pieces(Board, Player, PlayerPieces),
    length(PlayerPieces, PlayerPieceCount),
    switch_player(Player, Opponent),
    player_pieces(Board, Opponent, OpponentPieces),
    length(OpponentPieces, OpponentPieceCount),
    AmountValue is PlayerPieceCount - OpponentPieceCount,
    
    stack_value(Board, Player, PlayerStackValue),
    stack_value(Board, Opponent, OpponentStackValue),
    StackValue is PlayerStackValue - OpponentStackValue,

    valid_moves([Board, Player, _, _], PlayerMoves),
    length(PlayerMoves, PlayerMobility),
    valid_moves([Board, Opponent,_,_], OpponentMoves),
    length(OpponentMoves, OpponentMobility),
    MobilityValue is PlayerMobility - OpponentMobility,

    Value is (AmountValue * 1) + (StackValue * (-2)) + (MobilityValue * 2).

% stack_value(+Board, +Player, -Value)
% Sums the value of all the stacks of a player by iterating each player piece and getting its value
% Values: List that stores all the values of all the stacks of a player
% Value: Sum of all the elements of the list values
% This gives us the overall "powerfulness" of a players stacks
stack_value(Board, Player, Value):-
    player_pieces(Board, Player, Positions),
    findall(StackValue, (member((_,_,StackValue), Positions)), Values),
    sumlist(Values, Value).

% game_over(+GameState, -Winner)
% Determines if game is over and who won
% GameState: [Board, Next, _, _] current board and player to move
% Winner: Player who won the game
% Game is over when current player has no valid moves, losing the game
game_over([Board, Next, _, _], Winner) :-
    valid_moves([Board, Next, _, _], Positions),
    game_overAux(Next, Positions, Winner).

game_overAux(Next, [], Winner):-
    switch_player(Next, Winner).

game_overAux(Next, Moves, 0).

% check_no_stacks(+Moves)
% Checks if moves start from position [0,0] (new piece placement)
% Moves: List of [[FromPos,ToPos]|Rest] valid moves
check_no_stacks([ [FPos | _] |_]) :-
    FPos = [0,0].


% move_type(+Moves, -Type)
% Determines move type based on valid moves
% Type: new_piece if placing new piece, stack if moving existing piece
move_type(Moves, new_piece) :-
  check_no_stacks(Moves),!.
move_type(_, stack).

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

handle_input(Input,Pos, Moves) :-
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


% valid_chess_letter(+Letter)
% Checks if the given letter is a valid chess coordinate letter
valid_chess_letter(Letter) :-
    char_code(Letter, Code),
    between(97,122,Code).


% valid_chess_coord(+ChessCoord)
% Validates if the input is a valid chess coordinate
valid_chess_coord(ChessCoord) :-
    atom_chars(ChessCoord, [Letter|NumberChars]),
    valid_chess_letter(Letter),
    NumberChars \= [].

% choose_move(+GameState, +Level, -Move)
% Selects a move for the given computer player based on the current GameState.
% If the computer is random (level 1), a random valid move is chosen, if the 
% computer is smart (level 2), the best move is selected based on the evaluation 
% of game states (uses the value function).

choose_move(GameState, 1, Move) :-
  valid_moves(GameState, Moves),
  random_member(Move, Moves).

choose_move(GameState, 2, BestMove) :-
  valid_moves(GameState, Moves),
  find_best_move(GameState, Moves, BestMove).

% find_best_move(+GameState, +Moves, -BestMove)
% Evaluates each move in the list of Moves for the given GameState and returns the move with the highest value.
% Simulates each move and computes the resulting game state, then uses the value/3 predicate to calculate the
% heuristic value of each resulting game state, and % finally calls find_best_move_aux/5 to iterate over all
% remaining moves and find the optimal one.

find_best_move([Board, Next, P1, P2], [Move|RestMoves], BestMove) :-
  move([Board, Next, P1, P2], Move, NewState),
  value(NewState,Next,Value),
  find_best_move_aux([Board, Next, P1, P2], RestMoves, Move, Value, BestMove).

% find_best_move_aux(+GameState, +Moves, +CurrentBestMove, +CurrentBestValue, -BestMove)
% Helper predicate that recursively (tail recursion) evaluates a list of moves to find the best one.
% For each move, it simulates the game state and calculates its heuristic value, then compares the 
% value of the current move with the CurrentBestValue, it updates CurrentBestMove if a better move 
% is found and continues the recursion, finally, it ends recursion when there are no more moves left,
% returning the BestMove.

find_best_move_aux(_, [], BestMove, _, BestMove).
find_best_move_aux([Board, Next, P1, P2], [Move|RestMoves], CurrentBestMove, CurrentBestValue, BestMove) :-
  move([Board, Next, P1, P2], Move, NewState),
  value(NewState, Next, NewValue),
  (  NewValue > CurrentBestValue
  -> find_best_move_aux([Board, Next, P1, P2], RestMoves, Move, NewValue, BestMove)
  ;  find_best_move_aux([Board, Next, P1, P2], RestMoves, CurrentBestMove, CurrentBestValue, BestMove)
  ).
