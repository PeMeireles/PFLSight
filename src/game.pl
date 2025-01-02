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
:- use_module(library(between)).
:- use_module(library(lists)).

play :- 
    repeat,
    clear_screen,
    display_start_menu,
    read(Choice),
    validate_choice(Choice, [1,2,3,4]),
    handle_start_choice(Choice, Options), !.
  %start_game(Options), !.


handle_start_choice(1, [5, 0, 0]) :- !.

handle_start_choice(2, [5, 0, Choice]) :-
  repeat,
  display_menu(computer),
  read(Choice),
  validate_choice(Choice, [1,2]),!.

handle_start_choice(3, [5,Choice1, Choice2]) :-
  repeat,
  display_menu(computer),
  read(Choice1),
  validate_choice(Choice1, [1,2]),
  write('And for the second one.'),nl,
  display_menu(computer),
  read(Choice2),
  validate_choice(Choice2, [1,2]), !.

handle_start_choice(4, _) :-
  write('If this character is a 4 stack white piece its working: P'), nl,
  wait_for_enter, !.

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
  Y1 is 6 - Y,
  board_position(X,Y),
  nth1(Y1, Board, Row),
  nth1(X,Row,Val).

% board_position(-Row, -Col)
% Generates valid board coordinates
% Succeeds for all positions within 5x5 board
board_position(Row, Col) :-
    between(1, 5, Row),
    between(1, 5, Col).

% valid_moves(+GameState, -ValidMoves)
% Determines all valid moves for current player
% GameState: [Board, Next] representing current board and player
% ValidMoves: List of [[StartRow,StartCol], [TargetRow,TargetCol]] valid moves
valid_moves([Board, Next], Positions) :-
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
         board_position(Row,Col),
         access_board(Board,[Row,Col], Elem),
         Elem = '-'),
        PositionsWithDuplicates),
    % Remove duplicates from the list
    sort(PositionsWithDuplicates, Positions).

% empty_moves(+Board, -Moves)
% Finds all possible moves from outside of Board aka new piece [0,0]
% to empty board positions
empty_moves(Board, Spaces) :-
  findall([[0,0],[Row,Col]],
      (board_position(Col,Row),
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
    (AllPositions = [] -> Max = 0, Positions = []
        ;
       maximum_value(AllPositions, Max),
       positions_with_value(AllPositions, Max, Positions)
     ).

% player_pieces(+Board, +Next, -Positions)
% Collects all positions and their values for a given player
% Returns list of (Row, Col, Value) tuples for all player's pieces
player_pieces(Board, Next, Positions) :-
    findall((Row, Col, Value),
        (board_position(Row, Col),
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
% GameState: [Board, Next], current board state and next player
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
% GameState: [Board, Next], current board state and next player
% Position: [Row,Col] starting position
% Direction: [DirectionX,DirectionY] direction to search
% FoundPosition: [FRow,FCol] first position found with player's piece
%
% Searches in the specified direction from starting position until:
% - Finding first piece belonging to the current player
% - Reaching board edge (1-5)
% - Finding opponent's piece
% Uses cut (!) to ensure only the first matching piece is returned
first_sight([Board, Next], [Row, Col], [DirectionX, DirectionY], [FRow, FCol]) :-
  between(1,5,Step),
  FRow is Row + (DirectionX * Step),
  FCol is Col + (DirectionY * Step),
  board_position(FRow,FCol),
  access_board(Board, [FRow, FCol], Piece),
  Piece \= '-',
  atom_chars(Piece, [Next|_]), !.

switch_player(a, b).
switch_player(b, a).

firstpiece(a, a1).
firstpiece(b, b1).

% move(+GameState, +Move, -NewGameState)
% Executes a game move and updates the game state
% GameState: [Board, Next], where Board is current board and Next is next player (the one taking action)
% Move: [Origin,Target] representing start and end positions of the move
% NewGameState: [NewBoard, NewNext] resulting state after move execution
%
% The predicate succeeds if:
% 1. Move is valid according to game rules
% 2. Target position is within sight
% 3. All board updates are successful
move([Board, Next], [Origin,Target],[NewBoard, NewNext]) :-
  valid_moves([Board, Next], ValidPos),
  member([Origin,Target], ValidPos),
  in_sight([Board, Next], Target, SightPos),
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
  Y1 is 6-Y,
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


value([Board, Next], Player, Value) :-
    player_pieces(Board, Player, PlayerPieces),
    length(PlayerPieces, PlayerPieceCount),
    switch_player(Player, Opponent),
    player_pieces(Board, Opponent, OpponentPieces),
    length(OpponentPieces, OpponentPieceCount),
    AmountValue is PlayerPieceCount - OpponentPieceCount,
    
    stack_value(Board, Player, PlayerStackValue),
    stack_value(Board, Opponent, OpponentStackValue),
    StackValue is PlayerStackValue - OpponentStackValue,

   valid_moves([Board, Player], PlayerMoves),
   length(PlayerMoves, PlayerMobility),
    valid_moves([Board, Opponent], OpponentMoves),
   length(OpponentMoves, OpponentMobility),
   MobilityValue is PlayerMobility - OpponentMobility,

    Value is (AmountValue * 1) + (StackValue * 2) + (MobilityValue * 2).

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
% GameState: [Board, Next] current board and player to move
% Winner: Player who won the game
% Game is over when current player has no valid moves, losing the game
game_over([Board, Next], Winner) :-
    valid_moves([Board, Next], []),
    switch_player(Next, Winner).

