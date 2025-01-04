% Runs a series of tests to verify the functionality of the choose_move/3 predicate for different levels. 
% If all tests pass, outputs a success message 'all choose_move tests done'.
run_choose_move_tests :-
    test_level1_valid_move,
    test_level1_randomness,
    test_level2_best_move,
    test_level2_no_valid_moves,
    test_level3_killer_move,
    write('all choose_move tests done'), nl.

% Tests that the choose_move/3 predicate for player level 1 returns a valid move from the list of valid moves.
test_level1_valid_move :-
    initial_state((5, 1, 0), GameState),
    valid_moves(GameState, ValidMoves),
    choose_move(GameState, 1, Move),
    member(Move, ValidMoves),
    write('level 1 valid move test passed'),nl.

% Tests that the choose_move/3 predicate for player level 1 produces random moves across multiple calls.
test_level1_randomness :-
    initial_state((5, 1, 0), GameState),
    findall(Move, (between(1, 10, _), choose_move(GameState, 1, Move)), Moves),
    sort(Moves, SortedMoves),
    length(Moves, Len),
    length(SortedMoves, SortedLen),
    Len > SortedLen,
    write('level 1 randomness test passed'),nl.

% Tests that the choose_move/3 predicate for player level 2 selects the best move as determined by find_best_move/3.
test_level2_best_move :-
    initial_state((5, 0, 2), GameState),
    valid_moves(GameState, Moves),
    find_best_move(GameState, Moves, BestMove),
    choose_move(GameState, 2, ChosenMove),
    BestMove = ChosenMove,
    write('level 2 best move test passed'),nl.

% Tests that the game correctly identifies a game-over condition when no valid moves are available.
test_level2_no_valid_moves :-
    testBoard(g1, Board),
    game_over([Board, a, 0, 0], Winner),
    Winner \= 0,
    write('level 2 no valid moves test passed'),nl.

test_level3_killer_move :-
    Testboard = [[-,-,-,-,-],[-,-,-,-,-],[-,-,-,-,-],[-,b1,-,-,-],[a2,a1,-,-,-]],
    choose_move([Testboard, b, 3,3],3, ChosenMove),
    ChosenMove = [[0,0],[1,2]],
    write('level 3 test passed'),nl.



