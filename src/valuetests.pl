% Runs a series of tests to verify the correctness of the value/3 predicate. If all tests pass, outputs a success message 'value tests passed'.
value_tests :-
    test_value_basic,
    test_value_advantage,
    test_value_balanced,
    write('value tests passed'),nl.

% Tests the basic functionality of the value/3 predicate when evaluating a simple board with only one piece.
test_value_basic :-
    testBoard(5, Board2),
    value([Board2, a,0,0], a, Value3),
    Value3 < 0,
    value([Board2, b,0,0], b, Value4),
    Value4 > 0.

% Tests the value/3 predicate on a more complex board configuration where one player has a clear advantage.
test_value_advantage :-
    testBoard(7, Board),
    value([Board, a,0,0], a, Value1),
    Value1 > 0,
    value([Board, b,0,0], b, Value2),
    Value2 < 0.

% Tests the value/3 predicate on a board with a more balanced position for both players.
test_value_balanced :-
    testBoard(6, Board),
    value([Board, a,0,0], a, Value1),
    abs(Value1) < 10, 
    value([Board, b,0,0], b, Value2),
    abs(Value2) < 10.

