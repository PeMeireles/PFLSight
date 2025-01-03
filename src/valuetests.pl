value_tests :-
    test_value_basic,
    test_value_advantage,
    test_value_balanced,
    write('Value tests passed'),nl.

test_value_basic :-
    % One piece
    testBoard(5, Board2),
    value([Board2, a,0,0], a, Value3),
    Value3 > 0,
    value([Board2, b,0,0], b, Value4),
    Value4 < 0.


test_value_advantage :-
    testBoard(4, Board),
    value([Board, a,0,0], a, Value1),
    Value1 > 0,
    value([Board, b,0,0], b, Value2),
    Value2 < 0.

test_value_balanced :-
    testBoard(6, Board),
    value([Board, a,0,0], a, Value1),
    %abs(Value1) < 10, 
    value([Board, b,0,0], b, Value2),
    abs(Value2) < 10.
