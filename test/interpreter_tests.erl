-module(interpreter_tests).

-include_lib("eunit/include/eunit.hrl").

exec_transition_that_continues_and_goes_to_left_by_reusing_square_test() ->
    InitialTape = ["0", "0"],
    InitialIndexOnTape = 2,
    {continue, NewTape, NewIndex} = interpreter:read_and_exec(InitialIndexOnTape, InitialTape, config),
    ?assertEqual(1, NewIndex),
    ?assertMatch(["0", "."], NewTape).

exec_transition_that_continues_and_goes_to_left_by_expanding_test() ->
    InitialTape = ["0", "1"],
    InitialIndexOnTape = 1,
    {continue, NewTape, NewIndex} = interpreter:read_and_exec(InitialIndexOnTape, InitialTape, config),
    ?assertEqual(1, NewIndex),
    ?assertMatch([".", ".", "1"], NewTape).

exec_transition_that_continues_and_goes_to_right_by_reusing_square_test() ->
    InitialTape = ["1", "0"],
    InitialIndexOnTape = 1,
    {continue, NewTape, NewIndex} = interpreter:read_and_exec(InitialIndexOnTape, InitialTape, config),
    ?assertEqual(2, NewIndex),
    ?assertMatch(["0", "0"], NewTape).

exec_transition_that_continues_and_goes_to_right_by_expanding_test() ->
    InitialTape = ["0", "1"],
    InitialIndexOnTape = 2,
    {continue, NewTape, NewIndex} = interpreter:read_and_exec(InitialIndexOnTape, InitialTape, config),
    ?assertEqual(3, NewIndex),
    ?assertMatch(["0", "0", "."], NewTape).

exec_transition_that_continues_and_move_on_the_middle_of_a_large_tape_test() ->
    InitialTape = ["0", "0", "0", "1", "0", "0"],
    InitialIndexOnTape = 4,
    {continue, NewTape, NewIndex} = interpreter:read_and_exec(InitialIndexOnTape, InitialTape, config),
    ?assertEqual(5, NewIndex),
    ?assertMatch(["0", "0", "0", "0", "0", "0"], NewTape).

exec_transition_that_blocks_test() ->
    InitialTape = ["0", "2"],
    InitialIndexOnTape = 2,
    {blocked, InitialTape, InitialIndexOnTape} = interpreter:read_and_exec(InitialIndexOnTape, InitialTape, config).
