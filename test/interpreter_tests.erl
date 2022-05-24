-module(interpreter_tests).

-include("../src/machine.hrl").

-include_lib("eunit/include/eunit.hrl").

get_base_blank_char() -> ".".

get_default_current_state() -> "IDLE".

get_available_transitions() ->
    [
        #parsed_machine_config_transition{
            read = "0", to_state = "IDLE", write = ".", action = left
        },
        #parsed_machine_config_transition{
            read = "1", to_state = "IDLE", write = "0", action = right
        }
    ].

exec_transition_that_continues_and_goes_to_left_by_reusing_square_test() ->
    InitialTape = ["0", "0"],
    InitialIndexOnTape = 2,
    {continue, NewTape, NewIndex, "IDLE"} = interpreter:read_and_exec(
        InitialIndexOnTape,
        InitialTape,
        get_available_transitions(),
        get_base_blank_char(),
        get_default_current_state()
    ),
    ?assertEqual(1, NewIndex),
    ?assertMatch(["0", "."], NewTape).

exec_transition_that_continues_and_goes_to_left_by_expanding_test() ->
    InitialTape = ["0", "1"],
    InitialIndexOnTape = 1,
    {continue, NewTape, NewIndex, "IDLE"} = interpreter:read_and_exec(
        InitialIndexOnTape,
        InitialTape,
        get_available_transitions(),
        get_base_blank_char(),
        get_default_current_state()
    ),
    ?assertEqual(1, NewIndex),
    ?assertMatch([".", ".", "1"], NewTape).

exec_transition_that_continues_and_goes_to_right_by_reusing_square_test() ->
    InitialTape = ["1", "0"],
    InitialIndexOnTape = 1,
    {continue, NewTape, NewIndex, "IDLE"} = interpreter:read_and_exec(
        InitialIndexOnTape,
        InitialTape,
        get_available_transitions(),
        get_base_blank_char(),
        get_default_current_state()
    ),
    ?assertEqual(2, NewIndex),
    ?assertMatch(["0", "0"], NewTape).

exec_transition_that_continues_and_goes_to_right_by_expanding_test() ->
    InitialTape = ["0", "1"],
    InitialIndexOnTape = 2,
    {continue, NewTape, NewIndex, "IDLE"} = interpreter:read_and_exec(
        InitialIndexOnTape,
        InitialTape,
        get_available_transitions(),
        get_base_blank_char(),
        get_default_current_state()
    ),
    ?assertEqual(3, NewIndex),
    ?assertMatch(["0", "0", "."], NewTape).

exec_transition_that_continues_and_move_on_the_middle_of_a_large_tape_test() ->
    InitialTape = ["0", "0", "0", "1", "0", "0"],
    InitialIndexOnTape = 4,
    {continue, NewTape, NewIndex, "IDLE"} = interpreter:read_and_exec(
        InitialIndexOnTape,
        InitialTape,
        get_available_transitions(),
        get_base_blank_char(),
        get_default_current_state()
    ),
    ?assertEqual(5, NewIndex),
    ?assertMatch(["0", "0", "0", "0", "0", "0"], NewTape).

exec_transition_that_blocks_test() ->
    InitialTape = ["0", "2"],
    InitialIndexOnTape = 2,
    {blocked, InitialTape, InitialIndexOnTape} = interpreter:read_and_exec(
        InitialIndexOnTape,
        InitialTape,
        get_available_transitions(),
        get_base_blank_char(),
        get_default_current_state()
    ).

exec_transition_to_state_test() ->
    InitialTape = ["0", "1", "1", "0"],
    InitialIndexOnTape = 1,
    Transitions = [
        #parsed_machine_config_transition{
            read = "0", to_state = "DAY", write = ".", action = right
        },
        #parsed_machine_config_transition{
            read = "1", to_state = "NIGHT", write = "0", action = right
        }
    ],
    {continue, [".", "1", "1", "0"], 2, "DAY"} = interpreter:read_and_exec(
        InitialIndexOnTape,
        InitialTape,
        Transitions,
        get_base_blank_char(),
        get_default_current_state()
    ).

exec_transition_that_extends_left_the_tape_with_blank_char_test() ->
    InitialTape = ["0", "0", "0"],
    InitialIndexOnTape = 1,
    {continue, ["$", ".", "0", "0"], 1, "IDLE"} = interpreter:read_and_exec(
        InitialIndexOnTape,
        InitialTape,
        get_available_transitions(),
        "$",
        get_default_current_state()
    ).

exec_transition_that_extends_right_the_tape_with_blank_char_test() ->
    InitialTape = ["0", "0", "1"],
    InitialIndexOnTape = 3,
    {continue, ["0", "0", "0", "*"], 4, "IDLE"} = interpreter:read_and_exec(
        InitialIndexOnTape,
        InitialTape,
        get_available_transitions(),
        "*",
        get_default_current_state()
    ).
