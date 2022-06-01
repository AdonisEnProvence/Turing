-module(machine_validator_test).

-include("../src/machine.hrl").

-include_lib("eunit/include/eunit.hrl").

get_valid_alphabet() ->
    [
        "1",
        ".",
        "-",
        "="
    ].

get_valid_states() ->
    [
        "scanright",
        "eraseone",
        "subone",
        "skip",
        "HALT"
    ].

get_valid_transitions_map() ->
    #{
        "scanright" => [
            #parsed_machine_config_transition{
                read = ".",
                to_state = "scanright",
                write = ".",
                action = right
            },
            #parsed_machine_config_transition{
                read = "=",
                to_state = "skip",
                write = "-",
                action = left
            }
        ],
        "eraseone" => [
            #parsed_machine_config_transition{
                read = ".",
                to_state = "HALT",
                write = ".",
                action = right
            },
            #parsed_machine_config_transition{
                read = "-",
                to_state = "eraseone",
                write = "=",
                action = left
            }
        ],
        "subone" => [
            #parsed_machine_config_transition{
                read = ".",
                to_state = "scanright",
                write = ".",
                action = right
            },
            #parsed_machine_config_transition{
                read = "-",
                to_state = "subone",
                write = ".",
                action = left
            }
        ]
    }.

% Validate Alphabet
validate_machine_alphabet_test() ->
    ok = machine_validator:validate_machine_alphabet(get_valid_alphabet()).
parse_machine_alphabet_duplicated_entry_test() ->
    ExpectedErrorContent = {duplicated_elements, ["="]},
    {error, ExpectedErrorContent} = machine_validator:validate_machine_alphabet(
        get_valid_alphabet() ++ ["="]
    ),
    ?assertMatch(
        "machine alphabet has duplicated elements ([\"=\"]); Machine alphabet must contains unique elements",
        machine_validator:format_error({alphabet, ExpectedErrorContent})
    ).
parse_machine_alphabet_several_duplicated_entry_test() ->
    ExpectedErrorContent = {duplicated_elements, [".", "="]},
    {error, ExpectedErrorContent} = machine_validator:validate_machine_alphabet(
        get_valid_alphabet() ++ [".", "="]
    ),
    ?assertMatch(
        "machine alphabet has duplicated elements ([\".\",\"=\"]); Machine alphabet must contains unique elements",
        machine_validator:format_error({alphabet, ExpectedErrorContent})
    ).

% Validate States
validate_machine_states_test() ->
    ok = machine_validator:validate_machine_states(get_valid_states()).
parse_machine_states_duplicated_entry_test() ->
    ExpectedErrorContent = {duplicated_elements, ["HALT"]},
    {error, ExpectedErrorContent} = machine_validator:validate_machine_states(
        get_valid_states() ++ ["HALT"]
    ),
    ?assertMatch(
        "machine states has duplicated elements ([\"HALT\"]); Machine states must contains unique elements",
        machine_validator:format_error({states, ExpectedErrorContent})
    ).
parse_machine_states_several_duplicated_entry_test() ->
    ExpectedErrorContent = {duplicated_elements, ["HALT", "scanright"]},
    {error, ExpectedErrorContent} = machine_validator:validate_machine_states(
        get_valid_states() ++ ["HALT", "scanright"]
    ),
    ?assertMatch(
        "machine states has duplicated elements ([\"HALT\",\"scanright\"]); Machine states must contains unique elements",
        machine_validator:format_error({states, ExpectedErrorContent})
    ).

% Validate Blank
validate_machine_blank_test() ->
    ok = machine_validator:validate_machine_blank(".", get_valid_alphabet()).
validate_machine_blank_not_alphabet_character_error_test() ->
    ExpectedErrorContent = {expected_alphabet_character, "$"},
    {error, ExpectedErrorContent} = machine_validator:validate_machine_blank(
        "$", get_valid_alphabet()
    ),
    ?assertMatch(
        "machine blank is not an alphabet character (received: $); Machine blank must contains an alphabet character",
        machine_validator:format_error({blank, ExpectedErrorContent})
    ).

% Validate Finals
validate_machine_finals_test() ->
    ok = machine_validator:validate_machine_finals(["HALT"], get_valid_states()).

validate_machine_finals_duplicated_entry_test() ->
    ExpectedErrorContent = {duplicated_elements, ["HALT"]},
    {error, ExpectedErrorContent} = machine_validator:validate_machine_finals(
        ["HALT", "scanright", "HALT"], get_valid_states()
    ),
    ?assertMatch(
        "machine finals has duplicated elements ([\"HALT\"]); Machine finals must contain unique elements listed by the machine states list",
        machine_validator:format_error({finals, ExpectedErrorContent})
    ).

validate_machine_finals_expected_states_entry_error_test() ->
    ExpectedErrorContent = {expected_states, ["invalid_state_entry"]},
    {error, {expected_states, ["invalid_state_entry"]}} = machine_validator:validate_machine_finals(
        ["HALT", "invalid_state_entry"], get_valid_states()
    ),
    ?assertMatch(
        "machine finals has not states listed elements ([\"invalid_state_entry\"]); Machine finals must contain unique elements listed by the machine states list",
        machine_validator:format_error({finals, ExpectedErrorContent})
    ).

validate_machine_finals_several_expected_states_entry_error_test() ->
    ExpectedErrorContent = {expected_states, ["invalid_state_entry_0", "invalid_state_entry_1"]},
    {error, ExpectedErrorContent} = machine_validator:validate_machine_finals(
        ["HALT", "invalid_state_entry_0", "invalid_state_entry_1"], get_valid_states()
    ),
    ?assertMatch(
        "machine finals has not states listed elements ([\"invalid_state_entry_0\",\"invalid_state_entry_1\"]); Machine finals must contain unique elements listed by the machine states list",
        machine_validator:format_error({finals, ExpectedErrorContent})
    ).

% Validate Initial
validate_machine_initial_test() ->
    ok = machine_validator:validate_machine_initial("scanright", get_valid_states()).

validate_machine_initial_expected_state_error_test() ->
    {error, {expected_state, "invalid_state"}} = machine_validator:validate_machine_initial(
        "invalid_state", get_valid_states()
    ).

% Validate transitions
validate_machine_transitions_test() ->
    ok = machine_validator:validate_machine_transitions(
        get_valid_transitions_map(),
        get_valid_states(),
        get_valid_alphabet()
    ).

validate_machine_transitions_duplicated_error_test() ->
    {error, {"subone", {duplicated_elements, ["."]}}} = machine_validator:validate_machine_transitions(
        #{
            "skip" => [
                #parsed_machine_config_transition{
                    read = ".",
                    to_state = "scanright",
                    write = ".",
                    action = right
                },
                #parsed_machine_config_transition{
                    read = "-",
                    to_state = "subone",
                    write = "=",
                    action = left
                }
            ],
            "scanright" => [
                #parsed_machine_config_transition{
                    read = ".",
                    to_state = "scanright",
                    write = ".",
                    action = right
                },
                #parsed_machine_config_transition{
                    read = "-",
                    to_state = "subone",
                    write = "=",
                    action = left
                }
            ],
            "subone" => [
                #parsed_machine_config_transition{
                    read = ".",
                    to_state = "scanright",
                    write = ".",
                    action = right
                },
                #parsed_machine_config_transition{
                    read = ".",
                    to_state = "scanright",
                    write = ".",
                    action = right
                },
                #parsed_machine_config_transition{
                    read = "-",
                    to_state = "subone",
                    write = "=",
                    action = left
                }
            ]
        },
        get_valid_states(),
        get_valid_alphabet()
    ).

validate_machine_transitions_several_duplicated_error_test() ->
    {error, {"scanright", {duplicated_elements, [".", "-"]}}} = machine_validator:validate_machine_transitions(
        #{
            "skip" => [
                #parsed_machine_config_transition{
                    read = ".",
                    to_state = "scanright",
                    write = ".",
                    action = right
                },
                #parsed_machine_config_transition{
                    read = "-",
                    to_state = "subone",
                    write = "=",
                    action = left
                }
            ],
            "scanright" => [
                #parsed_machine_config_transition{
                    read = ".",
                    to_state = "scanright",
                    write = ".",
                    action = right
                },
                #parsed_machine_config_transition{
                    read = ".",
                    to_state = "scanright",
                    write = ".",
                    action = right
                },
                #parsed_machine_config_transition{
                    read = "-",
                    to_state = "subone",
                    write = "=",
                    action = left
                },
                #parsed_machine_config_transition{
                    read = "-",
                    to_state = "subone",
                    write = "=",
                    action = left
                }
            ],
            "subone" => [
                % Below duplcation on `"read":"."` is voluntary error, as you can see it will not achieve to validate this data as
                % above one is already failing
                #parsed_machine_config_transition{
                    read = ".",
                    to_state = "scanright",
                    write = ".",
                    action = right
                },
                #parsed_machine_config_transition{
                    read = ".",
                    to_state = "scanright",
                    write = ".",
                    action = right
                },
                #parsed_machine_config_transition{
                    read = "-",
                    to_state = "subone",
                    write = "=",
                    action = left
                }
            ]
        },
        get_valid_states(),
        get_valid_alphabet()
    ).

validate_machine_transitions_not_alphabet_read_test() ->
    {error, {"skip", {read, {expected_alphabet_character, "not_alphabet_character"}}}} = machine_validator:validate_machine_transitions(
        #{
            "scanright" => [
                #parsed_machine_config_transition{
                    read = ".",
                    to_state = "scanright",
                    write = ".",
                    action = right
                },
                #parsed_machine_config_transition{
                    read = "-",
                    to_state = "subone",
                    write = "=",
                    action = left
                }
            ],
            "skip" => [
                #parsed_machine_config_transition{
                    read = "not_alphabet_character",
                    to_state = "scanright",
                    % Below error is voluntary, as it verifies that validator will
                    % Look at read first
                    write = "also_not_alphabet_character",
                    action = right
                },
                #parsed_machine_config_transition{
                    read = "-",
                    to_state = "subone",
                    write = "=",
                    action = left
                }
            ],
            "subone" => [
                #parsed_machine_config_transition{
                    read = ".",
                    to_state = "scanright",
                    write = ".",
                    action = right
                },
                #parsed_machine_config_transition{
                    read = "-",
                    to_state = "subone",
                    write = "=",
                    action = left
                }
            ]
        },
        get_valid_states(),
        get_valid_alphabet()
    ).

validate_machine_transitions_not_alphabet_write_test() ->
    {error, {"scanright", {write, {expected_alphabet_character, "not_alphabet_character"}}}} = machine_validator:validate_machine_transitions(
        #{
            "skip" => [
                #parsed_machine_config_transition{
                    read = ".",
                    to_state = "scanright",
                    write = ".",
                    action = right
                },
                #parsed_machine_config_transition{
                    read = "-",
                    to_state = "subone",
                    write = "=",
                    action = left
                }
            ],
            "scanright" => [
                #parsed_machine_config_transition{
                    read = ".",
                    to_state = "scanright",
                    write = "=",
                    action = right
                },
                #parsed_machine_config_transition{
                    read = "=",
                    to_state = "subone",
                    write = "not_alphabet_character",
                    action = left
                }
            ],
            "subone" => [
                #parsed_machine_config_transition{
                    read = ".",
                    to_state = "scanright",
                    write = ".",
                    action = right
                },
                #parsed_machine_config_transition{
                    read = "-",
                    to_state = "subone",
                    write = "=",
                    action = left
                }
            ]
        },
        get_valid_states(),
        get_valid_alphabet()
    ).

validate_machine_transitions_invalid_key_error_test() ->
    {error, {expected_states, ["invalid_state_transition_key"]}} = machine_validator:validate_machine_transitions(
        #{
            "subone" => [
                #parsed_machine_config_transition{
                    read = ".",
                    to_state = "scanright",
                    write = ".",
                    action = right
                },
                #parsed_machine_config_transition{
                    read = "-",
                    to_state = "subone",
                    write = "=",
                    action = left
                }
            ],
            "scanright" => [
                #parsed_machine_config_transition{
                    read = ".",
                    to_state = "scanright",
                    write = "=",
                    action = right
                },
                #parsed_machine_config_transition{
                    read = "=",
                    to_state = "subone",
                    write = ".",
                    action = left
                }
            ],
            "invalid_state_transition_key" => [
                #parsed_machine_config_transition{
                    read = ".",
                    to_state = "scanright",
                    write = ".",
                    action = right
                },
                #parsed_machine_config_transition{
                    read = "-",
                    to_state = "subone",
                    write = "=",
                    action = left
                }
            ]
        },
        get_valid_states(),
        get_valid_alphabet()
    ).

validate_machine_transitions_several_invalid_key_error_test() ->
    {error,
        {expected_states, ["also_invalid_state_transition_key", "invalid_state_transition_key"]}} = machine_validator:validate_machine_transitions(
        #{
            "also_invalid_state_transition_key" => [
                #parsed_machine_config_transition{
                    read = ".",
                    to_state = "scanright",
                    write = ".",
                    action = right
                },
                #parsed_machine_config_transition{
                    read = "-",
                    to_state = "subone",
                    write = "=",
                    action = left
                }
            ],
            "scanright" => [
                #parsed_machine_config_transition{
                    read = ".",
                    to_state = "scanright",
                    write = "=",
                    action = right
                },
                #parsed_machine_config_transition{
                    read = "=",
                    to_state = "subone",
                    write = ".",
                    action = left
                }
            ],
            "invalid_state_transition_key" => [
                #parsed_machine_config_transition{
                    read = ".",
                    to_state = "scanright",
                    write = ".",
                    action = right
                },
                #parsed_machine_config_transition{
                    read = "-",
                    to_state = "subone",
                    write = "=",
                    action = left
                }
            ]
        },
        get_valid_states(),
        get_valid_alphabet()
    ).

validate_machine_transitions_invalid_to_state_error_test() ->
    % Note: Erlang looks like iterating over map keys via their ascii value, then here scanright is verified first
    {error, {"scanright", {to_state, {expected_state, "invalid_to_state"}}}} = machine_validator:validate_machine_transitions(
        #{
            "subone" => [
                #parsed_machine_config_transition{
                    read = ".",
                    to_state = "scanright",
                    write = ".",
                    action = right
                },
                #parsed_machine_config_transition{
                    read = "-",
                    to_state = "subone",
                    write = "=",
                    action = left
                }
            ],
            "scanright" => [
                #parsed_machine_config_transition{
                    read = ".",
                    to_state = "scanright",
                    write = "=",
                    action = right
                },
                #parsed_machine_config_transition{
                    read = "=",
                    to_state = "invalid_to_state",
                    write = ".",
                    action = left
                }
            ],
            "subone" => [
                #parsed_machine_config_transition{
                    read = ".",
                    to_state = "scanright",
                    write = ".",
                    action = right
                },
                #parsed_machine_config_transition{
                    read = "-",
                    to_state = "subone",
                    write = "=",
                    action = left
                }
            ]
        },
        get_valid_states(),
        get_valid_alphabet()
    ).

validate_machine_success_test() ->
    ok = machine_validator:validate_machine(#parsed_machine_config{
        initial = "subone",
        blank = ".",
        states = get_valid_states(),
        finals = ["HALT"],
        alphabet = get_valid_alphabet(),
        transitions = get_valid_transitions_map()
    }).

validate_machine_initial_error_test() ->
    {error, initial, {expected_state, "invalid_state"}} = machine_validator:validate_machine(
        #parsed_machine_config{
            initial = "invalid_state",
            blank = ".",
            states = get_valid_states(),
            finals = ["HALT"],
            alphabet = get_valid_alphabet(),
            transitions = get_valid_transitions_map()
        }
    ).

validate_machine_blank_error_test() ->
    {error, blank, {expected_alphabet_character, "not_alphabet_character"}} = machine_validator:validate_machine(
        #parsed_machine_config{
            initial = "subone",
            blank = "not_alphabet_character",
            states = get_valid_states(),
            finals = ["HALT"],
            alphabet = get_valid_alphabet(),
            transitions = get_valid_transitions_map()
        }
    ).

validate_machine_states_error_test() ->
    {error, states, {duplicated_elements, ["HALT", "scanright"]}} = machine_validator:validate_machine(
        #parsed_machine_config{
            initial = "subone",
            blank = ".",
            states = get_valid_states() ++ ["HALT", "scanright"],
            finals = ["HALT"],
            alphabet = get_valid_alphabet(),
            transitions = get_valid_transitions_map()
        }
    ).

validate_machine_finals_error_test() ->
    {error, finals, {expected_states, ["invalid_state", "also_invalid_state"]}} = machine_validator:validate_machine(
        #parsed_machine_config{
            initial = "subone",
            blank = ".",
            states = get_valid_states(),
            finals = ["invalid_state", "also_invalid_state"],
            alphabet = get_valid_alphabet(),
            transitions = get_valid_transitions_map()
        }
    ).

validate_machine_alphabet_error_test() ->
    {error, alphabet, {duplicated_elements, ["."]}} = machine_validator:validate_machine(
        #parsed_machine_config{
            initial = "subone",
            blank = ".",
            states = get_valid_states(),
            finals = ["HALT"],
            alphabet = get_valid_alphabet() ++ ["."],
            transitions = get_valid_transitions_map()
        }
    ).

validate_machine_transitions_error_test() ->
    {error, transitions, {"subone", {duplicated_elements, ["."]}}} = machine_validator:validate_machine(
        #parsed_machine_config{
            initial = "subone",
            blank = ".",
            states = get_valid_states(),
            finals = ["HALT"],
            alphabet = get_valid_alphabet(),
            transitions = #{
                "subone" => [
                    #parsed_machine_config_transition{
                        read = ".",
                        to_state = "scanright",
                        write = ".",
                        action = right
                    },
                    #parsed_machine_config_transition{
                        read = "-",
                        to_state = "subone",
                        write = "=",
                        action = left
                    }
                ],
                "scanright" => [
                    #parsed_machine_config_transition{
                        read = ".",
                        to_state = "scanright",
                        write = "=",
                        action = right
                    },
                    #parsed_machine_config_transition{
                        read = "=",
                        to_state = "subone",
                        write = ".",
                        action = left
                    }
                ],
                "subone" => [
                    #parsed_machine_config_transition{
                        read = ".",
                        to_state = "scanright",
                        write = ".",
                        action = right
                    },
                    #parsed_machine_config_transition{
                        read = ".",
                        to_state = "subone",
                        write = "=",
                        action = left
                    }
                ]
            }
        }
    ).
