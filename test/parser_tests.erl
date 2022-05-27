-module(parser_tests).

-include("../src/machine.hrl").

-include_lib("eunit/include/eunit.hrl").

get_raw_machine_config() ->
    #{
        <<"name">> => <<"unary_sub">>,
        <<"alphabet">> => [
            <<"1">>,
            <<".">>,
            <<"-">>,
            <<"=">>
        ],
        <<"blank">> => <<".">>,
        <<"states">> => [
            <<"scanright">>,
            <<"eraseone">>,
            <<"subone">>,
            <<"skip">>,
            <<"HALT">>
        ],
        <<"initial">> => <<"scanright">>,
        <<"finals">> => [
            <<"HALT">>
        ],
        <<"transitions">> => #{
            <<"add">> => [
                #{
                    <<"read">> => <<".">>,
                    <<"to_state">> => <<"scanright">>,
                    <<"write">> => <<".">>,
                    <<"action">> => <<"RIGHT">>
                },
                #{
                    <<"read">> => <<"?">>,
                    <<"to_state">> => <<"cocorico">>,
                    <<"write">> => <<"*">>,
                    <<"action">> => <<"LEFT">>
                }
            ],
            <<"sub">> => [
                #{
                    <<"read">> => <<".">>,
                    <<"to_state">> => <<"scanright">>,
                    <<"write">> => <<".">>,
                    <<"action">> => <<"RIGHT">>
                },
                #{
                    <<"read">> => <<"?">>,
                    <<"to_state">> => <<"cocorico">>,
                    <<"write">> => <<"*">>,
                    <<"action">> => <<"LEFT">>
                }
            ],
            <<"abs">> => [
                #{
                    <<"read">> => <<".">>,
                    <<"to_state">> => <<"scanright">>,
                    <<"write">> => <<".">>,
                    <<"action">> => <<"RIGHT">>
                },
                #{
                    <<"read">> => <<"?">>,
                    <<"to_state">> => <<"cocorico">>,
                    <<"write">> => <<"*">>,
                    <<"action">> => <<"LEFT">>
                }
            ]
        }
    }.

% Machine Name
parse_machine_name_test() ->
    {ok, "unary_add"} = parser:parse_machine_name(#{<<"name">> => <<"unary_add">>}).
parse_machine_name_error_invalid_test() ->
    {error, invalid} = parser:parse_machine_name(#{<<"name">> => "unary_add"}).
parse_machine_name_error_invalid_key_test() ->
    {error, invalid} = parser:parse_machine_name(#{"name" => <<"unary_add">>}).
parse_machine_name_error_empty_test() ->
    {error, empty} = parser:parse_machine_name(#{<<"name">> => <<"">>}).

% Machine Blank
parse_machine_blank_test() ->
    {ok, "."} = parser:parse_machine_blank(#{<<"blank">> => <<".">>}).
parse_machine_blank_error_invalid_test() ->
    {error, {expected_bitstring, "?"}} = parser:parse_machine_blank(#{<<"blank">> => "?"}).
parse_machine_blank_error_invalid_key_test() ->
    {error, invalid} = parser:parse_machine_blank(#{"blank" => <<"*">>}).
parse_machine_blank_error_empty_test() ->
    {error, empty_alphabet_character} = parser:parse_machine_blank(#{<<"blank">> => <<"">>}).
parse_machine_blank_error_too_long_test() ->
    {error, {too_long_alphabet_character, ".."}} = parser:parse_machine_blank(#{
        <<"blank">> => <<"..">>
    }).

% Initial State
parse_machine_initial_state_is_valid_test() ->
    {ok, "add"} = parser:parse_machine_initial_state(#{
        <<"initial">> => <<"add">>
    }).
parse_machine_initial_state_is_not_bitstring_test() ->
    {error, {expected_bitstring, 2}} = parser:parse_machine_initial_state(#{
        <<"initial">> => 2
    }).
parse_machine_initial_state_key_does_not_exist_test() ->
    {error, invalid} = parser:parse_machine_initial_state(#{}).
parse_machine_initial_state_is_empty_bitstring_test() ->
    {error, empty_state} = parser:parse_machine_initial_state(#{
        <<"initial">> => <<"">>
    }).

% Machine States
parse_machine_states_test() ->
    {ok, ["add", "sub", "abs"]} = parser:parse_machine_states(#{
        <<"states">> => [<<"add">>, <<"sub">>, <<"abs">>]
    }).
parse_machine_states_error_invalid_state_test() ->
    {error, {expected_bitstring, "sub"}} = parser:parse_machine_states(
        (#{<<"states">> => [<<"add">>, "sub", <<"abs">>]})
    ).
parse_machine_states_error_invalid_key_test() ->
    {error, invalid} = parser:parse_machine_states(#{
        "states" => [<<"add">>, <<"sub">>, <<"abs">>]
    }).
parse_machine_states_error_empty_list_test() ->
    {error, empty_list} = parser:parse_machine_states(#{
        <<"states">> => []
    }).
parse_machine_states_error_empty_state_test() ->
    {error, empty_state} = parser:parse_machine_states(#{
        <<"states">> => [<<"add">>, <<"sub">>, <<"">>]
    }).

% Machine Finals
parse_machine_finals_test() ->
    {ok, ["add", "sub", "abs"]} = parser:parse_machine_finals(#{
        <<"finals">> => [<<"add">>, <<"sub">>, <<"abs">>]
    }).
parse_machine_finals_error_invalid_state_test() ->
    {error, {expected_bitstring, "sub"}} = parser:parse_machine_finals(
        (#{<<"finals">> => [<<"add">>, "sub", <<"abs">>]})
    ).
parse_machine_finals_error_invalid_key_test() ->
    {error, invalid} = parser:parse_machine_finals(#{
        "finals" => [<<"add">>, <<"sub">>, <<"abs">>]
    }).
parse_machine_finals_empty_list_test() ->
    {ok, []} = parser:parse_machine_finals(#{
        <<"finals">> => []
    }).
parse_machine_finals_error_empty_state_test() ->
    {error, empty_state} = parser:parse_machine_finals(#{
        <<"finals">> => [<<"add">>, <<"sub">>, <<"">>]
    }).

% Machine transitions
parse_machine_error_invalid_transitions_map_test() ->
    {error, invalid} = parser:parse_machine_transitions(#{}).

parse_machine_transitions_test() ->
    RawTransitions = #{
        <<"add">> => [
            #{
                <<"read">> => <<".">>,
                <<"to_state">> => <<"scanright">>,
                <<"write">> => <<".">>,
                <<"action">> => <<"RIGHT">>
            },
            #{
                <<"read">> => <<"?">>,
                <<"to_state">> => <<"cocorico">>,
                <<"write">> => <<"*">>,
                <<"action">> => <<"LEFT">>
            }
        ],
        <<"sub">> => [
            #{
                <<"read">> => <<".">>,
                <<"to_state">> => <<"scanright">>,
                <<"write">> => <<".">>,
                <<"action">> => <<"RIGHT">>
            },
            #{
                <<"read">> => <<"?">>,
                <<"to_state">> => <<"cocorico">>,
                <<"write">> => <<"*">>,
                <<"action">> => <<"LEFT">>
            }
        ],
        <<"abs">> => [
            #{
                <<"read">> => <<".">>,
                <<"to_state">> => <<"scanright">>,
                <<"write">> => <<".">>,
                <<"action">> => <<"RIGHT">>
            },
            #{
                <<"read">> => <<"?">>,
                <<"to_state">> => <<"cocorico">>,
                <<"write">> => <<"*">>,
                <<"action">> => <<"LEFT">>
            }
        ]
    },
    ExpectedParsedTransitionsResult = #{
        "abs" => [
            #parsed_machine_config_transition{
                read = ".",
                to_state = "scanright",
                write = ".",
                action = right
            },
            #parsed_machine_config_transition{
                read = "?",
                to_state = "cocorico",
                write = "*",
                action = left
            }
        ],
        "add" => [
            #parsed_machine_config_transition{
                read = ".",
                to_state = "scanright",
                write = ".",
                action = right
            },
            #parsed_machine_config_transition{
                read = "?",
                to_state = "cocorico",
                write = "*",
                action = left
            }
        ],
        "sub" => [
            #parsed_machine_config_transition{
                read = ".",
                to_state = "scanright",
                write = ".",
                action = right
            },
            #parsed_machine_config_transition{
                read = "?",
                to_state = "cocorico",
                write = "*",
                action = left
            }
        ]
    },
    {ok, ExpectedParsedTransitionsResult} = parser:parse_machine_transitions(#{
        <<"transitions">> => RawTransitions
    }).

% Read transition prop tests

parse_machine_transitions_state_is_not_bitstring_test() ->
    RawTransitions = #{
        <<"add">> => [
            #{
                <<"read">> => <<".">>,
                <<"to_state">> => <<"scanright">>,
                <<"write">> => <<".">>,
                <<"action">> => <<"RIGHT">>
            },
            #{
                <<"read">> => <<".">>,
                <<"to_state">> => <<"cocorico">>,
                <<"write">> => <<"*">>,
                <<"action">> => <<"LEFT">>
            }
        ],
        "sub" => [
            #{
                <<"read">> => <<".">>,
                <<"to_state">> => <<"scanright">>,
                <<"write">> => <<".">>,
                <<"action">> => <<"RIGHT">>
            },
            #{
                <<"read">> => <<".">>,
                <<"to_state">> => <<"cocorico">>,
                <<"write">> => <<"*">>,
                <<"action">> => <<"LEFT">>
            }
        ]
    },

    {error, {expected_state_bitstring, "sub"}} = parser:parse_machine_transitions(
        #{
            <<"transitions">> => RawTransitions
        }
    ).

parse_machine_transitions_read_value_is_not_bitstring_test() ->
    RawTransitions = #{
        <<"add">> => [
            #{
                <<"read">> => ".",
                <<"to_state">> => <<"scanright">>,
                <<"write">> => <<".">>,
                <<"action">> => <<"RIGHT">>
            },
            #{
                <<"read">> => <<"?">>,
                <<"to_state">> => <<"cocorico">>,
                <<"write">> => <<"*">>,
                <<"action">> => <<"LEFT">>
            }
        ]
    },

    {error, {0, read, {expected_bitstring, "."}}} = parser:parse_machine_transitions(#{
        <<"transitions">> => RawTransitions
    }).

parse_machine_transitions_read_is_too_long_alphabet_character_test() ->
    RawTransitions = #{
        <<"add">> => [
            #{
                <<"read">> => <<".">>,
                <<"to_state">> => <<"scanright">>,
                <<"write">> => <<".">>,
                <<"action">> => <<"RIGHT">>
            },
            #{
                <<"read">> => <<"......">>,
                <<"to_state">> => <<"cocorico">>,
                <<"write">> => <<"*">>,
                <<"action">> => <<"LEFT">>
            }
        ]
    },

    {error, {1, read, {too_long_alphabet_character, "......"}}} = parser:parse_machine_transitions(
        #{
            <<"transitions">> => RawTransitions
        }
    ).

parse_machine_transitions_read_empty_alphabet_character_test() ->
    RawTransitions = #{
        <<"add">> => [
            #{
                <<"read">> => <<".">>,
                <<"to_state">> => <<"scanright">>,
                <<"write">> => <<".">>,
                <<"action">> => <<"RIGHT">>
            },
            #{
                <<"read">> => <<".">>,
                <<"to_state">> => <<"cocorico">>,
                <<"write">> => <<"*">>,
                <<"action">> => <<"LEFT">>
            }
        ],
        <<"sub">> => [
            #{
                <<"read">> => <<"">>,
                <<"to_state">> => <<"scanright">>,
                <<"write">> => <<".">>,
                <<"action">> => <<"RIGHT">>
            },
            #{
                <<"read">> => <<".">>,
                <<"to_state">> => <<"cocorico">>,
                <<"write">> => <<"*">>,
                <<"action">> => <<"LEFT">>
            }
        ]
    },

    {error, {0, read, empty_alphabet_character}} = parser:parse_machine_transitions(
        #{
            <<"transitions">> => RawTransitions
        }
    ).

parse_machine_transitions_read_property_not_found_test() ->
    RawTransitions = #{
        <<"add">> => [
            #{
                <<"read">> => <<".">>,
                <<"to_state">> => <<"scanright">>,
                <<"write">> => <<".">>,
                <<"action">> => <<"RIGHT">>
            },
            #{
                <<"read">> => <<".">>,
                <<"to_state">> => <<"cocorico">>,
                <<"write">> => <<"*">>,
                <<"action">> => <<"LEFT">>
            }
        ],
        <<"sub">> => [
            #{
                <<"read">> => <<".">>,
                <<"to_state">> => <<"scanright">>,
                <<"write">> => <<".">>,
                <<"action">> => <<"RIGHT">>
            },
            #{
                <<"to_state">> => <<"cocorico">>,
                <<"write">> => <<"*">>,
                <<"action">> => <<"LEFT">>
            }
        ]
    },

    {error, {1, read, no_entry}} = parser:parse_machine_transitions(
        #{
            <<"transitions">> => RawTransitions
        }
    ).

parse_machine_transitions_read_key_is_not_bitstring_test() ->
    RawTransitions = #{
        <<"add">> => [
            #{
                <<"read">> => <<".">>,
                <<"to_state">> => <<"scanright">>,
                <<"write">> => <<".">>,
                <<"action">> => <<"RIGHT">>
            },
            #{
                <<"read">> => <<".">>,
                <<"to_state">> => <<"cocorico">>,
                <<"write">> => <<"*">>,
                <<"action">> => <<"LEFT">>
            }
        ],
        <<"sub">> => [
            #{
                "read" => <<".">>,
                <<"to_state">> => <<"scanright">>,
                <<"write">> => <<".">>,
                <<"action">> => <<"RIGHT">>
            },
            #{
                <<"read">> => <<"">>,
                <<"to_state">> => <<"cocorico">>,
                <<"write">> => <<"*">>,
                <<"action">> => <<"LEFT">>
            }
        ]
    },

    {error, {0, read, no_entry}} = parser:parse_machine_transitions(
        #{
            <<"transitions">> => RawTransitions
        }
    ).

% write transition prop tests

parse_machine_transitions_write_value_is_not_bitstring_test() ->
    RawTransitions = #{
        <<"add">> => [
            #{
                <<"read">> => <<".">>,
                <<"to_state">> => <<"scanright">>,
                <<"write">> => ".",
                <<"action">> => <<"RIGHT">>
            },
            #{
                <<"read">> => <<"?">>,
                <<"to_state">> => <<"cocorico">>,
                <<"write">> => <<"*">>,
                <<"action">> => <<"LEFT">>
            }
        ]
    },

    {error, {0, write, {expected_bitstring, "."}}} = parser:parse_machine_transitions(#{
        <<"transitions">> => RawTransitions
    }).

parse_machine_transitions_write_is_too_long_alphabet_character_test() ->
    RawTransitions = #{
        <<"add">> => [
            #{
                <<"read">> => <<".">>,
                <<"to_state">> => <<"scanright">>,
                <<"write">> => <<".">>,
                <<"action">> => <<"RIGHT">>
            },
            #{
                <<"read">> => <<".">>,
                <<"to_state">> => <<"cocorico">>,
                <<"write">> => <<"*****">>,
                <<"action">> => <<"LEFT">>
            }
        ]
    },

    {error, {1, write, {too_long_alphabet_character, "*****"}}} = parser:parse_machine_transitions(
        #{
            <<"transitions">> => RawTransitions
        }
    ).

parse_machine_transitions_write_empty_alphabet_character_test() ->
    RawTransitions = #{
        <<"add">> => [
            #{
                <<"read">> => <<".">>,
                <<"to_state">> => <<"scanright">>,
                <<"write">> => <<".">>,
                <<"action">> => <<"RIGHT">>
            },
            #{
                <<"read">> => <<".">>,
                <<"to_state">> => <<"cocorico">>,
                <<"write">> => <<"*">>,
                <<"action">> => <<"LEFT">>
            }
        ],
        <<"sub">> => [
            #{
                <<"read">> => <<".">>,
                <<"to_state">> => <<"scanright">>,
                <<"write">> => <<"">>,
                <<"action">> => <<"RIGHT">>
            },
            #{
                <<"read">> => <<".">>,
                <<"to_state">> => <<"cocorico">>,
                <<"write">> => <<"*">>,
                <<"action">> => <<"LEFT">>
            }
        ]
    },

    {error, {0, write, empty_alphabet_character}} = parser:parse_machine_transitions(
        #{
            <<"transitions">> => RawTransitions
        }
    ).

parse_machine_transitions_write_property_not_found_test() ->
    RawTransitions = #{
        <<"add">> => [
            #{
                <<"read">> => <<".">>,
                <<"to_state">> => <<"scanright">>,
                <<"write">> => <<".">>,
                <<"action">> => <<"RIGHT">>
            },
            #{
                <<"read">> => <<".">>,
                <<"to_state">> => <<"cocorico">>,
                <<"write">> => <<"*">>,
                <<"action">> => <<"LEFT">>
            }
        ],
        <<"sub">> => [
            #{
                <<"read">> => <<".">>,
                <<"to_state">> => <<"scanright">>,
                <<"write">> => <<".">>,
                <<"action">> => <<"RIGHT">>
            },
            #{
                <<"read">> => <<".">>,
                <<"to_state">> => <<"cocorico">>,
                <<"action">> => <<"LEFT">>
            }
        ]
    },

    {error, {1, write, no_entry}} = parser:parse_machine_transitions(
        #{
            <<"transitions">> => RawTransitions
        }
    ).

parse_machine_transitions_write_key_is_not_bitstring_test() ->
    RawTransitions = #{
        <<"add">> => [
            #{
                <<"read">> => <<".">>,
                <<"to_state">> => <<"scanright">>,
                <<"write">> => <<".">>,
                <<"action">> => <<"RIGHT">>
            },
            #{
                <<"read">> => <<".">>,
                <<"to_state">> => <<"cocorico">>,
                <<"write">> => <<"*">>,
                <<"action">> => <<"LEFT">>
            }
        ],
        <<"sub">> => [
            #{
                <<"read">> => <<".">>,
                <<"to_state">> => <<"scanright">>,
                "write" => <<".">>,
                <<"action">> => <<"RIGHT">>
            },
            #{
                <<"read">> => <<"">>,
                <<"to_state">> => <<"cocorico">>,
                <<"write">> => <<"*">>,
                <<"action">> => <<"LEFT">>
            }
        ]
    },

    {error, {0, write, no_entry}} = parser:parse_machine_transitions(
        #{
            <<"transitions">> => RawTransitions
        }
    ).

% transition actions tests

parse_machine_transitions_action_is_missing_test() ->
    RawTransitions = #{
        <<"add">> => [
            #{
                <<"read">> => <<".">>,
                <<"to_state">> => <<"scanright">>,
                <<"write">> => <<".">>
            }
        ]
    },

    {error, {0, action, no_entry}} = parser:parse_machine_transitions(#{
        <<"transitions">> => RawTransitions
    }).
parse_machine_transitions_action_is_empty_test() ->
    RawTransitions = #{
        <<"add">> => [
            #{
                <<"read">> => <<".">>,
                <<"to_state">> => <<"scanright">>,
                <<"write">> => <<".">>,
                <<"action">> => <<"">>
            }
        ]
    },

    {error, {0, action, {unknown_action, <<"">>}}} = parser:parse_machine_transitions(#{
        <<"transitions">> => RawTransitions
    }).
parse_machine_transitions_action_is_invalid_bitstring_test() ->
    RawTransitions = #{
        <<"add">> => [
            #{
                <<"read">> => <<".">>,
                <<"to_state">> => <<"scanright">>,
                <<"write">> => <<".">>,
                <<"action">> => 2
            }
        ]
    },

    {error, {0, action, {unknown_action, 2}}} = parser:parse_machine_transitions(#{
        <<"transitions">> => RawTransitions
    }).
parse_machine_transitions_action_is_not_known_test() ->
    RawTransitions = #{
        <<"add">> => [
            #{
                <<"read">> => <<".">>,
                <<"to_state">> => <<"scanright">>,
                <<"write">> => <<".">>,
                <<"action">> => <<"MIDDLE">>
            }
        ]
    },

    {error, {0, action, {unknown_action, <<"MIDDLE">>}}} = parser:parse_machine_transitions(#{
        <<"transitions">> => RawTransitions
    }).
parse_machine_transitions_action_is_left_test() ->
    RawTransitions = #{
        <<"add">> => [
            #{
                <<"read">> => <<".">>,
                <<"to_state">> => <<"scanright">>,
                <<"write">> => <<".">>,
                <<"action">> => <<"LEFT">>
            }
        ]
    },

    {ok, _} = parser:parse_machine_transitions(#{
        <<"transitions">> => RawTransitions
    }).
parse_machine_transitions_action_is_right_test() ->
    RawTransitions = #{
        <<"add">> => [
            #{
                <<"read">> => <<".">>,
                <<"to_state">> => <<"scanright">>,
                <<"write">> => <<".">>,
                <<"action">> => <<"RIGHT">>
            }
        ]
    },

    {ok, _} = parser:parse_machine_transitions(#{
        <<"transitions">> => RawTransitions
    }).

% Tests for to_state

parse_machine_transitions_to_state_is_missing_test() ->
    RawTransitions = #{
        <<"add">> => [
            #{
                <<"read">> => <<".">>,
                <<"write">> => <<".">>,
                <<"action">> => <<"RIGHT">>
            }
        ]
    },

    {error, {0, to_state, no_entry}} = parser:parse_machine_transitions(#{
        <<"transitions">> => RawTransitions
    }).
parse_machine_transitions_to_state_is_empty_test() ->
    RawTransitions = #{
        <<"add">> => [
            #{
                <<"read">> => <<".">>,
                <<"write">> => <<".">>,
                <<"to_state">> => <<"">>,
                <<"action">> => <<"RIGHT">>
            }
        ]
    },

    {error, {0, to_state, empty_state}} = parser:parse_machine_transitions(#{
        <<"transitions">> => RawTransitions
    }).
parse_machine_transitions_to_state_is_invalid_bitstring_test() ->
    RawTransitions = #{
        <<"add">> => [
            #{
                <<"read">> => <<".">>,
                <<"write">> => <<".">>,
                <<"to_state">> => 2,
                <<"action">> => <<"RIGHT">>
            }
        ]
    },

    {error, {0, to_state, {expected_bitstring, 2}}} = parser:parse_machine_transitions(#{
        <<"transitions">> => RawTransitions
    }).
parse_machine_transitions_to_state_is_valid_test() ->
    RawTransitions = #{
        <<"add">> => [
            #{
                <<"read">> => <<".">>,
                <<"write">> => <<".">>,
                <<"to_state">> => <<"add">>,
                <<"action">> => <<"RIGHT">>
            }
        ]
    },

    {ok, _} = parser:parse_machine_transitions(#{
        <<"transitions">> => RawTransitions
    }).

%Machine alphabet tests

parse_machine_alphabet_test() -> 
    RawAlphabet = #{
        <<"alphabet">> => [
            <<"a">>, <<"b">>, <<"c">>, <<"d">>, <<"e">>, <<"f">>]
        },
    {ok, ["a", "b", "c", "d", "e", "f"]} = parser:parse_machine_alphabet(RawAlphabet).
parse_machine_alphabet_no_entry_test() -> 
    RawAlphabet = #{
        "alphabet" => [
            <<"a">>, <<"b">>, <<"c">>, <<"d">>, <<"e">>, <<"f">>]
        },
    {error, no_entry} = parser:parse_machine_alphabet(RawAlphabet).
parse_machine_alphabet_too_long_character_test() -> 
    RawAlphabet = #{
        <<"alphabet">> => [
            <<"a">>, <<"b">>, <<"ccccc">>, <<"d">>, <<"e">>, <<"f">>]
        },
    {error, {too_long_alphabet_character, "ccccc"}} = parser:parse_machine_alphabet(RawAlphabet).
parse_machine_alphabet_empty_test() -> 
    RawAlphabet = #{
        <<"alphabet">> => [
            <<"a">>, <<"b">>, <<"c">>, <<"d">>, <<"">>, <<"f">>]
        },
    {error, empty_alphabet_character} = parser:parse_machine_alphabet(RawAlphabet).
parse_machine_alphabet_empty_list_test() -> 
    RawAlphabet = #{
        <<"alphabet">> => []
        },
    {error, empty_list} = parser:parse_machine_alphabet(RawAlphabet).
parse_machine_alphabet_expected_bitstring_test() -> 
    RawAlphabet = #{
        <<"alphabet">> => [
            <<"a">>, "b", <<"c">>, <<"d">>, <<"">>, <<"f">>]
        },
    {error, {expected_bitstring, "b"}} = parser:parse_machine_alphabet(RawAlphabet).


%Machine whole parser tests
parse_machine_parses_valid_machine_test() ->
    ExpectedParsedMachineConfig = #parsed_machine_config{
        name="unary_sub",
        alphabet=[
            "1",
            ".",
            "-",
            "="
        ],
        blank=".",
        states=[
            "scanright",
            "eraseone",
            "subone",
            "skip",
            "HALT"
        ],
        initial="scanright",
        finals=[
            "HALT"
        ],
        transitions=#{
            "abs" => [
                #parsed_machine_config_transition{
                    read = ".",
                    to_state = "scanright",
                    write = ".",
                    action = right
                },
                #parsed_machine_config_transition{
                    read = "?",
                    to_state = "cocorico",
                    write = "*",
                    action = left
                }
            ],
            "add" => [
                #parsed_machine_config_transition{
                    read = ".",
                    to_state = "scanright",
                    write = ".",
                    action = right
                },
                #parsed_machine_config_transition{
                    read = "?",
                    to_state = "cocorico",
                    write = "*",
                    action = left
                }
            ],
            "sub" => [
                #parsed_machine_config_transition{
                    read = ".",
                    to_state = "scanright",
                    write = ".",
                    action = right
                },
                #parsed_machine_config_transition{
                    read = "?",
                    to_state = "cocorico",
                    write = "*",
                    action = left
                }
            ]
        }
    },
    RawMachineConfig = get_raw_machine_config(),
    {ok, ExpectedParsedMachineConfig} = parser:parse_machine(RawMachineConfig).

parse_machine_name_error_test() ->
    RawMachineConfig = maps:update(<<"name">>, <<"">>, get_raw_machine_config()),
    {error, name, empty} = parser:parse_machine(RawMachineConfig).

parse_machine_blank_error_test() ->
    RawMachineConfig = maps:update(<<"blank">>, <<"wwww">>, get_raw_machine_config()),
    {error, blank, {too_long_alphabet_character, "wwww"}} = parser:parse_machine(RawMachineConfig).

parse_machine_initial_error_test() ->
    RawMachineConfig = maps:remove(<<"initial">>, get_raw_machine_config()),
    {error, initial, invalid} = parser:parse_machine(RawMachineConfig).

parse_machine_alphabet_error_test() ->
    RawMachineConfig = maps:update(<<"alphabet">>, [], get_raw_machine_config()),
    {error, alphabet, empty_list} = parser:parse_machine(RawMachineConfig).

parse_machine_states_error_test() ->
    RawMachineConfig = maps:update(<<"states">>, [<<"word">>, 42, <<"cocorico">>], get_raw_machine_config()),
    {error, states, {expected_bitstring, 42}} = parser:parse_machine(RawMachineConfig).

parse_machine_finals_error_test() ->
    RawMachineConfig = maps:update(<<"finals">>, [atom], get_raw_machine_config()),
    {error, finals, {expected_bitstring, atom}} = parser:parse_machine(RawMachineConfig).

parse_machine_transitions_error_test() ->
    RawMachineConfig = maps:update(<<"transitions">>, #{
            <<"add">> => [
                #{
                    <<"read">> => <<".">>,
                    <<"to_state">> => <<"scanright">>,
                    <<"write">> => <<".">>,
                    <<"action">> => <<"RIGHT">>
                },
                #{
                    <<"read">> => <<"?">>,
                    <<"to_state">> => <<"cocorico">>,
                    <<"write">> => <<"*">>,
                    <<"action">> => <<"LEFT">>
                }
            ],
            <<"sub">> => [
                #{
                    <<"read">> => <<".">>,
                    <<"to_state">> => <<"scanright">>,
                    <<"write">> => <<".">>,
                    <<"action">> => <<"RIGHT">>
                },
                #{
                    <<"read">> => <<"?">>,
                    <<"to_state">> => <<"cocorico">>,
                    <<"write">> => <<"*">>,
                    <<"action">> => <<"LEFT">>
                }
            ],
            "abs" => [
                #{
                    <<"read">> => <<".">>,
                    <<"to_state">> => <<"scanright">>,
                    <<"write">> => <<".">>,
                    <<"action">> => <<"RIGHT">>
                },
                #{
                    <<"read">> => <<"?">>,
                    <<"to_state">> => <<"cocorico">>,
                    <<"write">> => <<"*">>,
                    <<"action">> => <<"LEFT">>
                }
            ]
        }, get_raw_machine_config()),
    {error, transitions, {expected_state_bitstring, "abs"}} = parser:parse_machine(RawMachineConfig).
