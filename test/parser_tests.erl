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
parse_machine_name_is_not_bitstring_test() ->
    {error, {expected_bitstring, 2}} = parser:parse_machine_name(#{<<"name">> => 2}).
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

parse_machine_transitions_state_key_is_empty_test() ->
    RawTransitions = #{
        <<"">> => [
            #{
                <<"read">> => <<".">>,
                <<"to_state">> => <<"scanright">>,
                <<"write">> => <<".">>,
                <<"action">> => <<"">>
            }
        ]
    },

    {error, empty_state_key} = parser:parse_machine_transitions(#{
        <<"transitions">> => RawTransitions
    }).

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

    {error, {"add", 0, read, {expected_bitstring, "."}}} = parser:parse_machine_transitions(#{
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

    {error, {"add", 1, read, {too_long_alphabet_character, "......"}}} = parser:parse_machine_transitions(
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

    {error, {"sub", 0, read, empty_alphabet_character}} = parser:parse_machine_transitions(
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

    {error, {"sub", 1, read, no_entry}} = parser:parse_machine_transitions(
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

    {error, {"sub", 0, read, no_entry}} = parser:parse_machine_transitions(
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
                <<"write">> => 2,
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

    {error, {"add", 0, write, {expected_bitstring, 2}}} = parser:parse_machine_transitions(#{
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

    {error, {"add", 1, write, {too_long_alphabet_character, "*****"}}} = parser:parse_machine_transitions(
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

    {error, {"sub", 0, write, empty_alphabet_character}} = parser:parse_machine_transitions(
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

    {error, {"sub", 1, write, no_entry}} = parser:parse_machine_transitions(
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

    {error, {"sub", 0, write, no_entry}} = parser:parse_machine_transitions(
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

    {error, {"add", 0, action, no_entry}} = parser:parse_machine_transitions(#{
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

    {error, {"add", 0, action, {unknown_action, <<"">>}}} = parser:parse_machine_transitions(#{
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

    {error, {"add", 0, action, {unknown_action, 2}}} = parser:parse_machine_transitions(#{
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

    {error, {"add", 0, action, {unknown_action, <<"MIDDLE">>}}} = parser:parse_machine_transitions(#{
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

    {error, {"add", 0, to_state, no_entry}} = parser:parse_machine_transitions(#{
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

    {error, {"add", 0, to_state, empty_state}} = parser:parse_machine_transitions(#{
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

    {error, {"add", 0, to_state, {expected_bitstring, 2}}} = parser:parse_machine_transitions(#{
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

format_error_name_is_empty_test() ->
    ?assertMatch("machine name is empty; a machine must have a name of at least one character", parser:format_error({name, empty})).
format_error_name_is_not_a_string_test() ->
    ?assertMatch("machine name is not a string (received: {\"key\":2}); a machine must have a name of at least one character", parser:format_error({name, {expected_bitstring, #{<<"key">> => 2}}})).
format_error_name_does_not_exist_test() ->
    ?assertMatch("machine has no name; a machine must have a name of at least one character", parser:format_error({name, invalid})).

format_error_blank_is_empty_test() ->
    ?assertMatch("machine blank character is empty; a machine must have a blank character exactly made of one character", parser:format_error({blank, empty_alphabet_character})).
format_error_blank_is_not_a_string_test() ->
    ?assertMatch("machine blank character is not a string (received: {\"key\":2}); a machine must have a blank character exactly made of one character", parser:format_error({blank, {expected_bitstring, #{<<"key">> => 2}}})).
format_error_blank_does_not_exist_test() ->
    ?assertMatch("machine has no blank character; a machine must have a blank character exactly made of one character", parser:format_error({blank, invalid})).
format_error_blank_is_too_long_test() ->
    ?assertMatch("machine blank character is too long (received: yolo); a machine must have a blank character exactly made of one character", parser:format_error({blank, {too_long_alphabet_character, "yolo"}})).

format_error_initial_state_is_not_a_string_test() ->
    ?assertMatch("machine initial state is not a string (received: {\"key\":2}); a machine must have a non-empty initial state", parser:format_error({initial, {expected_bitstring, #{<<"key">> => 2}}})).
format_error_initial_state_does_not_exist_test() ->
    ?assertMatch("machine has no initial state; a machine must have a non-empty initial state", parser:format_error({initial, invalid})).
format_error_initial_state_is_empty_test() ->
    ?assertMatch("machine initial state is empty; a machine must have a non-empty initial state", parser:format_error({initial, empty_state})).

format_error_states_a_state_is_not_a_string_test() ->
    ?assertMatch("machine has a state that is not a string ({\"key\":2}); a machine must have a non-empty list of states, which must all be non-empty strings", parser:format_error({states, {expected_bitstring, #{<<"key">> => 2}}})).
format_error_states_a_state_is_empty_test() ->
    ?assertMatch("machine has an empty state; a machine must have a non-empty list of states, which must all be non-empty strings", parser:format_error({states, empty_state})).
format_error_states_does_not_exist_test() ->
    ?assertMatch("machine has no states; a machine must have a non-empty list of states, which must all be non-empty strings", parser:format_error({states, invalid})).
format_error_states_is_empty_test() ->
    ?assertMatch("machine has an empty list of states; a machine must have a non-empty list of states, which must all be non-empty strings", parser:format_error({states, empty_list})).

format_error_finals_a_state_is_not_a_string_test() ->
    ?assertMatch("machine has a final state that is not a string ({\"key\":2}); a machine must have a list of states (optionally empty), which must all be non-empty strings", parser:format_error({finals, {expected_bitstring, #{<<"key">> => 2}}})).
format_error_finals_a_state_is_empty_test() ->
    ?assertMatch("machine has an empty final state; a machine must have a list of states (optionally empty), which must all be non-empty strings", parser:format_error({finals, empty_state})).
format_error_finals_does_not_exist_test() ->
    ?assertMatch("machine has no final states; a machine must have a list of states (optionally empty), which must all be non-empty strings", parser:format_error({finals, invalid})).

format_error_transitions_is_empty_test() ->
    % The message is not accurate.
    ?assertMatch("machine has an empty transitions object; a machine must contain at least one transition", parser:format_error({transitions, invalid})).
format_error_transitions_a_state_is_not_a_string_test() ->
    ?assertMatch("machine contains transitions for a state that is not a valid string ({\"key\":2}); a machine can only contain transitions for valid states, which must be non-empty strings", parser:format_error({transitions, {expected_state_bitstring, #{<<"key">> => 2}}})).
format_error_transitions_a_state_is_an_empty_string_test() ->
    ?assertMatch("machine contains transitions for a state that is an empty string; a machine can only contain transitions for valid states, which must be non-empty strings", parser:format_error({transitions, empty_state_key})).
format_error_transitions_read_property_of_a_transition_is_empty_test() ->
    ?assertMatch("transition 0 for state add has its read property that is empty; each transition must have a read property that is a string with exactly one character", parser:format_error({transitions, {"add", 0, read, empty_alphabet_character}})).
format_error_transitions_read_property_of_a_transition_is_not_a_string_test() ->
    ?assertMatch("transition 0 for state add has its read property that is not a string (2); each transition must have a read property that is a string with exactly one character", parser:format_error({transitions, {"add", 0, read, {expected_bitstring, 2}}})).
format_error_transitions_read_property_of_a_transition_is_longer_than_one_character_test() ->
    ?assertMatch("transition 0 for state add has its read property that is too long (aaa); each transition must have a read property that is a string with exactly one character", parser:format_error({transitions, {"add", 0, read, {too_long_alphabet_character, "aaa"}}})).
format_error_transitions_read_property_is_missing_test() ->
    ?assertMatch("transition 0 for state add does not have a read property; each transition must have a read property that is a string with exactly one character", parser:format_error({transitions, {"add", 0, read, no_entry}})).
format_error_transitions_write_property_of_a_transition_is_empty_test() ->
    ?assertMatch("transition 0 for state add has its write property that is empty; each transition must have a write property that is a string with exactly one character", parser:format_error({transitions, {"add", 0, write, empty_alphabet_character}})).
format_error_transitions_write_property_of_a_transition_is_not_a_string_test() ->
    ?assertMatch("transition 0 for state add has its write property that is not a string (2); each transition must have a write property that is a string with exactly one character", parser:format_error({transitions, {"add", 0, write, {expected_bitstring, 2}}})).
format_error_transitions_write_property_of_a_transition_is_longer_than_one_character_test() ->
    ?assertMatch("transition 0 for state add has its write property that is too long (aaa); each transition must have a write property that is a string with exactly one character", parser:format_error({transitions, {"add", 0, write, {too_long_alphabet_character, "aaa"}}})).
format_error_transitions_write_property_is_missing_test() ->
    ?assertMatch("transition 0 for state add does not have a write property; each transition must have a write property that is a string with exactly one character", parser:format_error({transitions, {"add", 0, write, no_entry}})).
format_error_transitions_to_state_property_is_not_a_string_test() ->
    ?assertMatch("transition 0 for state add has its to_state property that is not a string ({\"key\":2}); each transition must have a to_state property that is a non-empty string", parser:format_error({transitions, {"add", 0, to_state, {expected_bitstring, #{<<"key">> => 2}}}})).
format_error_transitions_to_state_property_does_not_exist_test() ->
    ?assertMatch("transition 0 for state add does not have a to_state property; each transition must have a to_state property that is a non-empty string", parser:format_error({transitions, {"add", 0, to_state, no_entry}})).
format_error_transitions_to_state_property_is_empty_test() ->
    ?assertMatch("transition 0 for state add has its to_state property that is empty; each transition must have a to_state property that is a non-empty string", parser:format_error({transitions, {"add", 0, to_state, empty_state}})).
format_error_transitions_action_property_is_unknown_test() ->
    ?assertMatch("transition 0 for state add has an unknown action property (\"trompinette\"); each transition must have an action property that is either \"LEFT\" OR \"RIGHT\"", parser:format_error({transitions, {"add", 0, action, {unknown_action, <<"trompinette">>}}})).
format_error_transitions_action_property_is_missing_test() ->
    ?assertMatch("transition 0 for state add does not have an action property; each transition must have an action property that is either \"LEFT\" OR \"RIGHT\"", parser:format_error({transitions, {"add", 0, action, no_entry}})).
