-module(parser_tests).

-include("../src/machine.hrl").

-include_lib("eunit/include/eunit.hrl").

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
    {error, invalid} = parser:parse_machine_blank(#{<<"blank">> => "?"}).
parse_machine_blank_error_invalid_key_test() ->
    {error, invalid} = parser:parse_machine_blank(#{"blank" => <<"*">>}).
parse_machine_blank_error_empty_test() ->
    {error, empty} = parser:parse_machine_blank(#{<<"blank">> => <<"">>}).
parse_machine_blank_error_too_long_test() ->
    {error, too_long} = parser:parse_machine_blank(#{<<"blank">> => <<"..">>}).

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
