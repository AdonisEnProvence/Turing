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

% Machine States
parse_machine_states_test() ->
    {ok, ["add", "sub", "abs"]} = parser:parse_machine_states(#{
        <<"states">> => [<<"add">>, <<"sub">>, <<"abs">>]
    }).
parse_machine_states_error_invalid_state_test() ->
    {error, invalid_element} = parser:parse_machine_states(
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
    {error, empty_element} = parser:parse_machine_states(#{
        <<"states">> => [<<"add">>, <<"sub">>, <<"">>]
    }).
