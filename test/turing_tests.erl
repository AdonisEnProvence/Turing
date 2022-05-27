-module(turing_tests).

-include("../src/machine.hrl").

-include_lib("eunit/include/eunit.hrl").


parse_optionnal_first_flag_arg_with_h_flag_test() ->
    exit = turing:parse_optionnal_first_flag_arg(["-h"]).
parse_optionnal_first_flag_arg_with_help_flag_test() ->
    exit = turing:parse_optionnal_first_flag_arg(["--help"]).
parse_optionnal_first_flag_arg_with_valid_args_and_help_flag_test() ->
    exit = turing:parse_optionnal_first_flag_arg(["--help", "myFile.json", "010101"]).
parse_optionnal_first_flag_arg_with_valid_args_and_h_flag_test() ->
    exit = turing:parse_optionnal_first_flag_arg(["-h", "myFile.json", "010101"]).
parse_optionnal_first_flag_arg_empty_args_test() ->
    {error, empty_args} = turing:parse_optionnal_first_flag_arg([]).
parse_optionnal_first_flag_arg_too_many_args_test() ->
    {error, too_many_args} = turing:parse_optionnal_first_flag_arg(["myFile.json", "010101", "--help"]).
parse_optionnal_first_flag_arg_test() ->
    ok = turing:parse_optionnal_first_flag_arg(["myFile.json", "010101"]).
