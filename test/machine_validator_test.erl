-module(machine_validator_test).

-include_lib("eunit/include/eunit.hrl").

% Validate Alphabet
validate_machine_alphabet_test() ->
    ok = machine_validator:validate_machine_alphabet([
        "1",
        ".",
        "-",
        "="
    ]).
parse_machine_alphabet_duplicated_entry_test() ->
    {error, {duplicated_characters, ["="]}} = machine_validator:validate_machine_alphabet([
        "1",
        ".",
        "=",
        "-",
        "="
    ]).
parse_machine_alphabet_several_duplicated_entry_test() ->
    {error, {duplicated_characters, [".", "="]}} = machine_validator:validate_machine_alphabet([
        "1",
        ".",
        "=",
        ".",
        "-",
        "="
    ]).

% Validate States
validate_machine_states_test() ->
    ok = machine_validator:validate_machine_states([
        "1",
        ".",
        "-",
        "="
    ]).
parse_machine_states_duplicated_entry_test() ->
    {error, {duplicated_characters, ["="]}} = machine_validator:validate_machine_states([
        "1",
        ".",
        "=",
        "-",
        "="
    ]).
parse_machine_states_several_duplicated_entry_test() ->
    {error, {duplicated_characters, ["/", "+"]}} = machine_validator:validate_machine_states([
        "1",
        "/",
        "+",
        "/",
        "-",
        "+"
    ]).
