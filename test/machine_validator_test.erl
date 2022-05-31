-module(machine_validator_test).

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

% Validate Alphabet
validate_machine_alphabet_test() ->
    ok = machine_validator:validate_machine_alphabet(get_valid_alphabet()).
parse_machine_alphabet_duplicated_entry_test() ->
    {error, {duplicated_elements, ["="]}} = machine_validator:validate_machine_alphabet(
        get_valid_alphabet() ++ ["="]
    ).
parse_machine_alphabet_several_duplicated_entry_test() ->
    {error, {duplicated_elements, [".", "="]}} = machine_validator:validate_machine_alphabet(
        get_valid_alphabet() ++ [".", "="]
    ).

% Validate States
validate_machine_states_test() ->
    ok = machine_validator:validate_machine_states(get_valid_states()).
parse_machine_states_duplicated_entry_test() ->
    {error, {duplicated_elements, ["HALT"]}} = machine_validator:validate_machine_states(
        get_valid_states() ++ ["HALT"]
    ).
parse_machine_states_several_duplicated_entry_test() ->
    {error, {duplicated_elements, ["HALT", "scanright"]}} = machine_validator:validate_machine_states(
        get_valid_states() ++ ["HALT", "scanright"]
    ).

% Validate Blank
validate_machine_blank_test() ->
    ok = machine_validator:validate_machine_blank(".", get_valid_alphabet()).
validate_machine_blank_not_alphabet_character_error_test() ->
    {error, {expected_alphabet_character, "$"}} = machine_validator:validate_machine_blank(
        "$", get_valid_alphabet()
    ).

% Validate Finals
validate_machine_finals_test() ->
    ok = machine_validator:validate_machine_finals(["HALT"], get_valid_states()).

validate_machine_finals_duplicated_entry_test() ->
    {error, {duplicated_elements, ["HALT"]}} = machine_validator:validate_machine_finals(
        ["HALT", "scanright", "HALT"], get_valid_states()
    ).

validate_machine_finals_expected_states_entry_error_test() ->
    {error, {expected_states, ["invalid_state_entry"]}} = machine_validator:validate_machine_finals(
        ["HALT", "invalid_state_entry"], get_valid_states()
    ).

validate_machine_finals_several_expected_states_entry_error_test() ->
    {error, {expected_states, ["invalid_state_entry_0", "invalid_state_entry_1"]}} = machine_validator:validate_machine_finals(
        ["HALT", "invalid_state_entry_0", "invalid_state_entry_1"], get_valid_states()
    ).
