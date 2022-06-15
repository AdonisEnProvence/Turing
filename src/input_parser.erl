-module(input_parser).

-include("machine.hrl").

-export([parse/2, format_error/1]).

parse(ParsedMachineConfig, "") ->
    InputPrefilledWithBlankCharacter = [ParsedMachineConfig#parsed_machine_config.blank],
    {ok, InputPrefilledWithBlankCharacter};
parse(ParsedMachineConfig, Input) ->
    ExplodedInput = [[Character] || Character <- Input],
    InputParsingSteps = [
        parse_input_check_all_characters_are_in_alphabet(
            ExplodedInput, ParsedMachineConfig#parsed_machine_config.alphabet
        ),
        parse_input_check_blank_is_not_in_input(
            ExplodedInput, ParsedMachineConfig#parsed_machine_config.blank
        )
    ],
    ParsingResult = parse_input_run_steps(InputParsingSteps),
    case ParsingResult of
        ok -> {ok, ExplodedInput};
        {error, Error} -> {error, Error}
    end.

parse_input_check_all_characters_are_in_alphabet(Input, Alphabet) ->
    fun() ->
        CheckingCharactersAreInAlphabet = check_characters_are_in_alphabet(Input, Alphabet),
        case CheckingCharactersAreInAlphabet of
            ok ->
                ok;
            {error, Error} ->
                {error, Error}
        end
    end.

parse_input_check_blank_is_not_in_input(Input, Blank) ->
    fun() ->
        DoesContainBlankCharacter = lists:member(Blank, Input),
        case DoesContainBlankCharacter of
            true -> {error, {blank_found_in_input}};
            false -> ok
        end
    end.

parse_input_run_steps([]) ->
    ok;
parse_input_run_steps([Step | OtherSteps]) ->
    case Step() of
        ok -> parse_input_run_steps(OtherSteps);
        {error, Error} -> {error, Error}
    end.

check_characters_are_in_alphabet([], _Alphabet) ->
    ok;
check_characters_are_in_alphabet([Character | OtherInputCharacters], Alphabet) ->
    IsCharacterInAlphabet = lists:member(Character, Alphabet),
    case IsCharacterInAlphabet of
        true -> check_characters_are_in_alphabet(OtherInputCharacters, Alphabet);
        false -> {error, {character_not_in_alphabet, Character}}
    end.

pretty_value(List) -> lists:flatten(io_lib:format("~p", [List])).

format_error({blank_found_in_input}) ->
    "Blank character is forbidden in input\n";
format_error({character_not_in_alphabet, NotFoundCharacter}) ->
    "Character " ++ pretty_value(NotFoundCharacter) ++ " is not in the alphabet\n".
