-module(turing).
-include("machine.hrl").

%% API exports
-ifdef(TEST).
-export([parse_optionnal_first_flag_arg/1]).
-else.
-export([main/1]).
-endif.

%%====================================================================
%% API functions
%%====================================================================

format_program_usage() ->
    io:format(
        "usage: ft_turing [-h] jsonfile input\n"
        "\n"
        "positional arguments:\n"
        "  jsonfile          json description of the machine\n"
        "\n"
        "  input             input of the machine\n"
        "\n"
        "optional arguments:\n"
        "  -h, --help        show this help message and exit~n"
    ).

format_read_file_error(Reason) ->
    io:format("Error while reading machine configuration json file: ~p~n", [Reason]).

format_try_decode_error({Reason, _Stack}) ->
    io:format("Error while decoding machine configuration json file: ~p~n", [Reason]).

%% escript Entry point
parse_optionnal_first_flag_arg([]) ->
    format_program_usage(),
    {error, empty_args};
parse_optionnal_first_flag_arg([FirstArg | _OtherArgs]) when
    FirstArg =:= "--help"; FirstArg =:= "-h"
->
    format_program_usage(),
    exit;
parse_optionnal_first_flag_arg(Args) when length(Args) =:= 2 ->
    get_raw_machine_config(Args);
parse_optionnal_first_flag_arg(_) ->
    format_program_usage(),
    {error, too_many_args}.

get_raw_machine_config([FilePath, Input]) ->
    ReadFileResult = file:read_file(FilePath),
    case ReadFileResult of
        {error, Reason} ->
            format_read_file_error(Reason);
        {ok, BinaryFile} ->
            decode_raw_machine_config(BinaryFile, Input)
    end.

decode_raw_machine_config(BinaryFile, Input) ->
    TryDecodeBinaryFileResult = jsone:try_decode(BinaryFile),
    case TryDecodeBinaryFileResult of
        {ok, DecodedBinaryFile, _} ->
            parse_decoded_machine_config(DecodedBinaryFile, Input);
        {error, Error} ->
            format_try_decode_error(Error)
    end.

parse_decoded_machine_config(DecodedMachineConfig, Input) ->
    ParsedMachineResult = parser:parse_machine(DecodedMachineConfig),
    case ParsedMachineResult of
        {error, Error} ->
            parser:format_error(error, Error);
        {ok, ParsedMachineConfig} ->
            parse_input(ParsedMachineConfig, Input)
    end.

parse_input(ParsedMachineConfig, "") ->
    InputPrefilledWithBlankCharacter = [ParsedMachineConfig#parsed_machine_config.blank],
    start_machine(ParsedMachineConfig, InputPrefilledWithBlankCharacter);
parse_input(ParsedMachineConfig, Input) ->
    ExplodedInput = [[Character] || Character <- Input],
    InputParsingSteps = [
        parse_input_check_all_characters_are_in_alphabet(ExplodedInput, ParsedMachineConfig#parsed_machine_config.alphabet),
        parse_input_check_blank_is_not_in_input(ExplodedInput, ParsedMachineConfig#parsed_machine_config.blank)
    ],
    ParsingResult = parse_input_run_steps(InputParsingSteps),
    case ParsingResult of
        ok -> start_machine(ParsedMachineConfig, ExplodedInput);
        {error, {character_not_in_alphabet, NotFoundCharacter}} -> io:format("Character ~p is not in the alphabet~n", [NotFoundCharacter]);
        {error, blank_found_in_input} -> io:format("Blank character is forbidden in input~n")
    end.

parse_input_check_all_characters_are_in_alphabet(Input, Alphabet) ->
    fun () ->
        CheckingCharactersAreInAlphabet = check_characters_are_in_alphabet(Input, Alphabet),
        case CheckingCharactersAreInAlphabet of
            ok ->
                ok;
            {error, Error} ->
                {error, Error}
        end
    end.

parse_input_check_blank_is_not_in_input(Input, Blank) ->
    fun () ->
        DoesContainBlankCharacter = lists:member(Blank, Input),
        case DoesContainBlankCharacter of
            true -> {error, blank_found_in_input};
            false -> ok
        end
    end.

parse_input_run_steps([]) -> ok;
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

start_machine(ParsedMachineConfig, Input) ->
    interpreter:start(ParsedMachineConfig, Input).

main(Args) ->
    %We could handle any logs below
    parse_optionnal_first_flag_arg(Args),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================
