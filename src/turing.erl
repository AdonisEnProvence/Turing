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
    io:format("Error while reading machine configuration: ~s~n", [file:format_error(Reason)]).

format_try_decode_error() ->
    io:format("Error while decoding machine configuration: can not decode invalid json file~n").

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
        {error, _Error} ->
            format_try_decode_error()
    end.

parse_decoded_machine_config(DecodedMachineConfig, Input) ->
    ParsedMachineResult = parser:parse_machine(DecodedMachineConfig),
    case ParsedMachineResult of
        {error, Error} ->
            FormattedError = parser:format_error(Error),
            io:format(
                "Error occured during machine configuration parsing:\n"
                "\n"
                "~s~n",
                [FormattedError]
            );
        {ok, ParsedMachineConfig} ->
            validate_parsed_machine_config(ParsedMachineConfig, Input)
    end.

validate_parsed_machine_config(ParsedMachineConfig, Input) ->
    ParsedMachineResult = machine_validator:validate_machine(ParsedMachineConfig),
    case ParsedMachineResult of
        {error, Error} ->
            FormattedError = machine_validator:format_error(Error),
            io:format(
                "Error occured during machine configuration validation:\n"
                "\n"
                "~s~n",
                [FormattedError]
            );
        ok ->
            parse_input(ParsedMachineConfig, Input)
    end.

parse_input(ParsedMachineConfig, Input) ->
    InputParsingResult = input_parser:parse(ParsedMachineConfig, Input),
    case InputParsingResult of
        {ok, ParsedInput} -> start_machine(ParsedMachineConfig, ParsedInput);
        error -> error
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
