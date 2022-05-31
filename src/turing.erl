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

get_raw_machine_config([FilePath, _Input]) ->
    ReadFileResult = file:read_file(FilePath),
    case ReadFileResult of
        {error, Reason} ->
            format_read_file_error(Reason);
        {ok, BinaryFile} ->
            decode_raw_machine_config(BinaryFile)
    end.

decode_raw_machine_config(BinaryFile) ->
    TryDecodeBinaryFileResult = jsone:try_decode(BinaryFile),
    case TryDecodeBinaryFileResult of
        {ok, DecodedBinaryFile, _} ->
            parse_decoded_machine_config(DecodedBinaryFile);
        {error, Error} ->
            format_try_decode_error(Error)
    end.

parse_decoded_machine_config(DecodedMachineConfig) ->
    ParsedMachineResult = parser:parse_machine(DecodedMachineConfig),
    case ParsedMachineResult of
        {error, _Error} ->
            % parser:format_error(error, Error);
            io:format("Parser error");
        {ok, ParsedMachineConfig} ->
            % Will change in the future
            start_machine(ParsedMachineConfig)
    end.
% validate_parsed_machine_config(ParsedMachineConfig)->
% parse_and_validate_word( DecodedMachineConfig) ->
start_machine(ParsedMachineConfig) ->
    interpreter:start(ParsedMachineConfig, ["1", "1", "1", "-", "1", "1", "="]).

main(Args) ->
    %We could handle any logs below
    parse_optionnal_first_flag_arg(Args),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================
