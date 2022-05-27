-module(turing).
-include("machine.hrl").

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================


%% escript Entry point
parse_args(Args) ->
    Argc = length(Args),
    if 
        Argc =:= 2 ->
            [FilePath | _OtherArgs] = Args,
            get_raw_machine_config(FilePath);
        true ->
            io:format("USAGE~n")
    end.

get_raw_machine_config(FilePath) ->
    ReadFileResult = file:read_file(FilePath),
    case ReadFileResult of
        {error, Reason} ->
            io:format("Could'nt get given machine config, ~p~n", [Reason]);
        {ok, BinaryFile} ->
            decode_raw_machine_config(BinaryFile)
    end.

decode_raw_machine_config(BinaryFile) ->
    TryDecodeBinaryFileResult = jsone:try_decode(BinaryFile),
    case TryDecodeBinaryFileResult of 
        {error, Error} ->
            io:format("AIE AIE AIE~n");
        {ok, DecodedBinaryFile} ->
            parse_decoded_machine_config(DecodedBinaryFile)
    end.

parse_decoded_machine_config(DecodedMachineConfig) -> 
    ParsedMachineResult = parser:parse_machine(DecodedMachineConfig),
    case ParsedMachineResult of 
        {error, Error} ->
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
    parse_args(Args),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================
