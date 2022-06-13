-module(turing).
-include("machine.hrl").

-export([main/1]).

option_spec_list() ->
    [
        {help, $h, "help", undefined, ""},
        {jsonfile, undefined, undefined, string, ""},
        {input, undefined, undefined, string, ""}
    ].

print_usage() ->
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

print_read_file_error(Reason) ->
    io:format("Error while reading machine configuration: ~s~n", [file:format_error(Reason)]).

print_json_decode_error() ->
    io:format("Error while decoding machine configuration: can not decode invalid json file~n").

is_arg(ArgName) ->
    fun(Arg) ->
        case Arg of
            {ArgName, _} -> true;
            _ -> false
        end
    end.

extract_arg(ArgName, Args) ->
    case lists:search(is_arg(ArgName), Args) of
        {value, {ArgName, Value}} -> {ok, Value};
        false -> undefined
    end.

get_raw_machine_config(ParsedArgs) ->
    case extract_arg(jsonfile, ParsedArgs) of
        {ok, FilePath} ->
            case extract_arg(input, ParsedArgs) of
                {ok, Input} ->
                    ReadFileResult = file:read_file(FilePath),
                    case ReadFileResult of
                        {error, Reason} ->
                            print_read_file_error(Reason);
                        {ok, BinaryFile} ->
                            decode_raw_machine_config(BinaryFile, Input)
                    end;
                undefined ->
                    io:format("Error: missing input argument~n"),
                    print_usage()
            end;
        undefined ->
            io:format("Error: missing jsonfile argument~n"),
            print_usage()
    end.

decode_raw_machine_config(BinaryFile, Input) ->
    TryDecodeBinaryFileResult = jsone:try_decode(BinaryFile),
    case TryDecodeBinaryFileResult of
        {ok, DecodedBinaryFile, _} ->
            parse_decoded_machine_config(DecodedBinaryFile, Input);
        {error, _Error} ->
            print_json_decode_error()
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
    case getopt:parse(option_spec_list(), Args) of
        {ok, {ParsedArgs, _UnknownArgs}} ->
            ShowUsage = lists:member(help, ParsedArgs),
            case ShowUsage of
                true ->
                    print_usage();
                false ->
                    get_raw_machine_config(ParsedArgs)
            end;
        _ ->
            io:format("Error during arguments parsing~n")
    end,
    erlang:halt(0).
