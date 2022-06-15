-module(cli).
-include("machine.hrl").

-export([run_cli_command/1]).

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

print_read_file_error(Reason) ->
    io:format("Error while reading machine configuration: ~s~n", [file:format_error(Reason)]).

print_json_decode_error() ->
    io:format("Error while decoding machine configuration: can not decode invalid json file~n").

print_usage() ->
    io:format(
        "usage: ft_turing run [-h] jsonfile input\n"
        "\n"
        "positional arguments:\n"
        "  jsonfile          json description of the machine\n"
        "\n"
        "  input             input of the machine\n"
        "\n"
        "optional arguments:\n"
        "  -c, --color       output head position with color\n"
        "  -h, --help        show this help message and exit~n"
    ).

get_raw_machine_config(ParsedArgs, ProgramOptions) ->
    case extract_arg(jsonfile, ParsedArgs) of
        {ok, FilePath} ->
            case extract_arg(input, ParsedArgs) of
                {ok, Input} ->
                    ReadFileResult = file:read_file(FilePath),
                    case ReadFileResult of
                        {error, Reason} ->
                            print_read_file_error(Reason);
                        {ok, BinaryFile} ->
                            decode_raw_machine_config(BinaryFile, Input, ProgramOptions)
                    end;
                undefined ->
                    io:format("Error: missing input argument~n"),
                    print_usage()
            end;
        undefined ->
            io:format("Error: missing jsonfile argument~n"),
            print_usage()
    end.

decode_raw_machine_config(BinaryFile, Input, ProgramOptions) ->
    TryDecodeBinaryFileResult = jsone:try_decode(BinaryFile),
    case TryDecodeBinaryFileResult of
        {ok, DecodedBinaryFile, _} ->
            parse_and_validate_then_start(DecodedBinaryFile, Input, ProgramOptions);
        {error, _Error} ->
            print_json_decode_error()
    end.

parse_and_validate_then_start(DecodedBinaryFile, Input, ProgramOptions) ->
    ParserValidatorResult = parse_and_validate_machine_config:parse_and_validate_decoded_machine(
        DecodedBinaryFile, Input
    ),
    case ParserValidatorResult of
        {ok, ParsedMachineConfig, ParsedInput} ->
            interpreter:start(ParsedMachineConfig, ParsedInput, ProgramOptions);
        {error, FormattedError} ->
            io:format(FormattedError)
    end.

option_spec_list() ->
    [
        {help, $h, "help", undefined, ""},
        {color, $c, "color", {boolean, false}, ""},
        {jsonfile, undefined, undefined, string, ""},
        {input, undefined, undefined, string, ""}
    ].

run_cli_command(Args) ->
    case getopt:parse(option_spec_list(), Args) of
        {ok, {ParsedArgs, _UnknownArgs}} ->
            ShowUsage = lists:member(help, ParsedArgs),
            case ShowUsage of
                true ->
                    print_usage();
                false ->
                    PrintHeadWithColor =
                        case extract_arg(color, ParsedArgs) of
                            {ok, Value} -> Value;
                            undefined -> false
                        end,
                    ProgramOptions = #program_options{
                        print_head_with_color = PrintHeadWithColor
                    },
                    get_raw_machine_config(ParsedArgs, ProgramOptions)
            end;
        _ ->
            io:format("Error during arguments parsing~n")
    end.
