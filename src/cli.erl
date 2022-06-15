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

print_transition_details(CurrentState, #parsed_machine_config_transition{
    to_state = ToState, read = Read, action = Action, write = Write
}) ->
    io:format("(~s, ~s) -> (~s, ~s, ~s)~n", [CurrentState, Read, ToState, Write, Action]).

print_blocked_transition_details(CurrentState, TapeCurrentValue) ->
    io:format("(~s, ~s) -> BLOCKED~n", [CurrentState, TapeCurrentValue]).

print_tape_and_head_on_tape(IndexOnTape, Tape, PrintHeadWithColor) ->
    io:format("["),
    print_head_index_value_on_tape(IndexOnTape, Tape, 1, PrintHeadWithColor),
    io:format("] ").

print_head_index_value_on_tape(IndexOnTape, Tape, CurrentIndexOnTape, PrintHeadWithColor) ->
    IndexOnTapeIsCurrentIndex = IndexOnTape =:= CurrentIndexOnTape,
    TapeCurrentValue = lists:nth(CurrentIndexOnTape, Tape),
    CurrentIndexOnTapeIsLastIndex = CurrentIndexOnTape =:= length(Tape),

    if
        IndexOnTapeIsCurrentIndex ->
            case PrintHeadWithColor of
                true -> io:format("\033[0;101m~s\033[0m", [TapeCurrentValue]);
                false -> io:format("<~s>", [TapeCurrentValue])
            end;
        true ->
            io:format("~s", [TapeCurrentValue])
    end,
    if
        CurrentIndexOnTapeIsLastIndex ->
            ok;
        true ->
            print_head_index_value_on_tape(
                IndexOnTape, Tape, CurrentIndexOnTape + 1, PrintHeadWithColor
            )
    end.

cli_logs(PrintHeadWithColor, {Tape, CurrentState, IndexOnTape, continue, Transition}) ->
    print_tape_and_head_on_tape(IndexOnTape, Tape, PrintHeadWithColor),
    print_transition_details(CurrentState, Transition);
cli_logs(PrintHeadWithColor, {Tape, CurrentState, IndexOnTape, blocked, _Transition}) ->
    TapeCurrentValue = lists:nth(IndexOnTape, Tape),
    print_tape_and_head_on_tape(IndexOnTape, Tape, PrintHeadWithColor),
    print_blocked_transition_details(CurrentState, TapeCurrentValue),
    io:format("Machine is blocked no more transitions available~n");
cli_logs(PrintHeadWithColor, {Tape, _CurrentState, IndexOnTape, final, _Transition}) ->
    print_tape_and_head_on_tape(IndexOnTape, Tape, PrintHeadWithColor),
    io:format("Final state reached !~n").

parse_and_validate_then_start(DecodedBinaryFile, Input, ProgramOptions) ->
    ParserValidatorResult = parse_and_validate_machine_config:parse_and_validate_decoded_machine(
        DecodedBinaryFile, Input
    ),
    PrintHeadWithColor = ProgramOptions#program_options.print_head_with_color,

    case ParserValidatorResult of
        {ok, ParsedMachineConfig, ParsedInput} ->
            io:format("Interpreter starting...~n"),
            interpreter:start(ParsedMachineConfig, ParsedInput, fun(
                LoopInformation
            ) ->
                cli_logs(PrintHeadWithColor, LoopInformation)
            end),
            io:format("Interpreter closing...~n");
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
