-module(parse_and_validate_machine_config).
-include("machine.hrl").

-export([
    parse_and_validate_then_start_decoded_machine/3
]).

parse_and_validate_then_start_decoded_machine(DecodedMachineConfig, Input, ProgramOptions) ->
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
            validate_parsed_machine_config(ParsedMachineConfig, Input, ProgramOptions)
    end.

validate_parsed_machine_config(ParsedMachineConfig, Input, ProgramOptions) ->
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
            parse_input(ParsedMachineConfig, Input, ProgramOptions)
    end.

parse_input(ParsedMachineConfig, Input, ProgramOptions) ->
    InputParsingResult = input_parser:parse(ParsedMachineConfig, Input),
    case InputParsingResult of
        {ok, ParsedInput} -> start_machine(ParsedMachineConfig, ParsedInput, ProgramOptions);
        error -> error
    end.

start_machine(ParsedMachineConfig, Input, ProgramOptions) ->
    interpreter:start(ParsedMachineConfig, Input, ProgramOptions).
