-module(parse_and_validate_machine_config).
-include("machine.hrl").

-export([
    parse_and_validate_decoded_machine_and_input/2
]).

parse_and_validate_decoded_machine_and_input(
    DecodedMachineConfig, Input
) ->
    ParsedMachineResult = parser:parse_machine(DecodedMachineConfig),
    case ParsedMachineResult of
        {error, Error} ->
            FormattedError =
                "Error occured during machine configuration parsing:\n\n" ++
                    parser:format_error(Error) ++ "\n",
            {
                error,
                FormattedError
            };
        {ok, ParsedMachineConfig} ->
            validate_parsed_machine_config(ParsedMachineConfig, Input)
    end.

validate_parsed_machine_config(ParsedMachineConfig, Input) ->
    ParsedMachineResult = machine_validator:validate_machine(ParsedMachineConfig),
    case ParsedMachineResult of
        {error, Error} ->
            FormattedError =
                "Error occured during machine configuration validation:\n\n" ++
                    machine_validator:format_error(Error) ++ "\n",
            {
                error,
                FormattedError
            };
        ok ->
            parse_input(ParsedMachineConfig, Input)
    end.

parse_input(ParsedMachineConfig, Input) ->
    InputParsingResult = input_parser:parse(ParsedMachineConfig, Input),
    case InputParsingResult of
        {ok, ParsedInput} ->
            {ok, ParsedMachineConfig, ParsedInput};
        {error, Error} ->
            FormattedError = input_parser:format_error(Error),
            {error, FormattedError}
    end.
