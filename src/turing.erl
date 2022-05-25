-module(turing).
-include("machine.hrl").

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    io:format("~p~n", [Args]),
    [FilePath | _OtherArgs] = Args,
    {ok, BinaryFile} = file:read_file(FilePath),
    % io:format("~p~n", [BinaryFile]),
    DecodedMachineConfig = jsone:decode(BinaryFile),
    % io:format("~p~n", [DecodedMachineConfig]),

    % ?
    ParsedTransitions = parser:parse_machine_transitions(DecodedMachineConfig),
    ParsedName = parser:parse_machine_name(DecodedMachineConfig),
    ParsedStates = parser:parse_machine_states(DecodedMachineConfig),
    ParsedBlank = parser:parse_machine_blank(DecodedMachineConfig),

    io:format("~p~n", [
        [
            ParsedName,
            ParsedStates,
            ParsedBlank,
            ParsedTransitions
        ]
    ]),

    % Parser step
    ParsedMachineConfig = #parsed_machine_config{
        states = DecodedMachineConfig,
        initial = "scanright",
        finals = ["HALT"],
        blank = ".",
        transitions = #{
            "scanright" => [
                #parsed_machine_config_transition{
                    read = ".", to_state = "scanright", write = ".", action = right
                },
                #parsed_machine_config_transition{
                    read = "1", to_state = "scanright", write = "1", action = right
                },
                #parsed_machine_config_transition{
                    read = "-", to_state = "scanright", write = "-", action = right
                },
                #parsed_machine_config_transition{
                    read = "=", to_state = "eraseone", write = ".", action = left
                }
            ],
            "eraseone" => [
                #parsed_machine_config_transition{
                    read = "1", to_state = "subone", write = "=", action = left
                },
                #parsed_machine_config_transition{
                    read = "-", to_state = "HALT", write = ".", action = left
                }
            ],
            "subone" => [
                #parsed_machine_config_transition{
                    read = "1", to_state = "subone", write = "1", action = left
                },
                #parsed_machine_config_transition{
                    read = "-", to_state = "skip", write = "-", action = left
                }
            ],
            "skip" => [
                #parsed_machine_config_transition{
                    read = ".", to_state = "skip", write = ".", action = left
                },
                #parsed_machine_config_transition{
                    read = "1", to_state = "scanright", write = ".", action = right
                }
            ]
        }
    },

    interpreter:start(ParsedMachineConfig, ["1", "1", "1", "-", "1", "1", "="]),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================
