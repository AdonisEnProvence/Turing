-module(turing).
-include("machine.hrl").

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    % Parser step
    ParsedMachineConfig = #parsed_machine_config{
        states = ["scanright", "eraseone", "subone", "skip", "HALT"],
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
