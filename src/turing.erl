-module(turing).
-include("machine.hrl").

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%{
%     "states": ["IDLE", "HALT"],
%     "initial": "IDLE",
%     "transitions": {
%         "IDLE": [
%             { "read": "0", "to_state": "IDLE", "write": ".", "action": "LEFT" },
%             { "read": "1", "to_state": "IDLE", "write": "0", "action": "RIGHT" },
%             { "read": ".", "to_state": "HALT", "write": ".", "action": "LEFT" }
%         ]
%     }
% }
%% escript Entry point

main(Args) ->
    % Parser step
    ParsedMachineConfig = #parsed_machine_config{
        states = ["IDLE", "HALT"],
        initial = "IDLE",
        transitions = #{
            "IDLE" => [
                #parsed_machine_config_transition{
                    read = "0", to_state = "IDLE", write = ".", action = left
                },
                #parsed_machine_config_transition{
                    read = "1", to_state = "IDLE", write = "0", action = right
                },
                #parsed_machine_config_transition{
                    read = ".", to_state = "HALT", write = ".", action = left
                }
            ]
        }
    },
    interpreter:start(ParsedMachineConfig, ["1", "1", "0"]),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================
