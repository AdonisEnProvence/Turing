-module(turing).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    io:format("Args: ~p~n", [Args]),
    % Parser step
    ParsedMachineConfig = #{
        "transitions" =>
            [
                #{"read" => ".", "to_state" => "writing", "write" => "1", "action" => "RIGHT"},
                #{"read" => "0", "to_state" => "writing", "write" => "1", "action" => "RIGHT"},
                #{"read" => "1", "to_state" => "halt", "write" => "1", "action" => "RIGHT"}
            ]
    },
    interpreter:start(ParsedMachineConfig, ["1", "1", "0"]),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================
