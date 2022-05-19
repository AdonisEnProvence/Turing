-module(interpreter).

-export([start/2]).

% Premiere Etape: Deplacement sur la tape, et detection d'impossibilite de transitionner
%
% Instructions:
% On lit un 0, on va gauche
% On lit un 1, on va à droite
% On lit n'importe quel autre symbole, on ne fait rien.
%
% Entrée :
% Tape: [<"0">, "1", "0"]
% Tape: [<".">, "0", "1", "0"]
% => Detected as blocked as no transitions available for "."
%
% Entrée :
% Tape: [<"1">, "1", "0"]
% Tape: ["1", <"1">, "0"]
% Tape: ["1", "1", <"0">]
% Tape: ["1", <"1">, "0"]
% Tape: ["1", "1", <"0">]
% Tape: ["1", <"1">, "0"]
% Tape: ["1", "1", <"0">]
% Tape: ...
% => Infinite looping
%
% Deuxième étape : écriture
%
% Instructions:
% On lit un 0, on ecrit un blank, on va a gauche.
% On lit un 1, on ecrit un 0, on va a droite.
% On lit n'importe quel autre symbole, on ne fait rien.
%
% Entree:
% Tape: [<"0">, "1", "0"]
% Tape: [<".">, ".", "1", "0"]
% => Detected as blocked
%
% Entrée :
% 0/ Tape: [<"1">, "1", "0"]
% 1/ Tape: ["0", <"1">, "0"]
% 2/ Tape: ["0", "0", <"0">]
% 3/ Tape: ["0", <"0">, "."]
% 4/ Tape: [<"0">, ".", "."]
% 5/ Tape: [<".">, ".", ".", "."]
% => Detected as blocked as no transitions available for "."
%
% Troisieme étape : States, intial + transitions to states.
%
% Input: ["1","1","0"]
% Instructions:
% {
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
%
% Entrée :
% 0/ Tape: [<"1">, "1", "0"]       STATE=IDLE
% 1/ Tape: ["0", <"1">, "0"]       STATE=IDLE
% 2/ Tape: ["0", "0", <"0">]       STATE=IDLE
% 3/ Tape: ["0", <"0">, "."]       STATE=IDLE
% 4/ Tape: [<"0">, ".", "."]       STATE=IDLE
% 5/ Tape: [<".">, ".", ".", "."]  STATE=HALT
% => Detected as blocked as no transitions available in HALT state for "."
%
% Input: ["1","0","1", "0"]
% Instructions:
% {
%     "states": [ "DAY", "NIGHT", "HALT"],
%     "initial": "DAY",
%     "transitions": {
%         "DAY": [
%             { "read": "0", "to_state": "DAY", "write": "d", "action": "RIGHT" },
%             { "read": "1", "to_state": "NIGHT", "write": "d", "action": "RIGHT" },
%             { "read": ".", "to_state": "HALT", "write": "e", "action": "RIGHT" }
%         ],
%         "NIGHT": [
%             { "read": "0", "to_state": "NIGHT", "write": "n", "action": "RIGHT" },
%             { "read": "1", "to_state": "DAY", "write": "n", "action": "RIGHT" },
%             { "read": ".", "to_state": "HALT", "write": "e", "action": "RIGHT" }
%         ]
%     }
% }
%
% Entrée :
% 0/ Tape: [<"1">, "0", "1", "0"]            STATE=DAY
% 1/ Tape: ["d", <"0">, "1", "0"]            STATE=NIGHT
% 2/ Tape: ["d", "n", <"1">, "0"]            STATE=NIGHT
% 3/ Tape: ["d", "n", "n", <"0">]            STATE=DAY
% 4/ Tape: ["d", "n", "n", "d", <".">]       STATE=DAY
% 5/ Tape: ["d", "n", "n", "d", "e", <".">]  STATE=HALT
% => Detected as blocked as no transitions available in HALT state for "."
%
% Input: ["1","1","0"]
% Quatrieme étape : Final state.
% {
%     "name": "test machine",
%     "alphabet": ["0", "1", "."],
%     "blank": ".",
%     "states": ["IDLE", "HALT"],
%     "initial": "IDLE",
%     "finals": ["HALT"],
%     "transitions": {
%         "IDLE": [
%             { "read": "0", "to_state": "IDLE", "write": ".", "action": "LEFT" },
%             { "read": "1", "to_state": "IDLE", "write": "0", "action": "RIGHT" },
%             { "read": ".", "to_state": "HALT", "write": ".", "action": "LEFT" }
%         ]
%     }
% }
%
% Entrée :
% 0/ Tape: [<"1">, "1", "0"]       STATE=IDLE
% 1/ Tape: ["0", <"1">, "0"]       STATE=IDLE
% 2/ Tape: ["0", "0", <"0">]       STATE=IDLE
% 3/ Tape: ["0", <"0">, "."]       STATE=IDLE
% 4/ Tape: [<"0">, ".", "."]       STATE=IDLE
% 5/ Tape: [<".">, ".", ".", "."]  STATE=HALT
% => Final state has been reached BRAVO !

start(MachineConfig, Input) ->
    io:format("Interpreter starting...~n"),
    Tape = Input,
    IndexOnTape = 1,
    read_and_exec(IndexOnTape, Tape, MachineConfig),
    io:format("Interpreter closing...~n").

move_index_on_tape({Index, Tape, left}) ->
    LeftIndex = Index - 1,
    if
        LeftIndex < 1 ->
            {1, ["." | Tape]};
        true ->
            {LeftIndex, Tape}
    end;
move_index_on_tape({Index, Tape, right}) ->
    RightIndex = Index + 1,
    TapeLength = length(Tape),
    if
        RightIndex > TapeLength ->
            {RightIndex, Tape ++ ["."]};
        true ->
            {RightIndex, Tape}
    end.

read_and_exec(IndexOnTape, Tape, MachineConfig) ->
    io:format("Read and execute index tape value ...~n"),
    TapeCurrentValue = lists:nth(IndexOnTape, Tape),
    if
        TapeCurrentValue =:= "0" ->
            io:format("TapeCurrentValue == 0~n", []),
            {NewIndex, NewTape} = move_index_on_tape({IndexOnTape, Tape, left}),
            read_and_exec(NewIndex, NewTape, MachineConfig);
        TapeCurrentValue =:= "1" ->
            io:format("TapeCurrentValue == 1~n", []),
            {NewIndex, NewTape} = move_index_on_tape({IndexOnTape, Tape, right}),
            read_and_exec(NewIndex, NewTape, MachineConfig);
        true ->
            io:format("Machine is blocked no more transitions available~n", [])
    end.
