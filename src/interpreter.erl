-module(interpreter).

-export([start/2]).

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
