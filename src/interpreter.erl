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

print_tape_and_head_on_tape(IndexOnTape, Tape) ->
    io:format("Tape: ["),
    print_tape_and_head_on_tape(IndexOnTape, Tape, 1),
    io:format("]~n").

print_tape_and_head_on_tape(IndexOnTape, Tape, CurrentIndexOnTape) ->
    IndexOnTapeIsCurrentIndex = IndexOnTape =:= CurrentIndexOnTape,
    TapeCurrentValue = lists:nth(CurrentIndexOnTape, Tape),
    CurrentIndexOnTapeIsLastIndex = CurrentIndexOnTape =:= length(Tape),

    if 
        IndexOnTapeIsCurrentIndex ->
            io:format("<~p>", [TapeCurrentValue]);
        true ->
            io:format("~p", [TapeCurrentValue])
    end,
    if 
        CurrentIndexOnTapeIsLastIndex ->
            ok;
        true ->
            io:format(","),
            print_tape_and_head_on_tape(IndexOnTape, Tape, CurrentIndexOnTape + 1)
    end.

read_and_exec(IndexOnTape, Tape, MachineConfig) ->
    print_tape_and_head_on_tape(IndexOnTape, Tape),
    TapeCurrentValue = lists:nth(IndexOnTape, Tape),
    if
        TapeCurrentValue =:= "0" ->
            RewrittenTape = replace_character_on_square(Tape, IndexOnTape, "."),
            {NewIndex, NewTape} = move_index_on_tape({IndexOnTape, RewrittenTape, left}),
            read_and_exec(NewIndex, NewTape, MachineConfig);
        TapeCurrentValue =:= "1" ->
            RewrittenTape = replace_character_on_square(Tape, IndexOnTape, "0"),
            {NewIndex, NewTape} = move_index_on_tape({IndexOnTape, RewrittenTape, right}),
            read_and_exec(NewIndex, NewTape, MachineConfig);
        true ->
            io:format("Machine is blocked no more transitions available~n", [])
    end.

replace_character_on_square(Tape, 1 = IndexOnTape, CharacterToWrite) ->
    Right = lists:nthtail(IndexOnTape, Tape),
    [CharacterToWrite | Right];
replace_character_on_square(Tape, IndexOnTape, CharacterToWrite) when IndexOnTape =:= length(Tape) ->
    Left = lists:sublist(Tape, IndexOnTape - 1),
    Left ++ [CharacterToWrite];
replace_character_on_square(Tape, IndexOnTape, CharacterToWrite) ->
    Left = lists:sublist(Tape, IndexOnTape - 1),
    Right = lists:nthtail(IndexOnTape, Tape),
    Left ++ [CharacterToWrite] ++ Right.
