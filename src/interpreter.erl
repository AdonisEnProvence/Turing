-module(interpreter).
-include("machine.hrl").

-ifdef(TEST).
-export([start/2, read_and_exec/4]).
-else.
-export([start/2]).
-endif.

-spec start(#parsed_machine_config{}, list(string())) -> no_return().

start(MachineConfig, Input) ->
    io:format("Interpreter starting...~n"),
    Tape = Input,
    IndexOnTape = 1,
    loop(IndexOnTape, Tape, MachineConfig, MachineConfig#parsed_machine_config.initial),
    io:format("Interpreter closing...~n").

loop(IndexOnTape, Tape, MachineConfig, CurrentState) ->
    CurrentStateIsFinalState = lists:member(
        CurrentState, MachineConfig#parsed_machine_config.finals
    ),

    if
        CurrentStateIsFinalState =:= true ->
            print_tape_and_head_on_tape(IndexOnTape, Tape),
            io:format("Final state reached !~n");
        true ->
            print_tape_and_head_on_tape(IndexOnTape, Tape),
            AvailableTransitions = maps:get(
                CurrentState, MachineConfig#parsed_machine_config.transitions, []
            ),
            ReadResult = read_and_exec(
                IndexOnTape,
                Tape,
                AvailableTransitions,
                MachineConfig#parsed_machine_config.blank,
                CurrentState
            ),
            case ReadResult of
                {continue, NewTape, NewIndexOnTape, NewState} ->
                    loop(NewIndexOnTape, NewTape, MachineConfig, NewState);
                {blocked, _NewTape, _NewIndexOnTape} ->
                    io:format("Machine is blocked no more transitions available~n")
            end
    end.

move_index_on_tape({Index, Tape, BlankChar, left}) ->
    LeftIndex = Index - 1,
    if
        LeftIndex < 1 ->
            {1, [BlankChar | Tape]};
        true ->
            {LeftIndex, Tape}
    end;
move_index_on_tape({Index, Tape, BlankChar, right}) ->
    RightIndex = Index + 1,
    TapeLength = length(Tape),
    if
        RightIndex > TapeLength ->
            {RightIndex, Tape ++ [BlankChar]};
        true ->
            {RightIndex, Tape}
    end.

print_transition_details(CurrentState, #parsed_machine_config_transition{
    to_state = ToState, read = Read, action = Action, write = Write
}) ->
    io:format("(~p, ~p) -> (~p, ~p, ~p)~n", [CurrentState, Read, ToState, Write, Action]).

print_blocked_transition_details(CurrentState, TapeCurrentValue) ->
    io:format("(~p, ~p) -> BLOCKED~n", [CurrentState, TapeCurrentValue]).

print_tape_and_head_on_tape(IndexOnTape, Tape) ->
    io:format("Tape: ["),
    print_head_index_value_on_tape(IndexOnTape, Tape, 1),
    io:format("] ").

print_head_index_value_on_tape(IndexOnTape, Tape, CurrentIndexOnTape) ->
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
            print_head_index_value_on_tape(IndexOnTape, Tape, CurrentIndexOnTape + 1)
    end.

retrieve_transition_to_perform(_, []) ->
    error;
retrieve_transition_to_perform(TapeCurrentValue, [
    #parsed_machine_config_transition{read = TapeCurrentValue} = Transition | _AvailableTransitions
]) ->
    {ok, Transition};
retrieve_transition_to_perform(TapeCurrentValue, [_Transition | AvailableTransitions]) ->
    retrieve_transition_to_perform(TapeCurrentValue, AvailableTransitions).

read_and_exec(IndexOnTape, Tape, AvailableTransitions, BlankChar, CurrentState) ->
    TapeCurrentValue = lists:nth(IndexOnTape, Tape),

    RawTransition = retrieve_transition_to_perform(TapeCurrentValue, AvailableTransitions),
    case RawTransition of
        error ->
            print_blocked_transition_details(CurrentState, TapeCurrentValue),
            {blocked, Tape, IndexOnTape};
        {ok, Transition} ->
            print_transition_details(CurrentState, Transition),
            RewrittenTape = replace_character_on_square(
                Tape, IndexOnTape, Transition#parsed_machine_config_transition.write
            ),
            {NewIndex, ExtentedTape} = move_index_on_tape(
                {IndexOnTape, RewrittenTape, BlankChar,
                    Transition#parsed_machine_config_transition.action}
            ),
            {continue, ExtentedTape, NewIndex, Transition#parsed_machine_config_transition.to_state}
    end.

replace_character_on_square(Tape, 1 = IndexOnTape, CharacterToWrite) ->
    Right = lists:nthtail(IndexOnTape, Tape),
    [CharacterToWrite | Right];
replace_character_on_square(Tape, IndexOnTape, CharacterToWrite) when
    IndexOnTape =:= length(Tape)
->
    Left = lists:sublist(Tape, IndexOnTape - 1),
    Left ++ [CharacterToWrite];
replace_character_on_square(Tape, IndexOnTape, CharacterToWrite) ->
    Left = lists:sublist(Tape, IndexOnTape - 1),
    Right = lists:nthtail(IndexOnTape, Tape),
    Left ++ [CharacterToWrite] ++ Right.
