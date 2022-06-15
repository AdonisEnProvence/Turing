-module(interpreter).
-include("machine.hrl").

-ifdef(TEST).
-export([start/3, read_and_exec/7]).
-else.
-export([start/3]).
-endif.

-spec start(#parsed_machine_config{}, list(string()), fun(
    ({list(string()), string(), number(), atom(), #parsed_machine_config_transition{}}) ->
        no_return()
)) -> no_return().

start(MachineConfig, Input, FunToExecuteOnEveryLoop) ->
    Tape = Input,
    IndexOnTape = 1,
    loop(
        IndexOnTape,
        Tape,
        MachineConfig,
        MachineConfig#parsed_machine_config.initial,
        FunToExecuteOnEveryLoop
    ).

loop(IndexOnTape, Tape, MachineConfig, CurrentState, FunToExecuteOnEveryLoop) ->
    AvailableTransitions = maps:get(
        CurrentState, MachineConfig#parsed_machine_config.transitions, []
    ),
    ReadResult = read_and_exec(
        IndexOnTape,
        Tape,
        AvailableTransitions,
        MachineConfig#parsed_machine_config.blank,
        CurrentState,
        MachineConfig#parsed_machine_config.finals,
        FunToExecuteOnEveryLoop
    ),
    case ReadResult of
        {continue, NewTape, NewIndexOnTape, NewState} ->
            loop(
                NewIndexOnTape,
                NewTape,
                MachineConfig,
                NewState,
                FunToExecuteOnEveryLoop
            );
        _ ->
            undefined
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

retrieve_transition_to_perform(_, []) ->
    error;
retrieve_transition_to_perform(TapeCurrentValue, [
    #parsed_machine_config_transition{read = TapeCurrentValue} = Transition | _AvailableTransitions
]) ->
    {ok, Transition};
retrieve_transition_to_perform(TapeCurrentValue, [_Transition | AvailableTransitions]) ->
    retrieve_transition_to_perform(TapeCurrentValue, AvailableTransitions).

read_and_exec(
    IndexOnTape,
    Tape,
    AvailableTransitions,
    BlankChar,
    CurrentState,
    Finals,
    FunToExecuteOnEveryLoop
) ->
    TapeCurrentValue = lists:nth(IndexOnTape, Tape),

    RawTransition = retrieve_transition_to_perform(TapeCurrentValue, AvailableTransitions),
    case RawTransition of
        error ->
            FunToExecuteOnEveryLoop({Tape, CurrentState, IndexOnTape, blocked, undefined}),
            {blocked, Tape, IndexOnTape};
        {ok, Transition} ->
            NextState = Transition#parsed_machine_config_transition.to_state,
            NextStateIsFinalState = lists:member(
                NextState, Finals
            ),

            RewrittenTape = replace_character_on_square(
                Tape, IndexOnTape, Transition#parsed_machine_config_transition.write
            ),
            {NewIndex, ExtentedTape} = move_index_on_tape(
                {IndexOnTape, RewrittenTape, BlankChar,
                    Transition#parsed_machine_config_transition.action}
            ),

            if
                NextStateIsFinalState =:= true ->
                    FunToExecuteOnEveryLoop(
                        {Tape, CurrentState, IndexOnTape, continue, Transition}
                    ),
                    FunToExecuteOnEveryLoop({ExtentedTape, NextState, NewIndex, final, undefined}),
                    {final, ExtentedTape, NewIndex,
                        Transition#parsed_machine_config_transition.to_state};
                NextStateIsFinalState =:= false ->
                    FunToExecuteOnEveryLoop(
                        {Tape, CurrentState, IndexOnTape, continue, Transition}
                    ),
                    {continue, ExtentedTape, NewIndex,
                        Transition#parsed_machine_config_transition.to_state}
            end
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
