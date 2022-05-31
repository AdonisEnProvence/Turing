-module(machine_validator).

-include("machine.hrl").

-ifdef(TEST).
-export([
    validate_machine_alphabet/1,
    validate_machine_states/1,
    validate_machine_blank/2,
    validate_machine_finals/2,
    validate_machine_initial/2,
    validate_machine_transitions/3
]).
-else.
-export([]).
-endif.

% Reminder validation step is after parser step
% We then assume that the received data is already parsed

look_for_duplicated_in_list(List) ->
    ListLength = erlang:length(List),
    ListSet = sets:from_list(List),
    ListSetLength = sets:size(ListSet),
    ListContainsDuplicates = ListLength =/= ListSetLength,
    if
        ListContainsDuplicates =:= false ->
            ok;
        ListContainsDuplicates =:= true ->
            DuplicatedCharacterList = List -- sets:to_list(ListSet),
            {error, {duplicated_elements, DuplicatedCharacterList}}
    end.

is_alphabet_character(Character, Alphabet) ->
    CharacterIsInAlphabet = lists:member(Character, Alphabet),
    if
        CharacterIsInAlphabet =:= true -> ok;
        CharacterIsInAlphabet =:= false -> {error, {expected_alphabet_character, Character}}
    end.

% Alphabet validation
validate_machine_alphabet(Alphabet) ->
    look_for_duplicated_in_list(Alphabet).

% States validation
validate_machine_states(States) ->
    look_for_duplicated_in_list(States).

% Blank Validation
validate_machine_blank(Blank, Alphabet) ->
    is_alphabet_character(Blank, Alphabet).

% Finals Validation
validate_machine_finals(Finals, States) ->
    validate_machine_finals(Finals, States, duplicated_step).

validate_machine_finals(Finals, States, duplicated_step) ->
    Result = look_for_duplicated_in_list(Finals),
    case Result of
        ok ->
            validate_machine_finals(Finals, States, expected_states_step);
        {error, Error} ->
            {error, Error}
    end;
validate_machine_finals(Finals, States, expected_states_step) ->
    FinalsLessStates = Finals -- States,
    FinalLessStatesLength = length(FinalsLessStates),
    io:format("FinalsLessStates ~p;~n", [FinalsLessStates]),
    ExpectedFinalsLessStatesLength = 0,
    EveryFinalsEntryIsStatesEntry = FinalLessStatesLength =:= ExpectedFinalsLessStatesLength,
    io:format("Expected= ~p; Actual= ~p;~n", [ExpectedFinalsLessStatesLength, FinalLessStatesLength]),
    if
        EveryFinalsEntryIsStatesEntry =:= true ->
            ok;
        EveryFinalsEntryIsStatesEntry =:= false ->
            {error, {expected_states, FinalsLessStates}}
    end.

% Initial validation
validate_machine_initial(Initial, States) ->
    InitialIsStatesMember = lists:member(Initial, States),
    if
        InitialIsStatesMember =:= true ->
            ok;
        InitialIsStatesMember =:= false ->
            {
                error, {expected_states, Initial}
            }
    end.

% Transitions validation
% 1/ Search for transitions listeners duplication
% 2/ Validate each read write occurences from alphabet
% 3/ Validate each key states and to_state from states

iterate_and_apply_on_map(Iterator, FunctionToApply) ->
    NextIteratorResult = maps:next(Iterator),
    case NextIteratorResult of
        none ->
            ok;
        {Key, Value, NextIterator} ->
            Result = FunctionToApply(Value),
            case Result of
                ok ->
                    iterate_and_apply_on_map(
                        NextIterator, FunctionToApply
                    );
                {error, Error} ->
                    {error, Key, Error}
            end
    end.

iterate_and_apply_on_list([], _) ->
    ok;
iterate_and_apply_on_list([FirstElement | Rest], FunctionToApply) ->
    Result = FunctionToApply(FirstElement),
    case Result of
        ok -> iterate_and_apply_on_list(Rest, FunctionToApply);
        {error, Error} -> {error, Error}
    end.

% 1/ Search for transitions listeners duplication
look_for_transitions_entry_read_duplicated(TransitionsList, States) ->
    EveryTransitionReadList = lists:map(
        fun(Transition) -> Transition#parsed_machine_config_transition.read end, TransitionsList
    ),
    look_for_duplicated_in_list(EveryTransitionReadList).

validation_step_transitions_map_read_duplication(TransitionsMap, States, Alphabet) ->
    Result = iterate_and_apply_on_map(maps:iterator(TransitionsMap), fun(TransitionList) ->
        look_for_transitions_entry_read_duplicated(TransitionList, States)
    end),
    case Result of
        ok -> validation_step_not_alphabet_read_write_transitions(TransitionsMap, States, Alphabet);
        {error, State, Error} -> {error, State, Error}
    end.

% 2/ Validate each read write occurences from alphabet
verify_read_write_transition(Transition, Alphabet) ->
    Read = Transition#parsed_machine_config_transition.read,
    ReadIsAlphabetCharacter = is_alphabet_character(Read, Alphabet),
    case ReadIsAlphabetCharacter of
        ok ->
            Write = Transition#parsed_machine_config_transition.write,
            WriteIsAlphabetCharacter = is_alphabet_character(Write, Alphabet),
            case WriteIsAlphabetCharacter of
                ok -> ok;
                {error, Error} -> {error, {"write", Error}}
            end;
        {error, Error} ->
            {error, {"read", Error}}
    end.

look_for_transitions_entry_read_write_not_alphabet(TransitionList, Alphabet) ->
    iterate_and_apply_on_list(TransitionList, fun(Transition) ->
        verify_read_write_transition(Transition, Alphabet)
    end).

validation_step_not_alphabet_read_write_transitions(TransitionsMap, States, Alphabet) ->
    Result = iterate_and_apply_on_map(maps:iterator(TransitionsMap), fun(TransitionList) ->
        look_for_transitions_entry_read_write_not_alphabet(TransitionList, Alphabet)
    end),
    io:format("~p~n", [Result]),
    case Result of
        ok -> ok;
        {error, State, Error} -> {error, State, Error}
    end.

validate_machine_transitions(TransitionsMap, States, Alphabet) ->
    validation_step_transitions_map_read_duplication(TransitionsMap, States, Alphabet).
