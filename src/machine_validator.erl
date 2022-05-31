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
look_for_transitions_read_duplicated(TransitionsList, States) ->
    EveryTransitionReadList = lists:map(
        fun(Transition) -> Transition#parsed_machine_config_transition.read end, TransitionsList
    ),
    look_for_duplicated_in_list(EveryTransitionReadList).

iterate_on_map(Iterator, FunctionToApply) ->
    NextIteratorResult = maps:next(Iterator),
    case NextIteratorResult of
        none ->
            ok;
        {Key, Value, NextIterator} ->
            Result = FunctionToApply(Value),
            case Result of
                ok ->
                    iterate_on_map(
                        NextIterator, FunctionToApply
                    );
                {error, Error} ->
                    {error, Key, Error}
            end
    end.

validate_machine_transitions(Transitions, States, Alphabet) ->
    io:format("~p~n", [Transitions]),
    Iterator = maps:iterator(Transitions),
    iterate_on_map(Iterator, fun(TransitionList) ->
        look_for_transitions_read_duplicated(TransitionList, States)
    end).
