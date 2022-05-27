-module(parser).

-include("machine.hrl").
-include_lib("eunit/include/eunit.hrl").

-ifdef(TEST).
-export([
    parse_machine_name/1,
    parse_machine_blank/1,
    parse_machine_initial_state/1,
    parse_machine_states/1,
    parse_machine_finals/1,
    parse_machine_transitions/1,
    parse_machine_alphabet/1
]).
-else.
-export([]).
-endif.

parse_machine_name(#{<<"name">> := <<"">>}) -> {error, empty};
parse_machine_name(#{<<"name">> := Name}) when is_bitstring(Name) -> {ok, binary_to_list(Name)};
parse_machine_name(_) -> {error, invalid}.

parse_machine_alphabet(#{<<"alphabet">> := Alphabet}) when is_list(Alphabet) ->
    parse_alphabet_list(Alphabet, []);
parse_machine_alphabet(_) -> 
    {error, no_entry}.

parse_alphabet_list([], []) ->
    {error, empty_list};
parse_alphabet_list([], ParsedAlphabet) ->
    {ok, lists:reverse(ParsedAlphabet)};
parse_alphabet_list([AlphabetCharacter | OtherAlphabetCharacters], ParsedAlphabet) ->
    Result = parse_alphabet_character(AlphabetCharacter),
    case Result of
        {error, Error} ->
            {error, Error};
        {ok, ParsedAlphabetCharacter} ->
            parse_alphabet_list(OtherAlphabetCharacters, [ParsedAlphabetCharacter | ParsedAlphabet])
    end.

parse_machine_blank(#{<<"blank">> := Blank}) ->
    parse_alphabet_character(Blank);
parse_machine_blank(_) ->
    {error, invalid}.

parse_machine_initial_state(#{<<"initial">> := InitialState}) ->
    parse_state(InitialState);
parse_machine_initial_state(_) -> {error, invalid}.

parse_machine_states(#{<<"states">> := States}) when is_list(States) ->
    parse_state_list(States, []);
parse_machine_states(_) ->
    {error, invalid}.

parse_machine_finals(#{<<"finals">> := FinalStates}) when is_list(FinalStates) ->
    parse_state_list(FinalStates, []);
parse_machine_finals(_) ->
    {error, invalid}.

parse_state_list([], []) ->
    {error, empty_list};
parse_state_list([], ParsedStates) ->
    {ok, lists:reverse(ParsedStates)};
parse_state_list([State | States], ParsedStates) ->
    Result = parse_state(State),
    case Result of
        {error, Error} ->
            {error, Error};
        {ok, ParsedState} ->
            parse_state_list(States, [ParsedState | ParsedStates])
    end.

parse_state(<<"">>) ->
    {error, empty_state};
parse_state(State) when is_bitstring(State) ->
    {ok, binary_to_list(State)};
parse_state(State) ->
    {error, {expected_bitstring, State}}.

parse_alphabet_character(<<"">>) ->
    {error, empty_alphabet_character};
parse_alphabet_character(Character) when is_bitstring(Character) ->
    CharacterString = binary_to_list(Character),
    CharacterStringLength = length(CharacterString),

    if
        CharacterStringLength > 1 ->
            {error, {too_long_alphabet_character, CharacterString}};
        CharacterStringLength =:= 1 ->
            {ok, CharacterString}
    end;
parse_alphabet_character(UnknownCharacter) ->
    {error, {expected_bitstring, UnknownCharacter}}.

parse_machine_transitions(#{<<"transitions">> := Transitions}) when is_map(Transitions) ->
    Iterator = maps:iterator(Transitions),
    Result = iterate_on_machine_states_transitions_map(Iterator),
    case Result of
        {error, Error} ->
            {error, Error};
        {ok, ParsedTransitions} ->
            {ok, ParsedTransitions}
    end;
parse_machine_transitions(_) ->
    {error, invalid}.

iterate_on_machine_states_transitions_map(Iterator) ->
    {State, Transitions, NextIterator} = maps:next(Iterator),
    iterate_on_machine_states_transitions_map(NextIterator, State, Transitions, #{}).
iterate_on_machine_states_transitions_map(Iterator, State, Transitions, ParsedTransitionMap) when
    is_bitstring(State)
->
    StateString = binary_to_list(State),
    ParsedTransitionsResult = iterate_on_machine_transitions_list(Transitions),
    case ParsedTransitionsResult of
        {error, CurrentTransitionIndex, Type, Error} ->
            {error, {CurrentTransitionIndex, Type, Error}};
        {ok, ParsedTransitions} ->
            MapWithNewState = maps:put(
                StateString, ParsedTransitions, ParsedTransitionMap
            ),
            NextIteratorResult = maps:next(Iterator),
            case NextIteratorResult of
                none ->
                    {ok, MapWithNewState};
                {NextKey, NextValue, NextIterator} ->
                    iterate_on_machine_states_transitions_map(
                        NextIterator, NextKey, NextValue, MapWithNewState
                    )
            end
    end;
iterate_on_machine_states_transitions_map(_Iterator, State, _Value, _ParsedTransitionMap) ->
    {error, {expected_state_bitstring, State}}.

parse_transition_read(#{<<"read">> := Read}) ->
    parse_alphabet_character(Read);
parse_transition_read(_) ->
    {error, no_entry}.

parse_transition_write(#{<<"write">> := Write}) ->
    parse_alphabet_character(Write);
parse_transition_write(_) ->
    {error, no_entry}.

parse_transition_target_state(#{<<"to_state">> := TargetState}) ->
    parse_state(TargetState);
parse_transition_target_state(_) ->
    {error, no_entry}.

parse_transition_action(#{<<"action">> := <<"LEFT">>}) ->
    {ok, left};
parse_transition_action(#{<<"action">> := <<"RIGHT">>}) ->
    {ok, right};
parse_transition_action(#{<<"action">> := UnknownAction}) ->
    {error, {unknown_action, UnknownAction}};
parse_transition_action(_) ->
    {error, no_entry}.

iterate_on_machine_transitions_list(TransitionsList) ->
    iterate_on_machine_transitions_list(TransitionsList, [], 0).

iterate_on_machine_transitions_list([], ParsedTransitionsList, _) ->
    {ok, lists:reverse(ParsedTransitionsList)};
iterate_on_machine_transitions_list(
    [RawTransition | OtherRawTransitions], ParsedTransitionsList, CurrentTransitionIndex
) ->
    ParseTransitionResult = parse_transition(RawTransition),
    case ParseTransitionResult of
        {error, Type, Error} ->
            {error, CurrentTransitionIndex, Type, Error};
        {ok, ParsedTransition} ->
            iterate_on_machine_transitions_list(
                OtherRawTransitions,
                [ParsedTransition | ParsedTransitionsList],
                CurrentTransitionIndex + 1
            )
    end.

parse_transition(RawTransition) ->
    ParsingReadResult = parse_transition_read(RawTransition),
    case ParsingReadResult of
        {error, Error} ->
            {error, read, Error};
        {ok, Read} ->
            ParsingWriteResult = parse_transition_write(RawTransition),
            case ParsingWriteResult of
                {error, Error} ->
                    {error, write, Error};
                {ok, Write} ->
                    ParsingTargetStateResult = parse_transition_target_state(RawTransition),
                    case ParsingTargetStateResult of
                        {error, Error} ->
                            {error, to_state, Error};
                        {ok, ToState} ->
                            ParsingActionResult = parse_transition_action(RawTransition),
                            case ParsingActionResult of
                                {error, Error} ->
                                    {error, action, Error};
                                {ok, Action} ->
                                    {ok, #parsed_machine_config_transition{
                                        read = Read,
                                        to_state = ToState,
                                        write = Write,
                                        action = Action
                                    }}
                            end
                    end
            end
    end.

with_quotes(String) -> "\"" ++ String ++ "\"".

format_error(read, no_entry) ->
    "property read not found";
format_error(read, empty_alphabet_character) ->
    "given read value is empty";
format_error(read, {too_long_alphabet_character, ErrorValue}) ->
    "given read value is longer than 1 character; received: " ++ with_quotes(ErrorValue);
format_error(read, {expected_bitstring, ErrorValue}) ->
    "expected read value to be a string; received: " ++ with_quotes(ErrorValue).
