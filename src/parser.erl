-module(parser).

-include("machine.hrl").
-include_lib("eunit/include/eunit.hrl").

-ifdef(TEST).
-export([
    parse_machine_name/1,
    parse_machine_blank/1,
    parse_machine_states/1,
    parse_machine_finals/1,
    parse_machine_transitions/1
]).
-else.
-export([]).
-endif.

parse_machine_name(#{<<"name">> := <<"">>}) -> {error, empty};
parse_machine_name(#{<<"name">> := Name}) when is_bitstring(Name) -> {ok, binary_to_list(Name)};
parse_machine_name(_) -> {error, invalid}.

parse_machine_blank(#{<<"blank">> := Blank}) ->
    parse_alphabet_character(Blank);
parse_machine_blank(_) ->
    {error, invalid}.

parse_machine_states(#{<<"states">> := States}) when is_list(States) ->
    Result = parse_state_list(States, []),
    case Result of
        {error, invalid} ->
            {error, invalid_element};
        {error, empty_list} ->
            {error, empty_list};
        {error, empty_element} ->
            {error, empty_element};
        {ok, ParsedStates} ->
            {ok, ParsedStates}
    end;
parse_machine_states(_) ->
    {error, invalid}.

parse_machine_finals(#{<<"finals">> := FinalStates}) when is_list(FinalStates) ->
    Result = parse_state_list(FinalStates, []),
    case Result of
        {error, invalid} ->
            {error, invalid_element};
        {error, empty_list} ->
            {error, empty_list};
        {error, empty_element} ->
            {error, empty_element};
        {ok, ParsedStates} ->
            {ok, ParsedStates}
    end;
parse_machine_finals(_) ->
    {error, invalid}.

parse_state_list([], []) ->
    {error, empty_list};
parse_state_list([], ParsedStates) ->
    {ok, lists:reverse(ParsedStates)};
parse_state_list([State | States], ParsedStates) ->
    Result = parse_state(State),
    case Result of
        {error, invalid} ->
            {error, invalid};
        {error, empty} ->
            {error, empty_element};
        {ok, ParsedState} ->
            parse_state_list(States, [ParsedState | ParsedStates])
    end.

parse_state(<<"">>) ->
    {error, empty};
parse_state(State) when is_bitstring(State) ->
    {ok, binary_to_list(State)};
parse_state(_) ->
    {error, invalid}.

parse_alphabet_character(<<"">>) ->
    {error, empty};
parse_alphabet_character(Character) when is_bitstring(Character) ->
    CharacterString = binary_to_list(Character),
    CharacterStringLength = length(CharacterString),

    if
        CharacterStringLength > 1 ->
            {error, too_long};
        CharacterStringLength =:= 1 ->
            {ok, CharacterString}
    end;
parse_alphabet_character(_) ->
    {error, invalid}.

parse_machine_transitions(#{<<"transitions">> := Transitions}) when is_map(Transitions) ->
    Iterator = maps:iterator(Transitions),
    Result = iterate_on_machine_states_transitions_map(Iterator),
    case Result of
        {error, invalid} ->
            {error};
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
    ParsedTransitionsResult = iterate_on_machine_transitions_list(Transitions),
    case ParsedTransitionsResult of
        {error, Type, Error} ->
            io:format("Parser failed => Invalid transitions~n"),
            io:format("In state ~p, the following error occured: [~p | ~p]~n", [State, Type, Error]),
            {error, invalid};
        {ok, ParsedTransitions} ->
            ?debugFmt("State = ~p~n", [State]),
            MapWithNewState = maps:put(
                binary_to_list(State), ParsedTransitions, ParsedTransitionMap
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
iterate_on_machine_states_transitions_map(_Iterator, _Key, _Value, _ParsedTransitionMap) ->
    {error, invalid}.

parse_transition_read(#{<<"read">> := Read}) ->
    parse_alphabet_character(Read);
parse_transition_read(_) ->
    {error, invalid}.

parse_transition_write(#{<<"write">> := <<"">>}) ->
    {error, empty};
parse_transition_write(#{<<"write">> := Write}) ->
    parse_alphabet_character(Write);
parse_transition_write(_) ->
    {error, invalid}.

parse_transition_target_state(#{<<"to_state">> := TargetState}) ->
    parse_state(TargetState);
parse_transition_target_state(_) ->
    {error, invalid}.

parse_transition_action(#{<<"action">> := <<"LEFT">>}) ->
    {ok, left};
parse_transition_action(#{<<"action">> := <<"RIGHT">>}) ->
    {ok, right};
parse_transition_action(#{<<"action">> := UnknownAction}) ->
    {error, {unknown_action, UnknownAction}};
parse_transition_action(_) ->
    {error, invalid}.

iterate_on_machine_transitions_list(TransitionsList) ->
    iterate_on_machine_transitions_list(TransitionsList, []).

iterate_on_machine_transitions_list([], ParsedTransitionsList) ->
    {ok, lists:reverse(ParsedTransitionsList)};
iterate_on_machine_transitions_list([RawTransition | OtherRawTransitions], ParsedTransitionsList) ->
    ?debugFmt("Processing transitions iteration = ~p ~n", [RawTransition]),
    ParseTransitionResult = parse_transition(RawTransition),
    case ParseTransitionResult of
        {error, Type, Error} ->
            {error, Type, Error};
        {ok, ParsedTransition} ->
            iterate_on_machine_transitions_list(
                OtherRawTransitions, [ParsedTransition | ParsedTransitionsList]
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
