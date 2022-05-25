-module(parser).

-ifdef(TEST).
-export([parse_machine_name/1, parse_machine_blank/1, parse_machine_states/1]).
-else.
-export([]).
-endif.

parse_machine_name(#{<<"name">> := <<"">>}) -> {error, empty};
parse_machine_name(#{<<"name">> := Name}) when is_bitstring(Name) -> {ok, binary_to_list(Name)};
parse_machine_name(_) -> {error, invalid}.

parse_machine_blank(#{<<"blank">> := <<"">>}) ->
    {error, empty};
parse_machine_blank(#{<<"blank">> := Blank}) when is_bitstring(Blank) ->
    StringBlank = binary_to_list(Blank),
    StringBlankLength = length(StringBlank),

    if
        StringBlankLength > 1 ->
            {error, too_long};
        true ->
            {ok, StringBlank}
    end;
parse_machine_blank(_) ->
    {error, invalid}.

parse_machine_states(#{<<"states">> := States}) when is_list(States) ->
    Result = parse_machine_states(States, []),
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

parse_machine_states([], []) ->
    {error, empty_list};
parse_machine_states([], ParsedStates) ->
    {ok, lists:reverse(ParsedStates)};
parse_machine_states([<<"">> | _Rest], _ParsedStates) ->
    {error, empty_element};
parse_machine_states([State | Rest], ParsedStates) when is_bitstring(State) ->
    parse_machine_states(Rest, [binary_to_list(State) | ParsedStates]);
parse_machine_states([_State | _Rest], _ParsedStates) ->
    {error, invalid}.
