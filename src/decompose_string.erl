-module(decompose_string).

-export([decompose_string/1]).

decompose_string(Head) ->
    FirstStringChar = string:slice(Head, 0, 1),
    RemainingString = string:slice(Head, 1),
    DecomposedString = [],
    decompose_string(FirstStringChar, RemainingString, DecomposedString).

decompose_string(PreviousFirstChar, RemainingString, DestString) ->
    FirstStringChar = string:slice(RemainingString, 0, 1),
    RemainingString = string:slice(RemainingString, 1),
    decompose_string(FirstStringChar, RemainingString, [PreviousFirstChar, DestString]);
decompose_string("", "", DestString) ->
    DestString.
