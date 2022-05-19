-module(reverse_tab).

%% API exports
-export([reverse_tab/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
reverse_tab([], ReversedTab) ->
    ReversedTab;
reverse_tab([FirstElement | Rest], ReversedTab) ->
    reverse_tab(Rest, [FirstElement | ReversedTab]).
reverse_tab(SrcTab) ->
    reverse_tab(SrcTab, []).

%%====================================================================
%% Internal functions
%%====================================================================
