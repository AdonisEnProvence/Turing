-module(reverse_tab_tests).

-include_lib("eunit/include/eunit.hrl").

reverse_tab_test() ->
    ?assertMatch([6, 5, 4, 3, 2, 1], reverse_tab:reverse_tab([1, 2, 3, 4, 5, 6])).
