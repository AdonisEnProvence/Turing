-module(decompose_string_tests).

-include_lib("eunit/include/eunit.hrl").

reverse_tab_test() ->
    ?assertMatch(
        ["d", "e", "v", "e", "s", "s", "i", "e", "r"],
        decompose_string:decompose_string("devessier")
    ).
