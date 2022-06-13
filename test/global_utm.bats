setup() {
    load 'test_helper/bats-support/load'
    load 'test_helper/bats-assert/load'
    # ... the remaining setup is unchanged

    # get the containing directory of this file
    # use $BATS_TEST_FILENAME instead of ${BASH_SOURCE[0]} or $0,
    # as those will point to the bats executable's location or the preprocessed file respectively
    DIR="$( cd "$( dirname "$BATS_TEST_FILENAME" )" >/dev/null 2>&1 && pwd )"
    # make executables in src/ visible to PATH
    PATH="$DIR/../src:$PATH"
}

@test "Invalid states decleration" {
    run bash -c './_build/default/bin/turing our-machines/unary_add_utm.json  "C~P{[_C>1]C{[+S>_][1C>1]}S{[1P<+][_H<_]}&11+1" | cat -e'
    assert_output 'Interpreter starting...$
[<C>~P{[_C>1]C{[+S>_][1C>1]}S{[1P<+][_H<_]}&11+1] (retrieve_initial_state, C) -> (go-to-input-start-for_C, C, right)$
[C<~>P{[_C>1]C{[+S>_][1C>1]}S{[1P<+][_H<_]}&11+1] (go-to-input-start-for_C, ~) -> (go-to-input-start-for_C, ~, right)$
[C~<P>{[_C>1]C{[+S>_][1C>1]}S{[1P<+][_H<_]}&11+1] (go-to-input-start-for_C, P) -> (go-to-input-start-for_C, P, right)$
[C~P<{>[_C>1]C{[+S>_][1C>1]}S{[1P<+][_H<_]}&11+1] (go-to-input-start-for_C, {) -> (go-to-input-start-for_C, {, right)$
[C~P{<[>_C>1]C{[+S>_][1C>1]}S{[1P<+][_H<_]}&11+1] (go-to-input-start-for_C, [) -> (go-to-input-start-for_C, [, right)$
[C~P{[<_>C>1]C{[+S>_][1C>1]}S{[1P<+][_H<_]}&11+1] (go-to-input-start-for_C, _) -> (go-to-input-start-for_C, _, right)$
[C~P{[_<C>>1]C{[+S>_][1C>1]}S{[1P<+][_H<_]}&11+1] (go-to-input-start-for_C, C) -> (go-to-input-start-for_C, C, right)$
[C~P{[_C<>>1]C{[+S>_][1C>1]}S{[1P<+][_H<_]}&11+1] (go-to-input-start-for_C, >) -> (go-to-input-start-for_C, >, right)$
[C~P{[_C><1>]C{[+S>_][1C>1]}S{[1P<+][_H<_]}&11+1] (go-to-input-start-for_C, 1) -> (go-to-input-start-for_C, 1, right)$
[C~P{[_C>1<]>C{[+S>_][1C>1]}S{[1P<+][_H<_]}&11+1] (go-to-input-start-for_C, ]) -> (go-to-input-start-for_C, ], right)$
[C~P{[_C>1]<C>{[+S>_][1C>1]}S{[1P<+][_H<_]}&11+1] (go-to-input-start-for_C, C) -> (go-to-input-start-for_C, C, right)$
[C~P{[_C>1]C<{>[+S>_][1C>1]}S{[1P<+][_H<_]}&11+1] (go-to-input-start-for_C, {) -> (go-to-input-start-for_C, {, right)$
[C~P{[_C>1]C{<[>+S>_][1C>1]}S{[1P<+][_H<_]}&11+1] (go-to-input-start-for_C, [) -> (go-to-input-start-for_C, [, right)$
[C~P{[_C>1]C{[<+>S>_][1C>1]}S{[1P<+][_H<_]}&11+1] (go-to-input-start-for_C, +) -> (go-to-input-start-for_C, +, right)$
[C~P{[_C>1]C{[+<S>>_][1C>1]}S{[1P<+][_H<_]}&11+1] (go-to-input-start-for_C, S) -> (go-to-input-start-for_C, S, right)$
[C~P{[_C>1]C{[+S<>>_][1C>1]}S{[1P<+][_H<_]}&11+1] (go-to-input-start-for_C, >) -> (go-to-input-start-for_C, >, right)$
[C~P{[_C>1]C{[+S><_>][1C>1]}S{[1P<+][_H<_]}&11+1] (go-to-input-start-for_C, _) -> (go-to-input-start-for_C, _, right)$
[C~P{[_C>1]C{[+S>_<]>[1C>1]}S{[1P<+][_H<_]}&11+1] (go-to-input-start-for_C, ]) -> (go-to-input-start-for_C, ], right)$
[C~P{[_C>1]C{[+S>_]<[>1C>1]}S{[1P<+][_H<_]}&11+1] (go-to-input-start-for_C, [) -> (go-to-input-start-for_C, [, right)$
[C~P{[_C>1]C{[+S>_][<1>C>1]}S{[1P<+][_H<_]}&11+1] (go-to-input-start-for_C, 1) -> (go-to-input-start-for_C, 1, right)$
[C~P{[_C>1]C{[+S>_][1<C>>1]}S{[1P<+][_H<_]}&11+1] (go-to-input-start-for_C, C) -> (go-to-input-start-for_C, C, right)$
[C~P{[_C>1]C{[+S>_][1C<>>1]}S{[1P<+][_H<_]}&11+1] (go-to-input-start-for_C, >) -> (go-to-input-start-for_C, >, right)$
[C~P{[_C>1]C{[+S>_][1C><1>]}S{[1P<+][_H<_]}&11+1] (go-to-input-start-for_C, 1) -> (go-to-input-start-for_C, 1, right)$
[C~P{[_C>1]C{[+S>_][1C>1<]>}S{[1P<+][_H<_]}&11+1] (go-to-input-start-for_C, ]) -> (go-to-input-start-for_C, ], right)$
[C~P{[_C>1]C{[+S>_][1C>1]<}>S{[1P<+][_H<_]}&11+1] (go-to-input-start-for_C, }) -> (go-to-input-start-for_C, }, right)$
[C~P{[_C>1]C{[+S>_][1C>1]}<S>{[1P<+][_H<_]}&11+1] (go-to-input-start-for_C, S) -> (go-to-input-start-for_C, S, right)$
[C~P{[_C>1]C{[+S>_][1C>1]}S<{>[1P<+][_H<_]}&11+1] (go-to-input-start-for_C, {) -> (go-to-input-start-for_C, {, right)$
[C~P{[_C>1]C{[+S>_][1C>1]}S{<[>1P<+][_H<_]}&11+1] (go-to-input-start-for_C, [) -> (go-to-input-start-for_C, [, right)$
[C~P{[_C>1]C{[+S>_][1C>1]}S{[<1>P<+][_H<_]}&11+1] (go-to-input-start-for_C, 1) -> (go-to-input-start-for_C, 1, right)$
[C~P{[_C>1]C{[+S>_][1C>1]}S{[1<P><+][_H<_]}&11+1] (go-to-input-start-for_C, P) -> (go-to-input-start-for_C, P, right)$
[C~P{[_C>1]C{[+S>_][1C>1]}S{[1P<<>+][_H<_]}&11+1] (go-to-input-start-for_C, <) -> (go-to-input-start-for_C, <, right)$
[C~P{[_C>1]C{[+S>_][1C>1]}S{[1P<<+>][_H<_]}&11+1] (go-to-input-start-for_C, +) -> (go-to-input-start-for_C, +, right)$
[C~P{[_C>1]C{[+S>_][1C>1]}S{[1P<+<]>[_H<_]}&11+1] (go-to-input-start-for_C, ]) -> (go-to-input-start-for_C, ], right)$
[C~P{[_C>1]C{[+S>_][1C>1]}S{[1P<+]<[>_H<_]}&11+1] (go-to-input-start-for_C, [) -> (go-to-input-start-for_C, [, right)$
[C~P{[_C>1]C{[+S>_][1C>1]}S{[1P<+][<_>H<_]}&11+1] (go-to-input-start-for_C, _) -> (go-to-input-start-for_C, _, right)$
[C~P{[_C>1]C{[+S>_][1C>1]}S{[1P<+][_<H><_]}&11+1] (go-to-input-start-for_C, H) -> (go-to-input-start-for_C, H, right)$
[C~P{[_C>1]C{[+S>_][1C>1]}S{[1P<+][_H<<>_]}&11+1] (go-to-input-start-for_C, <) -> (go-to-input-start-for_C, <, right)$
[C~P{[_C>1]C{[+S>_][1C>1]}S{[1P<+][_H<<_>]}&11+1] (go-to-input-start-for_C, _) -> (go-to-input-start-for_C, _, right)$
[C~P{[_C>1]C{[+S>_][1C>1]}S{[1P<+][_H<_<]>}&11+1] (go-to-input-start-for_C, ]) -> (go-to-input-start-for_C, ], right)$
[C~P{[_C>1]C{[+S>_][1C>1]}S{[1P<+][_H<_]<}>&11+1] (go-to-input-start-for_C, }) -> (go-to-input-start-for_C, }, right)$
[C~P{[_C>1]C{[+S>_][1C>1]}S{[1P<+][_H<_]}<&>11+1] (go-to-input-start-for_C, &) -> (read-tape-for_C, &, right)$
[C~P{[_C>1]C{[+S>_][1C>1]}S{[1P<+][_H<_]}&<1>1+1] (read-tape-for_C, 1) -> (scanleft-to-states-decleration-for_C(1), ^, left)$
[C~P{[_C>1]C{[+S>_][1C>1]}S{[1P<+][_H<_]}<&>^1+1] (scanleft-to-states-decleration-for_C(1), &) -> (scanleft-to-states-decleration-for_C(1), &, left)$
[C~P{[_C>1]C{[+S>_][1C>1]}S{[1P<+][_H<_]<}>&^1+1] (scanleft-to-states-decleration-for_C(1), }) -> (scanleft-to-states-decleration-for_C(1), }, left)$
[C~P{[_C>1]C{[+S>_][1C>1]}S{[1P<+][_H<_<]>}&^1+1] (scanleft-to-states-decleration-for_C(1), ]) -> (scanleft-to-states-decleration-for_C(1), ], left)$
[C~P{[_C>1]C{[+S>_][1C>1]}S{[1P<+][_H<<_>]}&^1+1] (scanleft-to-states-decleration-for_C(1), _) -> (scanleft-to-states-decleration-for_C(1), _, left)$
[C~P{[_C>1]C{[+S>_][1C>1]}S{[1P<+][_H<<>_]}&^1+1] (scanleft-to-states-decleration-for_C(1), <) -> (scanleft-to-states-decleration-for_C(1), <, left)$
[C~P{[_C>1]C{[+S>_][1C>1]}S{[1P<+][_<H><_]}&^1+1] (scanleft-to-states-decleration-for_C(1), H) -> (scanleft-to-states-decleration-for_C(1), H, left)$
[C~P{[_C>1]C{[+S>_][1C>1]}S{[1P<+][<_>H<_]}&^1+1] (scanleft-to-states-decleration-for_C(1), _) -> (scanleft-to-states-decleration-for_C(1), _, left)$
[C~P{[_C>1]C{[+S>_][1C>1]}S{[1P<+]<[>_H<_]}&^1+1] (scanleft-to-states-decleration-for_C(1), [) -> (scanleft-to-states-decleration-for_C(1), [, left)$
[C~P{[_C>1]C{[+S>_][1C>1]}S{[1P<+<]>[_H<_]}&^1+1] (scanleft-to-states-decleration-for_C(1), ]) -> (scanleft-to-states-decleration-for_C(1), ], left)$
[C~P{[_C>1]C{[+S>_][1C>1]}S{[1P<<+>][_H<_]}&^1+1] (scanleft-to-states-decleration-for_C(1), +) -> (scanleft-to-states-decleration-for_C(1), +, left)$
[C~P{[_C>1]C{[+S>_][1C>1]}S{[1P<<>+][_H<_]}&^1+1] (scanleft-to-states-decleration-for_C(1), <) -> (scanleft-to-states-decleration-for_C(1), <, left)$
[C~P{[_C>1]C{[+S>_][1C>1]}S{[1<P><+][_H<_]}&^1+1] (scanleft-to-states-decleration-for_C(1), P) -> (scanleft-to-states-decleration-for_C(1), P, left)$
[C~P{[_C>1]C{[+S>_][1C>1]}S{[<1>P<+][_H<_]}&^1+1] (scanleft-to-states-decleration-for_C(1), 1) -> (scanleft-to-states-decleration-for_C(1), 1, left)$
[C~P{[_C>1]C{[+S>_][1C>1]}S{<[>1P<+][_H<_]}&^1+1] (scanleft-to-states-decleration-for_C(1), [) -> (scanleft-to-states-decleration-for_C(1), [, left)$
[C~P{[_C>1]C{[+S>_][1C>1]}S<{>[1P<+][_H<_]}&^1+1] (scanleft-to-states-decleration-for_C(1), {) -> (scanleft-to-states-decleration-for_C(1), {, left)$
[C~P{[_C>1]C{[+S>_][1C>1]}<S>{[1P<+][_H<_]}&^1+1] (scanleft-to-states-decleration-for_C(1), S) -> (scanleft-to-states-decleration-for_C(1), S, left)$
[C~P{[_C>1]C{[+S>_][1C>1]<}>S{[1P<+][_H<_]}&^1+1] (scanleft-to-states-decleration-for_C(1), }) -> (scanleft-to-states-decleration-for_C(1), }, left)$
[C~P{[_C>1]C{[+S>_][1C>1<]>}S{[1P<+][_H<_]}&^1+1] (scanleft-to-states-decleration-for_C(1), ]) -> (scanleft-to-states-decleration-for_C(1), ], left)$
[C~P{[_C>1]C{[+S>_][1C><1>]}S{[1P<+][_H<_]}&^1+1] (scanleft-to-states-decleration-for_C(1), 1) -> (scanleft-to-states-decleration-for_C(1), 1, left)$
[C~P{[_C>1]C{[+S>_][1C<>>1]}S{[1P<+][_H<_]}&^1+1] (scanleft-to-states-decleration-for_C(1), >) -> (scanleft-to-states-decleration-for_C(1), >, left)$
[C~P{[_C>1]C{[+S>_][1<C>>1]}S{[1P<+][_H<_]}&^1+1] (scanleft-to-states-decleration-for_C(1), C) -> (scanleft-to-states-decleration-for_C(1), C, left)$
[C~P{[_C>1]C{[+S>_][<1>C>1]}S{[1P<+][_H<_]}&^1+1] (scanleft-to-states-decleration-for_C(1), 1) -> (scanleft-to-states-decleration-for_C(1), 1, left)$
[C~P{[_C>1]C{[+S>_]<[>1C>1]}S{[1P<+][_H<_]}&^1+1] (scanleft-to-states-decleration-for_C(1), [) -> (scanleft-to-states-decleration-for_C(1), [, left)$
[C~P{[_C>1]C{[+S>_<]>[1C>1]}S{[1P<+][_H<_]}&^1+1] (scanleft-to-states-decleration-for_C(1), ]) -> (scanleft-to-states-decleration-for_C(1), ], left)$
[C~P{[_C>1]C{[+S><_>][1C>1]}S{[1P<+][_H<_]}&^1+1] (scanleft-to-states-decleration-for_C(1), _) -> (scanleft-to-states-decleration-for_C(1), _, left)$
[C~P{[_C>1]C{[+S<>>_][1C>1]}S{[1P<+][_H<_]}&^1+1] (scanleft-to-states-decleration-for_C(1), >) -> (scanleft-to-states-decleration-for_C(1), >, left)$
[C~P{[_C>1]C{[+<S>>_][1C>1]}S{[1P<+][_H<_]}&^1+1] (scanleft-to-states-decleration-for_C(1), S) -> (scanleft-to-states-decleration-for_C(1), S, left)$
[C~P{[_C>1]C{[<+>S>_][1C>1]}S{[1P<+][_H<_]}&^1+1] (scanleft-to-states-decleration-for_C(1), +) -> (scanleft-to-states-decleration-for_C(1), +, left)$
[C~P{[_C>1]C{<[>+S>_][1C>1]}S{[1P<+][_H<_]}&^1+1] (scanleft-to-states-decleration-for_C(1), [) -> (scanleft-to-states-decleration-for_C(1), [, left)$
[C~P{[_C>1]C<{>[+S>_][1C>1]}S{[1P<+][_H<_]}&^1+1] (scanleft-to-states-decleration-for_C(1), {) -> (scanleft-to-states-decleration-for_C(1), {, left)$
[C~P{[_C>1]<C>{[+S>_][1C>1]}S{[1P<+][_H<_]}&^1+1] (scanleft-to-states-decleration-for_C(1), C) -> (scanleft-to-states-decleration-for_C(1), C, left)$
[C~P{[_C>1<]>C{[+S>_][1C>1]}S{[1P<+][_H<_]}&^1+1] (scanleft-to-states-decleration-for_C(1), ]) -> (scanleft-to-states-decleration-for_C(1), ], left)$
[C~P{[_C><1>]C{[+S>_][1C>1]}S{[1P<+][_H<_]}&^1+1] (scanleft-to-states-decleration-for_C(1), 1) -> (scanleft-to-states-decleration-for_C(1), 1, left)$
[C~P{[_C<>>1]C{[+S>_][1C>1]}S{[1P<+][_H<_]}&^1+1] (scanleft-to-states-decleration-for_C(1), >) -> (scanleft-to-states-decleration-for_C(1), >, left)$
[C~P{[_<C>>1]C{[+S>_][1C>1]}S{[1P<+][_H<_]}&^1+1] (scanleft-to-states-decleration-for_C(1), C) -> (scanleft-to-states-decleration-for_C(1), C, left)$
[C~P{[<_>C>1]C{[+S>_][1C>1]}S{[1P<+][_H<_]}&^1+1] (scanleft-to-states-decleration-for_C(1), _) -> (scanleft-to-states-decleration-for_C(1), _, left)$
[C~P{<[>_C>1]C{[+S>_][1C>1]}S{[1P<+][_H<_]}&^1+1] (scanleft-to-states-decleration-for_C(1), [) -> (scanleft-to-states-decleration-for_C(1), [, left)$
[C~P<{>[_C>1]C{[+S>_][1C>1]}S{[1P<+][_H<_]}&^1+1] (scanleft-to-states-decleration-for_C(1), {) -> (scanleft-to-states-decleration-for_C(1), {, left)$
[C~<P>{[_C>1]C{[+S>_][1C>1]}S{[1P<+][_H<_]}&^1+1] (scanleft-to-states-decleration-for_C(1), P) -> (scanleft-to-states-decleration-for_C(1), P, left)$
[C<~>P{[_C>1]C{[+S>_][1C>1]}S{[1P<+][_H<_]}&^1+1] (scanleft-to-states-decleration-for_C(1), ~) -> (find-state_C(1), ~, right)$
[C~<P>{[_C>1]C{[+S>_][1C>1]}S{[1P<+][_H<_]}&^1+1] (find-state_C(1), P) -> (scanright-to-next-state-definition_C(1), P, right)$
[C~P<{>[_C>1]C{[+S>_][1C>1]}S{[1P<+][_H<_]}&^1+1] (scanright-to-next-state-definition_C(1), {) -> (scanright-to-next-state-definition_C(1), {, right)$
[C~P{<[>_C>1]C{[+S>_][1C>1]}S{[1P<+][_H<_]}&^1+1] (scanright-to-next-state-definition_C(1), [) -> (scanright-to-next-state-definition_C(1), [, right)$
[C~P{[<_>C>1]C{[+S>_][1C>1]}S{[1P<+][_H<_]}&^1+1] (scanright-to-next-state-definition_C(1), _) -> (scanright-to-next-state-definition_C(1), _, right)$
[C~P{[_<C>>1]C{[+S>_][1C>1]}S{[1P<+][_H<_]}&^1+1] (scanright-to-next-state-definition_C(1), C) -> (scanright-to-next-state-definition_C(1), C, right)$
[C~P{[_C<>>1]C{[+S>_][1C>1]}S{[1P<+][_H<_]}&^1+1] (scanright-to-next-state-definition_C(1), >) -> (scanright-to-next-state-definition_C(1), >, right)$
[C~P{[_C><1>]C{[+S>_][1C>1]}S{[1P<+][_H<_]}&^1+1] (scanright-to-next-state-definition_C(1), 1) -> (scanright-to-next-state-definition_C(1), 1, right)$
[C~P{[_C>1<]>C{[+S>_][1C>1]}S{[1P<+][_H<_]}&^1+1] (scanright-to-next-state-definition_C(1), ]) -> (scanright-to-next-state-definition_C(1), ], right)$
[C~P{[_C>1]<C>{[+S>_][1C>1]}S{[1P<+][_H<_]}&^1+1] (scanright-to-next-state-definition_C(1), C) -> (scanright-to-next-state-definition_C(1), C, right)$
[C~P{[_C>1]C<{>[+S>_][1C>1]}S{[1P<+][_H<_]}&^1+1] (scanright-to-next-state-definition_C(1), {) -> (scanright-to-next-state-definition_C(1), {, right)$
[C~P{[_C>1]C{<[>+S>_][1C>1]}S{[1P<+][_H<_]}&^1+1] (scanright-to-next-state-definition_C(1), [) -> (scanright-to-next-state-definition_C(1), [, right)$
[C~P{[_C>1]C{[<+>S>_][1C>1]}S{[1P<+][_H<_]}&^1+1] (scanright-to-next-state-definition_C(1), +) -> (scanright-to-next-state-definition_C(1), +, right)$
[C~P{[_C>1]C{[+<S>>_][1C>1]}S{[1P<+][_H<_]}&^1+1] (scanright-to-next-state-definition_C(1), S) -> (scanright-to-next-state-definition_C(1), S, right)$
[C~P{[_C>1]C{[+S<>>_][1C>1]}S{[1P<+][_H<_]}&^1+1] (scanright-to-next-state-definition_C(1), >) -> (scanright-to-next-state-definition_C(1), >, right)$
[C~P{[_C>1]C{[+S><_>][1C>1]}S{[1P<+][_H<_]}&^1+1] (scanright-to-next-state-definition_C(1), _) -> (scanright-to-next-state-definition_C(1), _, right)$
[C~P{[_C>1]C{[+S>_<]>[1C>1]}S{[1P<+][_H<_]}&^1+1] (scanright-to-next-state-definition_C(1), ]) -> (scanright-to-next-state-definition_C(1), ], right)$
[C~P{[_C>1]C{[+S>_]<[>1C>1]}S{[1P<+][_H<_]}&^1+1] (scanright-to-next-state-definition_C(1), [) -> (scanright-to-next-state-definition_C(1), [, right)$
[C~P{[_C>1]C{[+S>_][<1>C>1]}S{[1P<+][_H<_]}&^1+1] (scanright-to-next-state-definition_C(1), 1) -> (scanright-to-next-state-definition_C(1), 1, right)$
[C~P{[_C>1]C{[+S>_][1<C>>1]}S{[1P<+][_H<_]}&^1+1] (scanright-to-next-state-definition_C(1), C) -> (scanright-to-next-state-definition_C(1), C, right)$
[C~P{[_C>1]C{[+S>_][1C<>>1]}S{[1P<+][_H<_]}&^1+1] (scanright-to-next-state-definition_C(1), >) -> (scanright-to-next-state-definition_C(1), >, right)$
[C~P{[_C>1]C{[+S>_][1C><1>]}S{[1P<+][_H<_]}&^1+1] (scanright-to-next-state-definition_C(1), 1) -> (scanright-to-next-state-definition_C(1), 1, right)$
[C~P{[_C>1]C{[+S>_][1C>1<]>}S{[1P<+][_H<_]}&^1+1] (scanright-to-next-state-definition_C(1), ]) -> (scanright-to-next-state-definition_C(1), ], right)$
[C~P{[_C>1]C{[+S>_][1C>1]<}>S{[1P<+][_H<_]}&^1+1] (scanright-to-next-state-definition_C(1), }) -> (find-state_C(1), }, right)$
[C~P{[_C>1]C{[+S>_][1C>1]}<S>{[1P<+][_H<_]}&^1+1] (find-state_C(1), S) -> (scanright-to-next-state-definition_C(1), S, right)$
[C~P{[_C>1]C{[+S>_][1C>1]}S<{>[1P<+][_H<_]}&^1+1] (scanright-to-next-state-definition_C(1), {) -> (scanright-to-next-state-definition_C(1), {, right)$
[C~P{[_C>1]C{[+S>_][1C>1]}S{<[>1P<+][_H<_]}&^1+1] (scanright-to-next-state-definition_C(1), [) -> (scanright-to-next-state-definition_C(1), [, right)$
[C~P{[_C>1]C{[+S>_][1C>1]}S{[<1>P<+][_H<_]}&^1+1] (scanright-to-next-state-definition_C(1), 1) -> (scanright-to-next-state-definition_C(1), 1, right)$
[C~P{[_C>1]C{[+S>_][1C>1]}S{[1<P><+][_H<_]}&^1+1] (scanright-to-next-state-definition_C(1), P) -> (scanright-to-next-state-definition_C(1), P, right)$
[C~P{[_C>1]C{[+S>_][1C>1]}S{[1P<<>+][_H<_]}&^1+1] (scanright-to-next-state-definition_C(1), <) -> (scanright-to-next-state-definition_C(1), <, right)$
[C~P{[_C>1]C{[+S>_][1C>1]}S{[1P<<+>][_H<_]}&^1+1] (scanright-to-next-state-definition_C(1), +) -> (scanright-to-next-state-definition_C(1), +, right)$
[C~P{[_C>1]C{[+S>_][1C>1]}S{[1P<+<]>[_H<_]}&^1+1] (scanright-to-next-state-definition_C(1), ]) -> (scanright-to-next-state-definition_C(1), ], right)$
[C~P{[_C>1]C{[+S>_][1C>1]}S{[1P<+]<[>_H<_]}&^1+1] (scanright-to-next-state-definition_C(1), [) -> (scanright-to-next-state-definition_C(1), [, right)$
[C~P{[_C>1]C{[+S>_][1C>1]}S{[1P<+][<_>H<_]}&^1+1] (scanright-to-next-state-definition_C(1), _) -> (scanright-to-next-state-definition_C(1), _, right)$
[C~P{[_C>1]C{[+S>_][1C>1]}S{[1P<+][_<H><_]}&^1+1] (scanright-to-next-state-definition_C(1), H) -> (scanright-to-next-state-definition_C(1), H, right)$
[C~P{[_C>1]C{[+S>_][1C>1]}S{[1P<+][_H<<>_]}&^1+1] (scanright-to-next-state-definition_C(1), <) -> (scanright-to-next-state-definition_C(1), <, right)$
[C~P{[_C>1]C{[+S>_][1C>1]}S{[1P<+][_H<<_>]}&^1+1] (scanright-to-next-state-definition_C(1), _) -> (scanright-to-next-state-definition_C(1), _, right)$
[C~P{[_C>1]C{[+S>_][1C>1]}S{[1P<+][_H<_<]>}&^1+1] (scanright-to-next-state-definition_C(1), ]) -> (scanright-to-next-state-definition_C(1), ], right)$
[C~P{[_C>1]C{[+S>_][1C>1]}S{[1P<+][_H<_]<}>&^1+1] (scanright-to-next-state-definition_C(1), }) -> (find-state_C(1), }, right)$
[C~P{[_C>1]C{[+S>_][1C>1]}S{[1P<+][_H<_]}<&>^1+1] (find-state_C(1), &) -> BLOCKED$
Machine is blocked no more transitions available$
Interpreter closing...$'
}

@test "Invalid transitions decleration" {
    run bash -c './_build/default/bin/turing our-machines/unary_add_utm.json  "C~P{[_C>1]}C{[+S>_[1C>1]}S{[1P<+][_H<_]}&11+1" | cat -e'
    assert_output 'Interpreter starting...$
[<C>~P{[_C>1]}C{[+S>_[1C>1]}S{[1P<+][_H<_]}&11+1] (retrieve_initial_state, C) -> (go-to-input-start-for_C, C, right)$
[C<~>P{[_C>1]}C{[+S>_[1C>1]}S{[1P<+][_H<_]}&11+1] (go-to-input-start-for_C, ~) -> (go-to-input-start-for_C, ~, right)$
[C~<P>{[_C>1]}C{[+S>_[1C>1]}S{[1P<+][_H<_]}&11+1] (go-to-input-start-for_C, P) -> (go-to-input-start-for_C, P, right)$
[C~P<{>[_C>1]}C{[+S>_[1C>1]}S{[1P<+][_H<_]}&11+1] (go-to-input-start-for_C, {) -> (go-to-input-start-for_C, {, right)$
[C~P{<[>_C>1]}C{[+S>_[1C>1]}S{[1P<+][_H<_]}&11+1] (go-to-input-start-for_C, [) -> (go-to-input-start-for_C, [, right)$
[C~P{[<_>C>1]}C{[+S>_[1C>1]}S{[1P<+][_H<_]}&11+1] (go-to-input-start-for_C, _) -> (go-to-input-start-for_C, _, right)$
[C~P{[_<C>>1]}C{[+S>_[1C>1]}S{[1P<+][_H<_]}&11+1] (go-to-input-start-for_C, C) -> (go-to-input-start-for_C, C, right)$
[C~P{[_C<>>1]}C{[+S>_[1C>1]}S{[1P<+][_H<_]}&11+1] (go-to-input-start-for_C, >) -> (go-to-input-start-for_C, >, right)$
[C~P{[_C><1>]}C{[+S>_[1C>1]}S{[1P<+][_H<_]}&11+1] (go-to-input-start-for_C, 1) -> (go-to-input-start-for_C, 1, right)$
[C~P{[_C>1<]>}C{[+S>_[1C>1]}S{[1P<+][_H<_]}&11+1] (go-to-input-start-for_C, ]) -> (go-to-input-start-for_C, ], right)$
[C~P{[_C>1]<}>C{[+S>_[1C>1]}S{[1P<+][_H<_]}&11+1] (go-to-input-start-for_C, }) -> (go-to-input-start-for_C, }, right)$
[C~P{[_C>1]}<C>{[+S>_[1C>1]}S{[1P<+][_H<_]}&11+1] (go-to-input-start-for_C, C) -> (go-to-input-start-for_C, C, right)$
[C~P{[_C>1]}C<{>[+S>_[1C>1]}S{[1P<+][_H<_]}&11+1] (go-to-input-start-for_C, {) -> (go-to-input-start-for_C, {, right)$
[C~P{[_C>1]}C{<[>+S>_[1C>1]}S{[1P<+][_H<_]}&11+1] (go-to-input-start-for_C, [) -> (go-to-input-start-for_C, [, right)$
[C~P{[_C>1]}C{[<+>S>_[1C>1]}S{[1P<+][_H<_]}&11+1] (go-to-input-start-for_C, +) -> (go-to-input-start-for_C, +, right)$
[C~P{[_C>1]}C{[+<S>>_[1C>1]}S{[1P<+][_H<_]}&11+1] (go-to-input-start-for_C, S) -> (go-to-input-start-for_C, S, right)$
[C~P{[_C>1]}C{[+S<>>_[1C>1]}S{[1P<+][_H<_]}&11+1] (go-to-input-start-for_C, >) -> (go-to-input-start-for_C, >, right)$
[C~P{[_C>1]}C{[+S><_>[1C>1]}S{[1P<+][_H<_]}&11+1] (go-to-input-start-for_C, _) -> (go-to-input-start-for_C, _, right)$
[C~P{[_C>1]}C{[+S>_<[>1C>1]}S{[1P<+][_H<_]}&11+1] (go-to-input-start-for_C, [) -> (go-to-input-start-for_C, [, right)$
[C~P{[_C>1]}C{[+S>_[<1>C>1]}S{[1P<+][_H<_]}&11+1] (go-to-input-start-for_C, 1) -> (go-to-input-start-for_C, 1, right)$
[C~P{[_C>1]}C{[+S>_[1<C>>1]}S{[1P<+][_H<_]}&11+1] (go-to-input-start-for_C, C) -> (go-to-input-start-for_C, C, right)$
[C~P{[_C>1]}C{[+S>_[1C<>>1]}S{[1P<+][_H<_]}&11+1] (go-to-input-start-for_C, >) -> (go-to-input-start-for_C, >, right)$
[C~P{[_C>1]}C{[+S>_[1C><1>]}S{[1P<+][_H<_]}&11+1] (go-to-input-start-for_C, 1) -> (go-to-input-start-for_C, 1, right)$
[C~P{[_C>1]}C{[+S>_[1C>1<]>}S{[1P<+][_H<_]}&11+1] (go-to-input-start-for_C, ]) -> (go-to-input-start-for_C, ], right)$
[C~P{[_C>1]}C{[+S>_[1C>1]<}>S{[1P<+][_H<_]}&11+1] (go-to-input-start-for_C, }) -> (go-to-input-start-for_C, }, right)$
[C~P{[_C>1]}C{[+S>_[1C>1]}<S>{[1P<+][_H<_]}&11+1] (go-to-input-start-for_C, S) -> (go-to-input-start-for_C, S, right)$
[C~P{[_C>1]}C{[+S>_[1C>1]}S<{>[1P<+][_H<_]}&11+1] (go-to-input-start-for_C, {) -> (go-to-input-start-for_C, {, right)$
[C~P{[_C>1]}C{[+S>_[1C>1]}S{<[>1P<+][_H<_]}&11+1] (go-to-input-start-for_C, [) -> (go-to-input-start-for_C, [, right)$
[C~P{[_C>1]}C{[+S>_[1C>1]}S{[<1>P<+][_H<_]}&11+1] (go-to-input-start-for_C, 1) -> (go-to-input-start-for_C, 1, right)$
[C~P{[_C>1]}C{[+S>_[1C>1]}S{[1<P><+][_H<_]}&11+1] (go-to-input-start-for_C, P) -> (go-to-input-start-for_C, P, right)$
[C~P{[_C>1]}C{[+S>_[1C>1]}S{[1P<<>+][_H<_]}&11+1] (go-to-input-start-for_C, <) -> (go-to-input-start-for_C, <, right)$
[C~P{[_C>1]}C{[+S>_[1C>1]}S{[1P<<+>][_H<_]}&11+1] (go-to-input-start-for_C, +) -> (go-to-input-start-for_C, +, right)$
[C~P{[_C>1]}C{[+S>_[1C>1]}S{[1P<+<]>[_H<_]}&11+1] (go-to-input-start-for_C, ]) -> (go-to-input-start-for_C, ], right)$
[C~P{[_C>1]}C{[+S>_[1C>1]}S{[1P<+]<[>_H<_]}&11+1] (go-to-input-start-for_C, [) -> (go-to-input-start-for_C, [, right)$
[C~P{[_C>1]}C{[+S>_[1C>1]}S{[1P<+][<_>H<_]}&11+1] (go-to-input-start-for_C, _) -> (go-to-input-start-for_C, _, right)$
[C~P{[_C>1]}C{[+S>_[1C>1]}S{[1P<+][_<H><_]}&11+1] (go-to-input-start-for_C, H) -> (go-to-input-start-for_C, H, right)$
[C~P{[_C>1]}C{[+S>_[1C>1]}S{[1P<+][_H<<>_]}&11+1] (go-to-input-start-for_C, <) -> (go-to-input-start-for_C, <, right)$
[C~P{[_C>1]}C{[+S>_[1C>1]}S{[1P<+][_H<<_>]}&11+1] (go-to-input-start-for_C, _) -> (go-to-input-start-for_C, _, right)$
[C~P{[_C>1]}C{[+S>_[1C>1]}S{[1P<+][_H<_<]>}&11+1] (go-to-input-start-for_C, ]) -> (go-to-input-start-for_C, ], right)$
[C~P{[_C>1]}C{[+S>_[1C>1]}S{[1P<+][_H<_]<}>&11+1] (go-to-input-start-for_C, }) -> (go-to-input-start-for_C, }, right)$
[C~P{[_C>1]}C{[+S>_[1C>1]}S{[1P<+][_H<_]}<&>11+1] (go-to-input-start-for_C, &) -> (read-tape-for_C, &, right)$
[C~P{[_C>1]}C{[+S>_[1C>1]}S{[1P<+][_H<_]}&<1>1+1] (read-tape-for_C, 1) -> (scanleft-to-states-decleration-for_C(1), ^, left)$
[C~P{[_C>1]}C{[+S>_[1C>1]}S{[1P<+][_H<_]}<&>^1+1] (scanleft-to-states-decleration-for_C(1), &) -> (scanleft-to-states-decleration-for_C(1), &, left)$
[C~P{[_C>1]}C{[+S>_[1C>1]}S{[1P<+][_H<_]<}>&^1+1] (scanleft-to-states-decleration-for_C(1), }) -> (scanleft-to-states-decleration-for_C(1), }, left)$
[C~P{[_C>1]}C{[+S>_[1C>1]}S{[1P<+][_H<_<]>}&^1+1] (scanleft-to-states-decleration-for_C(1), ]) -> (scanleft-to-states-decleration-for_C(1), ], left)$
[C~P{[_C>1]}C{[+S>_[1C>1]}S{[1P<+][_H<<_>]}&^1+1] (scanleft-to-states-decleration-for_C(1), _) -> (scanleft-to-states-decleration-for_C(1), _, left)$
[C~P{[_C>1]}C{[+S>_[1C>1]}S{[1P<+][_H<<>_]}&^1+1] (scanleft-to-states-decleration-for_C(1), <) -> (scanleft-to-states-decleration-for_C(1), <, left)$
[C~P{[_C>1]}C{[+S>_[1C>1]}S{[1P<+][_<H><_]}&^1+1] (scanleft-to-states-decleration-for_C(1), H) -> (scanleft-to-states-decleration-for_C(1), H, left)$
[C~P{[_C>1]}C{[+S>_[1C>1]}S{[1P<+][<_>H<_]}&^1+1] (scanleft-to-states-decleration-for_C(1), _) -> (scanleft-to-states-decleration-for_C(1), _, left)$
[C~P{[_C>1]}C{[+S>_[1C>1]}S{[1P<+]<[>_H<_]}&^1+1] (scanleft-to-states-decleration-for_C(1), [) -> (scanleft-to-states-decleration-for_C(1), [, left)$
[C~P{[_C>1]}C{[+S>_[1C>1]}S{[1P<+<]>[_H<_]}&^1+1] (scanleft-to-states-decleration-for_C(1), ]) -> (scanleft-to-states-decleration-for_C(1), ], left)$
[C~P{[_C>1]}C{[+S>_[1C>1]}S{[1P<<+>][_H<_]}&^1+1] (scanleft-to-states-decleration-for_C(1), +) -> (scanleft-to-states-decleration-for_C(1), +, left)$
[C~P{[_C>1]}C{[+S>_[1C>1]}S{[1P<<>+][_H<_]}&^1+1] (scanleft-to-states-decleration-for_C(1), <) -> (scanleft-to-states-decleration-for_C(1), <, left)$
[C~P{[_C>1]}C{[+S>_[1C>1]}S{[1<P><+][_H<_]}&^1+1] (scanleft-to-states-decleration-for_C(1), P) -> (scanleft-to-states-decleration-for_C(1), P, left)$
[C~P{[_C>1]}C{[+S>_[1C>1]}S{[<1>P<+][_H<_]}&^1+1] (scanleft-to-states-decleration-for_C(1), 1) -> (scanleft-to-states-decleration-for_C(1), 1, left)$
[C~P{[_C>1]}C{[+S>_[1C>1]}S{<[>1P<+][_H<_]}&^1+1] (scanleft-to-states-decleration-for_C(1), [) -> (scanleft-to-states-decleration-for_C(1), [, left)$
[C~P{[_C>1]}C{[+S>_[1C>1]}S<{>[1P<+][_H<_]}&^1+1] (scanleft-to-states-decleration-for_C(1), {) -> (scanleft-to-states-decleration-for_C(1), {, left)$
[C~P{[_C>1]}C{[+S>_[1C>1]}<S>{[1P<+][_H<_]}&^1+1] (scanleft-to-states-decleration-for_C(1), S) -> (scanleft-to-states-decleration-for_C(1), S, left)$
[C~P{[_C>1]}C{[+S>_[1C>1]<}>S{[1P<+][_H<_]}&^1+1] (scanleft-to-states-decleration-for_C(1), }) -> (scanleft-to-states-decleration-for_C(1), }, left)$
[C~P{[_C>1]}C{[+S>_[1C>1<]>}S{[1P<+][_H<_]}&^1+1] (scanleft-to-states-decleration-for_C(1), ]) -> (scanleft-to-states-decleration-for_C(1), ], left)$
[C~P{[_C>1]}C{[+S>_[1C><1>]}S{[1P<+][_H<_]}&^1+1] (scanleft-to-states-decleration-for_C(1), 1) -> (scanleft-to-states-decleration-for_C(1), 1, left)$
[C~P{[_C>1]}C{[+S>_[1C<>>1]}S{[1P<+][_H<_]}&^1+1] (scanleft-to-states-decleration-for_C(1), >) -> (scanleft-to-states-decleration-for_C(1), >, left)$
[C~P{[_C>1]}C{[+S>_[1<C>>1]}S{[1P<+][_H<_]}&^1+1] (scanleft-to-states-decleration-for_C(1), C) -> (scanleft-to-states-decleration-for_C(1), C, left)$
[C~P{[_C>1]}C{[+S>_[<1>C>1]}S{[1P<+][_H<_]}&^1+1] (scanleft-to-states-decleration-for_C(1), 1) -> (scanleft-to-states-decleration-for_C(1), 1, left)$
[C~P{[_C>1]}C{[+S>_<[>1C>1]}S{[1P<+][_H<_]}&^1+1] (scanleft-to-states-decleration-for_C(1), [) -> (scanleft-to-states-decleration-for_C(1), [, left)$
[C~P{[_C>1]}C{[+S><_>[1C>1]}S{[1P<+][_H<_]}&^1+1] (scanleft-to-states-decleration-for_C(1), _) -> (scanleft-to-states-decleration-for_C(1), _, left)$
[C~P{[_C>1]}C{[+S<>>_[1C>1]}S{[1P<+][_H<_]}&^1+1] (scanleft-to-states-decleration-for_C(1), >) -> (scanleft-to-states-decleration-for_C(1), >, left)$
[C~P{[_C>1]}C{[+<S>>_[1C>1]}S{[1P<+][_H<_]}&^1+1] (scanleft-to-states-decleration-for_C(1), S) -> (scanleft-to-states-decleration-for_C(1), S, left)$
[C~P{[_C>1]}C{[<+>S>_[1C>1]}S{[1P<+][_H<_]}&^1+1] (scanleft-to-states-decleration-for_C(1), +) -> (scanleft-to-states-decleration-for_C(1), +, left)$
[C~P{[_C>1]}C{<[>+S>_[1C>1]}S{[1P<+][_H<_]}&^1+1] (scanleft-to-states-decleration-for_C(1), [) -> (scanleft-to-states-decleration-for_C(1), [, left)$
[C~P{[_C>1]}C<{>[+S>_[1C>1]}S{[1P<+][_H<_]}&^1+1] (scanleft-to-states-decleration-for_C(1), {) -> (scanleft-to-states-decleration-for_C(1), {, left)$
[C~P{[_C>1]}<C>{[+S>_[1C>1]}S{[1P<+][_H<_]}&^1+1] (scanleft-to-states-decleration-for_C(1), C) -> (scanleft-to-states-decleration-for_C(1), C, left)$
[C~P{[_C>1]<}>C{[+S>_[1C>1]}S{[1P<+][_H<_]}&^1+1] (scanleft-to-states-decleration-for_C(1), }) -> (scanleft-to-states-decleration-for_C(1), }, left)$
[C~P{[_C>1<]>}C{[+S>_[1C>1]}S{[1P<+][_H<_]}&^1+1] (scanleft-to-states-decleration-for_C(1), ]) -> (scanleft-to-states-decleration-for_C(1), ], left)$
[C~P{[_C><1>]}C{[+S>_[1C>1]}S{[1P<+][_H<_]}&^1+1] (scanleft-to-states-decleration-for_C(1), 1) -> (scanleft-to-states-decleration-for_C(1), 1, left)$
[C~P{[_C<>>1]}C{[+S>_[1C>1]}S{[1P<+][_H<_]}&^1+1] (scanleft-to-states-decleration-for_C(1), >) -> (scanleft-to-states-decleration-for_C(1), >, left)$
[C~P{[_<C>>1]}C{[+S>_[1C>1]}S{[1P<+][_H<_]}&^1+1] (scanleft-to-states-decleration-for_C(1), C) -> (scanleft-to-states-decleration-for_C(1), C, left)$
[C~P{[<_>C>1]}C{[+S>_[1C>1]}S{[1P<+][_H<_]}&^1+1] (scanleft-to-states-decleration-for_C(1), _) -> (scanleft-to-states-decleration-for_C(1), _, left)$
[C~P{<[>_C>1]}C{[+S>_[1C>1]}S{[1P<+][_H<_]}&^1+1] (scanleft-to-states-decleration-for_C(1), [) -> (scanleft-to-states-decleration-for_C(1), [, left)$
[C~P<{>[_C>1]}C{[+S>_[1C>1]}S{[1P<+][_H<_]}&^1+1] (scanleft-to-states-decleration-for_C(1), {) -> (scanleft-to-states-decleration-for_C(1), {, left)$
[C~<P>{[_C>1]}C{[+S>_[1C>1]}S{[1P<+][_H<_]}&^1+1] (scanleft-to-states-decleration-for_C(1), P) -> (scanleft-to-states-decleration-for_C(1), P, left)$
[C<~>P{[_C>1]}C{[+S>_[1C>1]}S{[1P<+][_H<_]}&^1+1] (scanleft-to-states-decleration-for_C(1), ~) -> (find-state_C(1), ~, right)$
[C~<P>{[_C>1]}C{[+S>_[1C>1]}S{[1P<+][_H<_]}&^1+1] (find-state_C(1), P) -> (scanright-to-next-state-definition_C(1), P, right)$
[C~P<{>[_C>1]}C{[+S>_[1C>1]}S{[1P<+][_H<_]}&^1+1] (scanright-to-next-state-definition_C(1), {) -> (scanright-to-next-state-definition_C(1), {, right)$
[C~P{<[>_C>1]}C{[+S>_[1C>1]}S{[1P<+][_H<_]}&^1+1] (scanright-to-next-state-definition_C(1), [) -> (scanright-to-next-state-definition_C(1), [, right)$
[C~P{[<_>C>1]}C{[+S>_[1C>1]}S{[1P<+][_H<_]}&^1+1] (scanright-to-next-state-definition_C(1), _) -> (scanright-to-next-state-definition_C(1), _, right)$
[C~P{[_<C>>1]}C{[+S>_[1C>1]}S{[1P<+][_H<_]}&^1+1] (scanright-to-next-state-definition_C(1), C) -> (scanright-to-next-state-definition_C(1), C, right)$
[C~P{[_C<>>1]}C{[+S>_[1C>1]}S{[1P<+][_H<_]}&^1+1] (scanright-to-next-state-definition_C(1), >) -> (scanright-to-next-state-definition_C(1), >, right)$
[C~P{[_C><1>]}C{[+S>_[1C>1]}S{[1P<+][_H<_]}&^1+1] (scanright-to-next-state-definition_C(1), 1) -> (scanright-to-next-state-definition_C(1), 1, right)$
[C~P{[_C>1<]>}C{[+S>_[1C>1]}S{[1P<+][_H<_]}&^1+1] (scanright-to-next-state-definition_C(1), ]) -> (scanright-to-next-state-definition_C(1), ], right)$
[C~P{[_C>1]<}>C{[+S>_[1C>1]}S{[1P<+][_H<_]}&^1+1] (scanright-to-next-state-definition_C(1), }) -> (find-state_C(1), }, right)$
[C~P{[_C>1]}<C>{[+S>_[1C>1]}S{[1P<+][_H<_]}&^1+1] (find-state_C(1), C) -> (find-state-transition-for_C(1), C, right)$
[C~P{[_C>1]}C<{>[+S>_[1C>1]}S{[1P<+][_H<_]}&^1+1] (find-state-transition-for_C(1), {) -> (find-state-transition-for_C(1), {, right)$
[C~P{[_C>1]}C{<[>+S>_[1C>1]}S{[1P<+][_H<_]}&^1+1] (find-state-transition-for_C(1), [) -> (find-state-transition-for_C(1), [, right)$
[C~P{[_C>1]}C{[<+>S>_[1C>1]}S{[1P<+][_H<_]}&^1+1] (find-state-transition-for_C(1), +) -> (scanright-to-next-state-transition_C(1), +, right)$
[C~P{[_C>1]}C{[+<S>>_[1C>1]}S{[1P<+][_H<_]}&^1+1] (scanright-to-next-state-transition_C(1), S) -> (scanright-to-next-state-transition_C(1), S, right)$
[C~P{[_C>1]}C{[+S<>>_[1C>1]}S{[1P<+][_H<_]}&^1+1] (scanright-to-next-state-transition_C(1), >) -> (scanright-to-next-state-transition_C(1), >, right)$
[C~P{[_C>1]}C{[+S><_>[1C>1]}S{[1P<+][_H<_]}&^1+1] (scanright-to-next-state-transition_C(1), _) -> (scanright-to-next-state-transition_C(1), _, right)$
[C~P{[_C>1]}C{[+S>_<[>1C>1]}S{[1P<+][_H<_]}&^1+1] (scanright-to-next-state-transition_C(1), [) -> (scanright-to-next-state-transition_C(1), [, right)$
[C~P{[_C>1]}C{[+S>_[<1>C>1]}S{[1P<+][_H<_]}&^1+1] (scanright-to-next-state-transition_C(1), 1) -> (scanright-to-next-state-transition_C(1), 1, right)$
[C~P{[_C>1]}C{[+S>_[1<C>>1]}S{[1P<+][_H<_]}&^1+1] (scanright-to-next-state-transition_C(1), C) -> (scanright-to-next-state-transition_C(1), C, right)$
[C~P{[_C>1]}C{[+S>_[1C<>>1]}S{[1P<+][_H<_]}&^1+1] (scanright-to-next-state-transition_C(1), >) -> (scanright-to-next-state-transition_C(1), >, right)$
[C~P{[_C>1]}C{[+S>_[1C><1>]}S{[1P<+][_H<_]}&^1+1] (scanright-to-next-state-transition_C(1), 1) -> (scanright-to-next-state-transition_C(1), 1, right)$
[C~P{[_C>1]}C{[+S>_[1C>1<]>}S{[1P<+][_H<_]}&^1+1] (scanright-to-next-state-transition_C(1), ]) -> (find-state-transition-for_C(1), ], right)$
[C~P{[_C>1]}C{[+S>_[1C>1]<}>S{[1P<+][_H<_]}&^1+1] (find-state-transition-for_C(1), }) -> BLOCKED$
Machine is blocked no more transitions available$
Interpreter closing...$'
}

@test "Invalid initial state decleration" {
    run bash -c './_build/default/bin/turing our-machines/unary_add_utm.json  "~P{[_C>1]}C{[+S>_[1C>1]}S{[1P<+][_H<_]}&11+1" | cat -e'
    assert_output 'Interpreter starting...$
[<~>P{[_C>1]}C{[+S>_[1C>1]}S{[1P<+][_H<_]}&11+1] (retrieve_initial_state, ~) -> BLOCKED$
Machine is blocked no more transitions available$
Interpreter closing...$'
}

@test "Invalid input start decleration" {
    run bash -c './_build/default/bin/turing our-machines/unary_add_utm.json  "C~C{[+S>_][1C>1]}S{[1P<+][_H<_]}P{[_C>1]}11+1" | cat -e'
    assert_output 'Interpreter starting...$
[<C>~C{[+S>_][1C>1]}S{[1P<+][_H<_]}P{[_C>1]}11+1] (retrieve_initial_state, C) -> (go-to-input-start-for_C, C, right)$
[C<~>C{[+S>_][1C>1]}S{[1P<+][_H<_]}P{[_C>1]}11+1] (go-to-input-start-for_C, ~) -> (go-to-input-start-for_C, ~, right)$
[C~<C>{[+S>_][1C>1]}S{[1P<+][_H<_]}P{[_C>1]}11+1] (go-to-input-start-for_C, C) -> (go-to-input-start-for_C, C, right)$
[C~C<{>[+S>_][1C>1]}S{[1P<+][_H<_]}P{[_C>1]}11+1] (go-to-input-start-for_C, {) -> (go-to-input-start-for_C, {, right)$
[C~C{<[>+S>_][1C>1]}S{[1P<+][_H<_]}P{[_C>1]}11+1] (go-to-input-start-for_C, [) -> (go-to-input-start-for_C, [, right)$
[C~C{[<+>S>_][1C>1]}S{[1P<+][_H<_]}P{[_C>1]}11+1] (go-to-input-start-for_C, +) -> (go-to-input-start-for_C, +, right)$
[C~C{[+<S>>_][1C>1]}S{[1P<+][_H<_]}P{[_C>1]}11+1] (go-to-input-start-for_C, S) -> (go-to-input-start-for_C, S, right)$
[C~C{[+S<>>_][1C>1]}S{[1P<+][_H<_]}P{[_C>1]}11+1] (go-to-input-start-for_C, >) -> (go-to-input-start-for_C, >, right)$
[C~C{[+S><_>][1C>1]}S{[1P<+][_H<_]}P{[_C>1]}11+1] (go-to-input-start-for_C, _) -> (go-to-input-start-for_C, _, right)$
[C~C{[+S>_<]>[1C>1]}S{[1P<+][_H<_]}P{[_C>1]}11+1] (go-to-input-start-for_C, ]) -> (go-to-input-start-for_C, ], right)$
[C~C{[+S>_]<[>1C>1]}S{[1P<+][_H<_]}P{[_C>1]}11+1] (go-to-input-start-for_C, [) -> (go-to-input-start-for_C, [, right)$
[C~C{[+S>_][<1>C>1]}S{[1P<+][_H<_]}P{[_C>1]}11+1] (go-to-input-start-for_C, 1) -> (go-to-input-start-for_C, 1, right)$
[C~C{[+S>_][1<C>>1]}S{[1P<+][_H<_]}P{[_C>1]}11+1] (go-to-input-start-for_C, C) -> (go-to-input-start-for_C, C, right)$
[C~C{[+S>_][1C<>>1]}S{[1P<+][_H<_]}P{[_C>1]}11+1] (go-to-input-start-for_C, >) -> (go-to-input-start-for_C, >, right)$
[C~C{[+S>_][1C><1>]}S{[1P<+][_H<_]}P{[_C>1]}11+1] (go-to-input-start-for_C, 1) -> (go-to-input-start-for_C, 1, right)$
[C~C{[+S>_][1C>1<]>}S{[1P<+][_H<_]}P{[_C>1]}11+1] (go-to-input-start-for_C, ]) -> (go-to-input-start-for_C, ], right)$
[C~C{[+S>_][1C>1]<}>S{[1P<+][_H<_]}P{[_C>1]}11+1] (go-to-input-start-for_C, }) -> (go-to-input-start-for_C, }, right)$
[C~C{[+S>_][1C>1]}<S>{[1P<+][_H<_]}P{[_C>1]}11+1] (go-to-input-start-for_C, S) -> (go-to-input-start-for_C, S, right)$
[C~C{[+S>_][1C>1]}S<{>[1P<+][_H<_]}P{[_C>1]}11+1] (go-to-input-start-for_C, {) -> (go-to-input-start-for_C, {, right)$
[C~C{[+S>_][1C>1]}S{<[>1P<+][_H<_]}P{[_C>1]}11+1] (go-to-input-start-for_C, [) -> (go-to-input-start-for_C, [, right)$
[C~C{[+S>_][1C>1]}S{[<1>P<+][_H<_]}P{[_C>1]}11+1] (go-to-input-start-for_C, 1) -> (go-to-input-start-for_C, 1, right)$
[C~C{[+S>_][1C>1]}S{[1<P><+][_H<_]}P{[_C>1]}11+1] (go-to-input-start-for_C, P) -> (go-to-input-start-for_C, P, right)$
[C~C{[+S>_][1C>1]}S{[1P<<>+][_H<_]}P{[_C>1]}11+1] (go-to-input-start-for_C, <) -> (go-to-input-start-for_C, <, right)$
[C~C{[+S>_][1C>1]}S{[1P<<+>][_H<_]}P{[_C>1]}11+1] (go-to-input-start-for_C, +) -> (go-to-input-start-for_C, +, right)$
[C~C{[+S>_][1C>1]}S{[1P<+<]>[_H<_]}P{[_C>1]}11+1] (go-to-input-start-for_C, ]) -> (go-to-input-start-for_C, ], right)$
[C~C{[+S>_][1C>1]}S{[1P<+]<[>_H<_]}P{[_C>1]}11+1] (go-to-input-start-for_C, [) -> (go-to-input-start-for_C, [, right)$
[C~C{[+S>_][1C>1]}S{[1P<+][<_>H<_]}P{[_C>1]}11+1] (go-to-input-start-for_C, _) -> (go-to-input-start-for_C, _, right)$
[C~C{[+S>_][1C>1]}S{[1P<+][_<H><_]}P{[_C>1]}11+1] (go-to-input-start-for_C, H) -> (go-to-input-start-for_C, H, right)$
[C~C{[+S>_][1C>1]}S{[1P<+][_H<<>_]}P{[_C>1]}11+1] (go-to-input-start-for_C, <) -> (go-to-input-start-for_C, <, right)$
[C~C{[+S>_][1C>1]}S{[1P<+][_H<<_>]}P{[_C>1]}11+1] (go-to-input-start-for_C, _) -> (go-to-input-start-for_C, _, right)$
[C~C{[+S>_][1C>1]}S{[1P<+][_H<_<]>}P{[_C>1]}11+1] (go-to-input-start-for_C, ]) -> (go-to-input-start-for_C, ], right)$
[C~C{[+S>_][1C>1]}S{[1P<+][_H<_]<}>P{[_C>1]}11+1] (go-to-input-start-for_C, }) -> (go-to-input-start-for_C, }, right)$
[C~C{[+S>_][1C>1]}S{[1P<+][_H<_]}<P>{[_C>1]}11+1] (go-to-input-start-for_C, P) -> (go-to-input-start-for_C, P, right)$
[C~C{[+S>_][1C>1]}S{[1P<+][_H<_]}P<{>[_C>1]}11+1] (go-to-input-start-for_C, {) -> (go-to-input-start-for_C, {, right)$
[C~C{[+S>_][1C>1]}S{[1P<+][_H<_]}P{<[>_C>1]}11+1] (go-to-input-start-for_C, [) -> (go-to-input-start-for_C, [, right)$
[C~C{[+S>_][1C>1]}S{[1P<+][_H<_]}P{[<_>C>1]}11+1] (go-to-input-start-for_C, _) -> (go-to-input-start-for_C, _, right)$
[C~C{[+S>_][1C>1]}S{[1P<+][_H<_]}P{[_<C>>1]}11+1] (go-to-input-start-for_C, C) -> (go-to-input-start-for_C, C, right)$
[C~C{[+S>_][1C>1]}S{[1P<+][_H<_]}P{[_C<>>1]}11+1] (go-to-input-start-for_C, >) -> (go-to-input-start-for_C, >, right)$
[C~C{[+S>_][1C>1]}S{[1P<+][_H<_]}P{[_C><1>]}11+1] (go-to-input-start-for_C, 1) -> (go-to-input-start-for_C, 1, right)$
[C~C{[+S>_][1C>1]}S{[1P<+][_H<_]}P{[_C>1<]>}11+1] (go-to-input-start-for_C, ]) -> (go-to-input-start-for_C, ], right)$
[C~C{[+S>_][1C>1]}S{[1P<+][_H<_]}P{[_C>1]<}>11+1] (go-to-input-start-for_C, }) -> (go-to-input-start-for_C, }, right)$
[C~C{[+S>_][1C>1]}S{[1P<+][_H<_]}P{[_C>1]}<1>1+1] (go-to-input-start-for_C, 1) -> (go-to-input-start-for_C, 1, right)$
[C~C{[+S>_][1C>1]}S{[1P<+][_H<_]}P{[_C>1]}1<1>+1] (go-to-input-start-for_C, 1) -> (go-to-input-start-for_C, 1, right)$
[C~C{[+S>_][1C>1]}S{[1P<+][_H<_]}P{[_C>1]}11<+>1] (go-to-input-start-for_C, +) -> (go-to-input-start-for_C, +, right)$
[C~C{[+S>_][1C>1]}S{[1P<+][_H<_]}P{[_C>1]}11+<1>] (go-to-input-start-for_C, 1) -> (go-to-input-start-for_C, 1, right)$
[C~C{[+S>_][1C>1]}S{[1P<+][_H<_]}P{[_C>1]}11+1<.>] (go-to-input-start-for_C, .) -> BLOCKED$
Machine is blocked no more transitions available$
Interpreter closing...$'
}

@test "Initial state is H" {
    run bash -c './_build/default/bin/turing our-machines/unary_add_utm.json  "H~P{[_C>1]}C{[+S>_][1C>1]}S{[1P<+][_H<_]}&11+1" | cat -e'
    assert_output 'Interpreter starting...$
[<H>~P{[_C>1]}C{[+S>_][1C>1]}S{[1P<+][_H<_]}&11+1] (retrieve_initial_state, H) -> (HALT, H, right)$
[H<~>P{[_C>1]}C{[+S>_][1C>1]}S{[1P<+][_H<_]}&11+1] Final state reached !$
Interpreter closing...$'
}
