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

@test "'' does not match" {
    run bash -c './_build/default/bin/turing our-machines/02n.json "" | cat -e'
    assert_output 'Interpreter starting...$
[<.>] (one, .) -> (HALT, y, left)$
[<.>y] Final state reached !$
Interpreter closing...$'
}


@test "'0' does not match" {
    run bash -c './_build/default/bin/turing our-machines/02n.json "0" | cat -e'
    assert_output 'Interpreter starting...$
[<0>] (one, 0) -> (two, 0, right)$
[0<.>] (two, .) -> (HALT, n, left)$
[<0>n] Final state reached !$
Interpreter closing...$'
}

@test "'00' does match" {
    run bash -c './_build/default/bin/turing our-machines/02n.json "00" | cat -e'
    assert_output 'Interpreter starting...$
[<0>0] (one, 0) -> (two, 0, right)$
[0<0>] (two, 0) -> (one, 0, right)$
[00<.>] (one, .) -> (HALT, y, left)$
[0<0>y] Final state reached !$
Interpreter closing...$'
}

@test "'000' does not match" {
    run bash -c './_build/default/bin/turing our-machines/02n.json "000" | cat -e'
    assert_output 'Interpreter starting...$
[<0>00] (one, 0) -> (two, 0, right)$
[0<0>0] (two, 0) -> (one, 0, right)$
[00<0>] (one, 0) -> (two, 0, right)$
[000<.>] (two, .) -> (HALT, n, left)$
[00<0>n] Final state reached !$
Interpreter closing...$'
}

@test "'0000' does match" {
    run bash -c './_build/default/bin/turing our-machines/02n.json "0000" | cat -e'
    assert_output 'Interpreter starting...$
[<0>000] (one, 0) -> (two, 0, right)$
[0<0>00] (two, 0) -> (one, 0, right)$
[00<0>0] (one, 0) -> (two, 0, right)$
[000<0>] (two, 0) -> (one, 0, right)$
[0000<.>] (one, .) -> (HALT, y, left)$
[000<0>y] Final state reached !$
Interpreter closing...$'
}

@test "'00000000000' does not match" {
    run bash -c './_build/default/bin/turing our-machines/02n.json "00000000000" | cat -e'
    assert_output 'Interpreter starting...$
[<0>0000000000] (one, 0) -> (two, 0, right)$
[0<0>000000000] (two, 0) -> (one, 0, right)$
[00<0>00000000] (one, 0) -> (two, 0, right)$
[000<0>0000000] (two, 0) -> (one, 0, right)$
[0000<0>000000] (one, 0) -> (two, 0, right)$
[00000<0>00000] (two, 0) -> (one, 0, right)$
[000000<0>0000] (one, 0) -> (two, 0, right)$
[0000000<0>000] (two, 0) -> (one, 0, right)$
[00000000<0>00] (one, 0) -> (two, 0, right)$
[000000000<0>0] (two, 0) -> (one, 0, right)$
[0000000000<0>] (one, 0) -> (two, 0, right)$
[00000000000<.>] (two, .) -> (HALT, n, left)$
[0000000000<0>n] Final state reached !$
Interpreter closing...$'
}

@test "'000000000000' does match" {
    run bash -c './_build/default/bin/turing our-machines/02n.json "000000000000" | cat -e'
    assert_output 'Interpreter starting...$
[<0>00000000000] (one, 0) -> (two, 0, right)$
[0<0>0000000000] (two, 0) -> (one, 0, right)$
[00<0>000000000] (one, 0) -> (two, 0, right)$
[000<0>00000000] (two, 0) -> (one, 0, right)$
[0000<0>0000000] (one, 0) -> (two, 0, right)$
[00000<0>000000] (two, 0) -> (one, 0, right)$
[000000<0>00000] (one, 0) -> (two, 0, right)$
[0000000<0>0000] (two, 0) -> (one, 0, right)$
[00000000<0>000] (one, 0) -> (two, 0, right)$
[000000000<0>00] (two, 0) -> (one, 0, right)$
[0000000000<0>0] (one, 0) -> (two, 0, right)$
[00000000000<0>] (two, 0) -> (one, 0, right)$
[000000000000<.>] (one, .) -> (HALT, y, left)$
[00000000000<0>y] Final state reached !$
Interpreter closing...$'
}
