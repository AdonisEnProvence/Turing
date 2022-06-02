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

@test "Blocks on empty input" {
    run bash -c './_build/default/bin/turing our-machines/unary_add.json "" | cat -e'
    assert_output 'Interpreter starting...$
[<.>] (find_+, .) -> BLOCKED$
Machine is blocked no more transitions available$
Interpreter closing...$'
}

@test "1 is 1" {
    run bash -c './_build/default/bin/turing our-machines/unary_add.json "1" | cat -e'
    assert_output 'Interpreter starting...$
[<1>] (find_+, 1) -> (find_+, 1, right)$
[1<.>] (find_+, .) -> BLOCKED$
Machine is blocked no more transitions available$
Interpreter closing...$'
}

@test "1+1 is 11" {
    run bash -c './_build/default/bin/turing our-machines/unary_add.json "1+1" | cat -e'
    assert_output 'Interpreter starting...$
[<1>+1] (find_+, 1) -> (find_+, 1, right)$
[1<+>1] (find_+, +) -> (find_next_1_and_replace_with_+, +, right)$
[1+<1>] (find_next_1_and_replace_with_+, 1) -> (replace_+_with_1, +, left)$
[1<+>+] (replace_+_with_1, +) -> (find_+, 1, right)$
[11<+>] (find_+, +) -> (find_next_1_and_replace_with_+, +, right)$
[11+<.>] (find_next_1_and_replace_with_+, .) -> (clear_+, ., left)$
[11<+>.] (clear_+, +) -> (HALT, ., left)$
[1<1>..] Final state reached !$
Interpreter closing...$'
}

@test "1111+111 is 1111111" {
    run bash -c './_build/default/bin/turing our-machines/unary_add.json "1111+111" | cat -e'
    assert_output 'Interpreter starting...$
[<1>111+111] (find_+, 1) -> (find_+, 1, right)$
[1<1>11+111] (find_+, 1) -> (find_+, 1, right)$
[11<1>1+111] (find_+, 1) -> (find_+, 1, right)$
[111<1>+111] (find_+, 1) -> (find_+, 1, right)$
[1111<+>111] (find_+, +) -> (find_next_1_and_replace_with_+, +, right)$
[1111+<1>11] (find_next_1_and_replace_with_+, 1) -> (replace_+_with_1, +, left)$
[1111<+>+11] (replace_+_with_1, +) -> (find_+, 1, right)$
[11111<+>11] (find_+, +) -> (find_next_1_and_replace_with_+, +, right)$
[11111+<1>1] (find_next_1_and_replace_with_+, 1) -> (replace_+_with_1, +, left)$
[11111<+>+1] (replace_+_with_1, +) -> (find_+, 1, right)$
[111111<+>1] (find_+, +) -> (find_next_1_and_replace_with_+, +, right)$
[111111+<1>] (find_next_1_and_replace_with_+, 1) -> (replace_+_with_1, +, left)$
[111111<+>+] (replace_+_with_1, +) -> (find_+, 1, right)$
[1111111<+>] (find_+, +) -> (find_next_1_and_replace_with_+, +, right)$
[1111111+<.>] (find_next_1_and_replace_with_+, .) -> (clear_+, ., left)$
[1111111<+>.] (clear_+, +) -> (HALT, ., left)$
[111111<1>..] Final state reached !$
Interpreter closing...$'
}

@test "1+111 is 1111" {
    run bash -c './_build/default/bin/turing our-machines/unary_add.json "1+111" | cat -e'
    assert_output 'Interpreter starting...$
[<1>+111] (find_+, 1) -> (find_+, 1, right)$
[1<+>111] (find_+, +) -> (find_next_1_and_replace_with_+, +, right)$
[1+<1>11] (find_next_1_and_replace_with_+, 1) -> (replace_+_with_1, +, left)$
[1<+>+11] (replace_+_with_1, +) -> (find_+, 1, right)$
[11<+>11] (find_+, +) -> (find_next_1_and_replace_with_+, +, right)$
[11+<1>1] (find_next_1_and_replace_with_+, 1) -> (replace_+_with_1, +, left)$
[11<+>+1] (replace_+_with_1, +) -> (find_+, 1, right)$
[111<+>1] (find_+, +) -> (find_next_1_and_replace_with_+, +, right)$
[111+<1>] (find_next_1_and_replace_with_+, 1) -> (replace_+_with_1, +, left)$
[111<+>+] (replace_+_with_1, +) -> (find_+, 1, right)$
[1111<+>] (find_+, +) -> (find_next_1_and_replace_with_+, +, right)$
[1111+<.>] (find_next_1_and_replace_with_+, .) -> (clear_+, ., left)$
[1111<+>.] (clear_+, +) -> (HALT, ., left)$
[111<1>..] Final state reached !$
Interpreter closing...$'
}
