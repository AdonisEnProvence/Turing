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

@test "Fail on empty input" {
    run bash -c './_build/default/bin/turing run our-machines/0n_1n.json "" | cat -e'
    assert_output 'Interpreter starting...$
[<.>] (replace_0_to_blank, .) -> (write_n_at_the_end_tape_and_halt, ., right)$
[.<.>] (write_n_at_the_end_tape_and_halt, .) -> (HALT, n, left)$
[<.>n] Final state reached !$
Interpreter closing...$'
}


@test "Fail on input 11111" {
    run bash -c './_build/default/bin/turing run our-machines/0n_1n.json "11111" | cat -e'
    assert_output 'Interpreter starting...$
[<1>1111] (replace_0_to_blank, 1) -> (write_n_at_the_end_tape_and_halt, 1, right)$
[1<1>111] (write_n_at_the_end_tape_and_halt, 1) -> (write_n_at_the_end_tape_and_halt, 1, right)$
[11<1>11] (write_n_at_the_end_tape_and_halt, 1) -> (write_n_at_the_end_tape_and_halt, 1, right)$
[111<1>1] (write_n_at_the_end_tape_and_halt, 1) -> (write_n_at_the_end_tape_and_halt, 1, right)$
[1111<1>] (write_n_at_the_end_tape_and_halt, 1) -> (write_n_at_the_end_tape_and_halt, 1, right)$
[11111<.>] (write_n_at_the_end_tape_and_halt, .) -> (HALT, n, left)$
[1111<1>n] Final state reached !$
Interpreter closing...$'
}

@test "Fail on input 10011" {
    run bash -c './_build/default/bin/turing run our-machines/0n_1n.json "10011" | cat -e'
    assert_output 'Interpreter starting...$
[<1>0011] (replace_0_to_blank, 1) -> (write_n_at_the_end_tape_and_halt, 1, right)$
[1<0>011] (write_n_at_the_end_tape_and_halt, 0) -> (write_n_at_the_end_tape_and_halt, 0, right)$
[10<0>11] (write_n_at_the_end_tape_and_halt, 0) -> (write_n_at_the_end_tape_and_halt, 0, right)$
[100<1>1] (write_n_at_the_end_tape_and_halt, 1) -> (write_n_at_the_end_tape_and_halt, 1, right)$
[1001<1>] (write_n_at_the_end_tape_and_halt, 1) -> (write_n_at_the_end_tape_and_halt, 1, right)$
[10011<.>] (write_n_at_the_end_tape_and_halt, .) -> (HALT, n, left)$
[1001<1>n] Final state reached !$
Interpreter closing...$'
}

@test "Fail on input y0011" {
    run bash -c './_build/default/bin/turing run our-machines/0n_1n.json "y0011" | cat -e'
    assert_output 'Interpreter starting...$
[<y>0011] (replace_0_to_blank, y) -> (write_n_at_the_end_tape_and_halt, y, right)$
[y<0>011] (write_n_at_the_end_tape_and_halt, 0) -> (write_n_at_the_end_tape_and_halt, 0, right)$
[y0<0>11] (write_n_at_the_end_tape_and_halt, 0) -> (write_n_at_the_end_tape_and_halt, 0, right)$
[y00<1>1] (write_n_at_the_end_tape_and_halt, 1) -> (write_n_at_the_end_tape_and_halt, 1, right)$
[y001<1>] (write_n_at_the_end_tape_and_halt, 1) -> (write_n_at_the_end_tape_and_halt, 1, right)$
[y0011<.>] (write_n_at_the_end_tape_and_halt, .) -> (HALT, n, left)$
[y001<1>n] Final state reached !$
Interpreter closing...$'
}

@test "Fail on input n0011" {
    run bash -c './_build/default/bin/turing run our-machines/0n_1n.json "n0011" | cat -e'
    assert_output 'Interpreter starting...$
[<n>0011] (replace_0_to_blank, n) -> (write_n_at_the_end_tape_and_halt, n, right)$
[n<0>011] (write_n_at_the_end_tape_and_halt, 0) -> (write_n_at_the_end_tape_and_halt, 0, right)$
[n0<0>11] (write_n_at_the_end_tape_and_halt, 0) -> (write_n_at_the_end_tape_and_halt, 0, right)$
[n00<1>1] (write_n_at_the_end_tape_and_halt, 1) -> (write_n_at_the_end_tape_and_halt, 1, right)$
[n001<1>] (write_n_at_the_end_tape_and_halt, 1) -> (write_n_at_the_end_tape_and_halt, 1, right)$
[n0011<.>] (write_n_at_the_end_tape_and_halt, .) -> (HALT, n, left)$
[n001<1>n] Final state reached !$
Interpreter closing...$'
}

@test "Fail on input 00000" {
    run bash -c './_build/default/bin/turing run our-machines/0n_1n.json "00000" | cat -e'
    assert_output 'Interpreter starting...$
[<0>0000] (replace_0_to_blank, 0) -> (scanright_to_end_of_tape_blank, ., right)$
[.<0>000] (scanright_to_end_of_tape_blank, 0) -> (scanright_to_end_of_tape_blank, 0, right)$
[.0<0>00] (scanright_to_end_of_tape_blank, 0) -> (scanright_to_end_of_tape_blank, 0, right)$
[.00<0>0] (scanright_to_end_of_tape_blank, 0) -> (scanright_to_end_of_tape_blank, 0, right)$
[.000<0>] (scanright_to_end_of_tape_blank, 0) -> (scanright_to_end_of_tape_blank, 0, right)$
[.0000<.>] (scanright_to_end_of_tape_blank, .) -> (replace_1_to_blank, ., left)$
[.000<0>.] (replace_1_to_blank, 0) -> (write_n_at_the_end_tape_and_halt, 0, right)$
[.0000<.>] (write_n_at_the_end_tape_and_halt, .) -> (HALT, n, left)$
[.000<0>n] Final state reached !$
Interpreter closing...$'
}

@test "Fail on input 0011n" {
    run bash -c './_build/default/bin/turing run our-machines/0n_1n.json "0011n" | cat -e'
    assert_output 'Interpreter starting...$
[<0>011n] (replace_0_to_blank, 0) -> (scanright_to_end_of_tape_blank, ., right)$
[.<0>11n] (scanright_to_end_of_tape_blank, 0) -> (scanright_to_end_of_tape_blank, 0, right)$
[.0<1>1n] (scanright_to_end_of_tape_blank, 1) -> (scanright_to_end_of_tape_blank, 1, right)$
[.01<1>n] (scanright_to_end_of_tape_blank, 1) -> (scanright_to_end_of_tape_blank, 1, right)$
[.011<n>] (scanright_to_end_of_tape_blank, n) -> (write_n_at_the_end_tape_and_halt, n, right)$
[.011n<.>] (write_n_at_the_end_tape_and_halt, .) -> (HALT, n, left)$
[.011<n>n] Final state reached !$
Interpreter closing...$'
}


@test "Fail on input 0011y" {
    run bash -c './_build/default/bin/turing run our-machines/0n_1n.json "0011y" | cat -e'
    assert_output 'Interpreter starting...$
[<0>011y] (replace_0_to_blank, 0) -> (scanright_to_end_of_tape_blank, ., right)$
[.<0>11y] (scanright_to_end_of_tape_blank, 0) -> (scanright_to_end_of_tape_blank, 0, right)$
[.0<1>1y] (scanright_to_end_of_tape_blank, 1) -> (scanright_to_end_of_tape_blank, 1, right)$
[.01<1>y] (scanright_to_end_of_tape_blank, 1) -> (scanright_to_end_of_tape_blank, 1, right)$
[.011<y>] (scanright_to_end_of_tape_blank, y) -> (write_n_at_the_end_tape_and_halt, y, right)$
[.011y<.>] (write_n_at_the_end_tape_and_halt, .) -> (HALT, n, left)$
[.011<y>n] Final state reached !$
Interpreter closing...$'
}

@test "Fail on input 00110" {
    run bash -c './_build/default/bin/turing run our-machines/0n_1n.json "00110" | cat -e'
    assert_output 'Interpreter starting...$
[<0>0110] (replace_0_to_blank, 0) -> (scanright_to_end_of_tape_blank, ., right)$
[.<0>110] (scanright_to_end_of_tape_blank, 0) -> (scanright_to_end_of_tape_blank, 0, right)$
[.0<1>10] (scanright_to_end_of_tape_blank, 1) -> (scanright_to_end_of_tape_blank, 1, right)$
[.01<1>0] (scanright_to_end_of_tape_blank, 1) -> (scanright_to_end_of_tape_blank, 1, right)$
[.011<0>] (scanright_to_end_of_tape_blank, 0) -> (scanright_to_end_of_tape_blank, 0, right)$
[.0110<.>] (scanright_to_end_of_tape_blank, .) -> (replace_1_to_blank, ., left)$
[.011<0>.] (replace_1_to_blank, 0) -> (write_n_at_the_end_tape_and_halt, 0, right)$
[.0110<.>] (write_n_at_the_end_tape_and_halt, .) -> (HALT, n, left)$
[.011<0>n] Final state reached !$
Interpreter closing...$'
}

@test "Fail on input 00y11" {
    run bash -c './_build/default/bin/turing run our-machines/0n_1n.json "00y11" | cat -e'
    assert_output 'Interpreter starting...$
[<0>0y11] (replace_0_to_blank, 0) -> (scanright_to_end_of_tape_blank, ., right)$
[.<0>y11] (scanright_to_end_of_tape_blank, 0) -> (scanright_to_end_of_tape_blank, 0, right)$
[.0<y>11] (scanright_to_end_of_tape_blank, y) -> (write_n_at_the_end_tape_and_halt, y, right)$
[.0y<1>1] (write_n_at_the_end_tape_and_halt, 1) -> (write_n_at_the_end_tape_and_halt, 1, right)$
[.0y1<1>] (write_n_at_the_end_tape_and_halt, 1) -> (write_n_at_the_end_tape_and_halt, 1, right)$
[.0y11<.>] (write_n_at_the_end_tape_and_halt, .) -> (HALT, n, left)$
[.0y1<1>n] Final state reached !$
Interpreter closing...$'
}

@test "Fail on input 0" {
    run bash -c './_build/default/bin/turing run our-machines/0n_1n.json "0" | cat -e'
    assert_output 'Interpreter starting...$
[<0>] (replace_0_to_blank, 0) -> (scanright_to_end_of_tape_blank, ., right)$
[.<.>] (scanright_to_end_of_tape_blank, .) -> (replace_1_to_blank, ., left)$
[<.>.] (replace_1_to_blank, .) -> (write_n_at_the_end_tape_and_halt, ., right)$
[.<.>] (write_n_at_the_end_tape_and_halt, .) -> (HALT, n, left)$
[<.>n] Final state reached !$
Interpreter closing...$'
}

@test "Fail on input 1" {
    run bash -c './_build/default/bin/turing run our-machines/0n_1n.json "1" | cat -e'
    assert_output 'Interpreter starting...$
[<1>] (replace_0_to_blank, 1) -> (write_n_at_the_end_tape_and_halt, 1, right)$
[1<.>] (write_n_at_the_end_tape_and_halt, .) -> (HALT, n, left)$
[<1>n] Final state reached !$
Interpreter closing...$'
}

@test "Fail on input 001" {
    run bash -c './_build/default/bin/turing run our-machines/0n_1n.json "001" | cat -e'
    assert_output 'Interpreter starting...$
[<0>01] (replace_0_to_blank, 0) -> (scanright_to_end_of_tape_blank, ., right)$
[.<0>1] (scanright_to_end_of_tape_blank, 0) -> (scanright_to_end_of_tape_blank, 0, right)$
[.0<1>] (scanright_to_end_of_tape_blank, 1) -> (scanright_to_end_of_tape_blank, 1, right)$
[.01<.>] (scanright_to_end_of_tape_blank, .) -> (replace_1_to_blank, ., left)$
[.0<1>.] (replace_1_to_blank, 1) -> (search_for_success_or_continue, ., left)$
[.<0>..] (search_for_success_or_continue, 0) -> (scanleft_to_begin_of_tape_blank, 0, left)$
[<.>0..] (scanleft_to_begin_of_tape_blank, .) -> (replace_0_to_blank, ., right)$
[.<0>..] (replace_0_to_blank, 0) -> (scanright_to_end_of_tape_blank, ., right)$
[..<.>.] (scanright_to_end_of_tape_blank, .) -> (replace_1_to_blank, ., left)$
[.<.>..] (replace_1_to_blank, .) -> (write_n_at_the_end_tape_and_halt, ., right)$
[..<.>.] (write_n_at_the_end_tape_and_halt, .) -> (HALT, n, left)$
[.<.>n.] Final state reached !$
Interpreter closing...$'
}

@test "Fail on input 011" {
    run bash -c './_build/default/bin/turing run our-machines/0n_1n.json "011" | cat -e'
    assert_output 'Interpreter starting...$
[<0>11] (replace_0_to_blank, 0) -> (scanright_to_end_of_tape_blank, ., right)$
[.<1>1] (scanright_to_end_of_tape_blank, 1) -> (scanright_to_end_of_tape_blank, 1, right)$
[.1<1>] (scanright_to_end_of_tape_blank, 1) -> (scanright_to_end_of_tape_blank, 1, right)$
[.11<.>] (scanright_to_end_of_tape_blank, .) -> (replace_1_to_blank, ., left)$
[.1<1>.] (replace_1_to_blank, 1) -> (search_for_success_or_continue, ., left)$
[.<1>..] (search_for_success_or_continue, 1) -> (scanleft_to_begin_of_tape_blank, 1, left)$
[<.>1..] (scanleft_to_begin_of_tape_blank, .) -> (replace_0_to_blank, ., right)$
[.<1>..] (replace_0_to_blank, 1) -> (write_n_at_the_end_tape_and_halt, 1, right)$
[.1<.>.] (write_n_at_the_end_tape_and_halt, .) -> (HALT, n, left)$
[.<1>n.] Final state reached !$
Interpreter closing...$'
}

@test "Fail on input 000000011111" {
    run bash -c './_build/default/bin/turing run our-machines/0n_1n.json "000000011111" | cat -e'
    assert_output 'Interpreter starting...$
[<0>00000011111] (replace_0_to_blank, 0) -> (scanright_to_end_of_tape_blank, ., right)$
[.<0>0000011111] (scanright_to_end_of_tape_blank, 0) -> (scanright_to_end_of_tape_blank, 0, right)$
[.0<0>000011111] (scanright_to_end_of_tape_blank, 0) -> (scanright_to_end_of_tape_blank, 0, right)$
[.00<0>00011111] (scanright_to_end_of_tape_blank, 0) -> (scanright_to_end_of_tape_blank, 0, right)$
[.000<0>0011111] (scanright_to_end_of_tape_blank, 0) -> (scanright_to_end_of_tape_blank, 0, right)$
[.0000<0>011111] (scanright_to_end_of_tape_blank, 0) -> (scanright_to_end_of_tape_blank, 0, right)$
[.00000<0>11111] (scanright_to_end_of_tape_blank, 0) -> (scanright_to_end_of_tape_blank, 0, right)$
[.000000<1>1111] (scanright_to_end_of_tape_blank, 1) -> (scanright_to_end_of_tape_blank, 1, right)$
[.0000001<1>111] (scanright_to_end_of_tape_blank, 1) -> (scanright_to_end_of_tape_blank, 1, right)$
[.00000011<1>11] (scanright_to_end_of_tape_blank, 1) -> (scanright_to_end_of_tape_blank, 1, right)$
[.000000111<1>1] (scanright_to_end_of_tape_blank, 1) -> (scanright_to_end_of_tape_blank, 1, right)$
[.0000001111<1>] (scanright_to_end_of_tape_blank, 1) -> (scanright_to_end_of_tape_blank, 1, right)$
[.00000011111<.>] (scanright_to_end_of_tape_blank, .) -> (replace_1_to_blank, ., left)$
[.0000001111<1>.] (replace_1_to_blank, 1) -> (search_for_success_or_continue, ., left)$
[.000000111<1>..] (search_for_success_or_continue, 1) -> (scanleft_to_begin_of_tape_blank, 1, left)$
[.00000011<1>1..] (scanleft_to_begin_of_tape_blank, 1) -> (scanleft_to_begin_of_tape_blank, 1, left)$
[.0000001<1>11..] (scanleft_to_begin_of_tape_blank, 1) -> (scanleft_to_begin_of_tape_blank, 1, left)$
[.000000<1>111..] (scanleft_to_begin_of_tape_blank, 1) -> (scanleft_to_begin_of_tape_blank, 1, left)$
[.00000<0>1111..] (scanleft_to_begin_of_tape_blank, 0) -> (scanleft_to_begin_of_tape_blank, 0, left)$
[.0000<0>01111..] (scanleft_to_begin_of_tape_blank, 0) -> (scanleft_to_begin_of_tape_blank, 0, left)$
[.000<0>001111..] (scanleft_to_begin_of_tape_blank, 0) -> (scanleft_to_begin_of_tape_blank, 0, left)$
[.00<0>0001111..] (scanleft_to_begin_of_tape_blank, 0) -> (scanleft_to_begin_of_tape_blank, 0, left)$
[.0<0>00001111..] (scanleft_to_begin_of_tape_blank, 0) -> (scanleft_to_begin_of_tape_blank, 0, left)$
[.<0>000001111..] (scanleft_to_begin_of_tape_blank, 0) -> (scanleft_to_begin_of_tape_blank, 0, left)$
[<.>0000001111..] (scanleft_to_begin_of_tape_blank, .) -> (replace_0_to_blank, ., right)$
[.<0>000001111..] (replace_0_to_blank, 0) -> (scanright_to_end_of_tape_blank, ., right)$
[..<0>00001111..] (scanright_to_end_of_tape_blank, 0) -> (scanright_to_end_of_tape_blank, 0, right)$
[..0<0>0001111..] (scanright_to_end_of_tape_blank, 0) -> (scanright_to_end_of_tape_blank, 0, right)$
[..00<0>001111..] (scanright_to_end_of_tape_blank, 0) -> (scanright_to_end_of_tape_blank, 0, right)$
[..000<0>01111..] (scanright_to_end_of_tape_blank, 0) -> (scanright_to_end_of_tape_blank, 0, right)$
[..0000<0>1111..] (scanright_to_end_of_tape_blank, 0) -> (scanright_to_end_of_tape_blank, 0, right)$
[..00000<1>111..] (scanright_to_end_of_tape_blank, 1) -> (scanright_to_end_of_tape_blank, 1, right)$
[..000001<1>11..] (scanright_to_end_of_tape_blank, 1) -> (scanright_to_end_of_tape_blank, 1, right)$
[..0000011<1>1..] (scanright_to_end_of_tape_blank, 1) -> (scanright_to_end_of_tape_blank, 1, right)$
[..00000111<1>..] (scanright_to_end_of_tape_blank, 1) -> (scanright_to_end_of_tape_blank, 1, right)$
[..000001111<.>.] (scanright_to_end_of_tape_blank, .) -> (replace_1_to_blank, ., left)$
[..00000111<1>..] (replace_1_to_blank, 1) -> (search_for_success_or_continue, ., left)$
[..0000011<1>...] (search_for_success_or_continue, 1) -> (scanleft_to_begin_of_tape_blank, 1, left)$
[..000001<1>1...] (scanleft_to_begin_of_tape_blank, 1) -> (scanleft_to_begin_of_tape_blank, 1, left)$
[..00000<1>11...] (scanleft_to_begin_of_tape_blank, 1) -> (scanleft_to_begin_of_tape_blank, 1, left)$
[..0000<0>111...] (scanleft_to_begin_of_tape_blank, 0) -> (scanleft_to_begin_of_tape_blank, 0, left)$
[..000<0>0111...] (scanleft_to_begin_of_tape_blank, 0) -> (scanleft_to_begin_of_tape_blank, 0, left)$
[..00<0>00111...] (scanleft_to_begin_of_tape_blank, 0) -> (scanleft_to_begin_of_tape_blank, 0, left)$
[..0<0>000111...] (scanleft_to_begin_of_tape_blank, 0) -> (scanleft_to_begin_of_tape_blank, 0, left)$
[..<0>0000111...] (scanleft_to_begin_of_tape_blank, 0) -> (scanleft_to_begin_of_tape_blank, 0, left)$
[.<.>00000111...] (scanleft_to_begin_of_tape_blank, .) -> (replace_0_to_blank, ., right)$
[..<0>0000111...] (replace_0_to_blank, 0) -> (scanright_to_end_of_tape_blank, ., right)$
[...<0>000111...] (scanright_to_end_of_tape_blank, 0) -> (scanright_to_end_of_tape_blank, 0, right)$
[...0<0>00111...] (scanright_to_end_of_tape_blank, 0) -> (scanright_to_end_of_tape_blank, 0, right)$
[...00<0>0111...] (scanright_to_end_of_tape_blank, 0) -> (scanright_to_end_of_tape_blank, 0, right)$
[...000<0>111...] (scanright_to_end_of_tape_blank, 0) -> (scanright_to_end_of_tape_blank, 0, right)$
[...0000<1>11...] (scanright_to_end_of_tape_blank, 1) -> (scanright_to_end_of_tape_blank, 1, right)$
[...00001<1>1...] (scanright_to_end_of_tape_blank, 1) -> (scanright_to_end_of_tape_blank, 1, right)$
[...000011<1>...] (scanright_to_end_of_tape_blank, 1) -> (scanright_to_end_of_tape_blank, 1, right)$
[...0000111<.>..] (scanright_to_end_of_tape_blank, .) -> (replace_1_to_blank, ., left)$
[...000011<1>...] (replace_1_to_blank, 1) -> (search_for_success_or_continue, ., left)$
[...00001<1>....] (search_for_success_or_continue, 1) -> (scanleft_to_begin_of_tape_blank, 1, left)$
[...0000<1>1....] (scanleft_to_begin_of_tape_blank, 1) -> (scanleft_to_begin_of_tape_blank, 1, left)$
[...000<0>11....] (scanleft_to_begin_of_tape_blank, 0) -> (scanleft_to_begin_of_tape_blank, 0, left)$
[...00<0>011....] (scanleft_to_begin_of_tape_blank, 0) -> (scanleft_to_begin_of_tape_blank, 0, left)$
[...0<0>0011....] (scanleft_to_begin_of_tape_blank, 0) -> (scanleft_to_begin_of_tape_blank, 0, left)$
[...<0>00011....] (scanleft_to_begin_of_tape_blank, 0) -> (scanleft_to_begin_of_tape_blank, 0, left)$
[..<.>000011....] (scanleft_to_begin_of_tape_blank, .) -> (replace_0_to_blank, ., right)$
[...<0>00011....] (replace_0_to_blank, 0) -> (scanright_to_end_of_tape_blank, ., right)$
[....<0>0011....] (scanright_to_end_of_tape_blank, 0) -> (scanright_to_end_of_tape_blank, 0, right)$
[....0<0>011....] (scanright_to_end_of_tape_blank, 0) -> (scanright_to_end_of_tape_blank, 0, right)$
[....00<0>11....] (scanright_to_end_of_tape_blank, 0) -> (scanright_to_end_of_tape_blank, 0, right)$
[....000<1>1....] (scanright_to_end_of_tape_blank, 1) -> (scanright_to_end_of_tape_blank, 1, right)$
[....0001<1>....] (scanright_to_end_of_tape_blank, 1) -> (scanright_to_end_of_tape_blank, 1, right)$
[....00011<.>...] (scanright_to_end_of_tape_blank, .) -> (replace_1_to_blank, ., left)$
[....0001<1>....] (replace_1_to_blank, 1) -> (search_for_success_or_continue, ., left)$
[....000<1>.....] (search_for_success_or_continue, 1) -> (scanleft_to_begin_of_tape_blank, 1, left)$
[....00<0>1.....] (scanleft_to_begin_of_tape_blank, 0) -> (scanleft_to_begin_of_tape_blank, 0, left)$
[....0<0>01.....] (scanleft_to_begin_of_tape_blank, 0) -> (scanleft_to_begin_of_tape_blank, 0, left)$
[....<0>001.....] (scanleft_to_begin_of_tape_blank, 0) -> (scanleft_to_begin_of_tape_blank, 0, left)$
[...<.>0001.....] (scanleft_to_begin_of_tape_blank, .) -> (replace_0_to_blank, ., right)$
[....<0>001.....] (replace_0_to_blank, 0) -> (scanright_to_end_of_tape_blank, ., right)$
[.....<0>01.....] (scanright_to_end_of_tape_blank, 0) -> (scanright_to_end_of_tape_blank, 0, right)$
[.....0<0>1.....] (scanright_to_end_of_tape_blank, 0) -> (scanright_to_end_of_tape_blank, 0, right)$
[.....00<1>.....] (scanright_to_end_of_tape_blank, 1) -> (scanright_to_end_of_tape_blank, 1, right)$
[.....001<.>....] (scanright_to_end_of_tape_blank, .) -> (replace_1_to_blank, ., left)$
[.....00<1>.....] (replace_1_to_blank, 1) -> (search_for_success_or_continue, ., left)$
[.....0<0>......] (search_for_success_or_continue, 0) -> (scanleft_to_begin_of_tape_blank, 0, left)$
[.....<0>0......] (scanleft_to_begin_of_tape_blank, 0) -> (scanleft_to_begin_of_tape_blank, 0, left)$
[....<.>00......] (scanleft_to_begin_of_tape_blank, .) -> (replace_0_to_blank, ., right)$
[.....<0>0......] (replace_0_to_blank, 0) -> (scanright_to_end_of_tape_blank, ., right)$
[......<0>......] (scanright_to_end_of_tape_blank, 0) -> (scanright_to_end_of_tape_blank, 0, right)$
[......0<.>.....] (scanright_to_end_of_tape_blank, .) -> (replace_1_to_blank, ., left)$
[......<0>......] (replace_1_to_blank, 0) -> (write_n_at_the_end_tape_and_halt, 0, right)$
[......0<.>.....] (write_n_at_the_end_tape_and_halt, .) -> (HALT, n, left)$
[......<0>n.....] Final state reached !$
Interpreter closing...$'
}

@test "Success on input 0011" {
    run bash -c './_build/default/bin/turing run our-machines/0n_1n.json "0011" | cat -e'
    assert_output 'Interpreter starting...$
[<0>011] (replace_0_to_blank, 0) -> (scanright_to_end_of_tape_blank, ., right)$
[.<0>11] (scanright_to_end_of_tape_blank, 0) -> (scanright_to_end_of_tape_blank, 0, right)$
[.0<1>1] (scanright_to_end_of_tape_blank, 1) -> (scanright_to_end_of_tape_blank, 1, right)$
[.01<1>] (scanright_to_end_of_tape_blank, 1) -> (scanright_to_end_of_tape_blank, 1, right)$
[.011<.>] (scanright_to_end_of_tape_blank, .) -> (replace_1_to_blank, ., left)$
[.01<1>.] (replace_1_to_blank, 1) -> (search_for_success_or_continue, ., left)$
[.0<1>..] (search_for_success_or_continue, 1) -> (scanleft_to_begin_of_tape_blank, 1, left)$
[.<0>1..] (scanleft_to_begin_of_tape_blank, 0) -> (scanleft_to_begin_of_tape_blank, 0, left)$
[<.>01..] (scanleft_to_begin_of_tape_blank, .) -> (replace_0_to_blank, ., right)$
[.<0>1..] (replace_0_to_blank, 0) -> (scanright_to_end_of_tape_blank, ., right)$
[..<1>..] (scanright_to_end_of_tape_blank, 1) -> (scanright_to_end_of_tape_blank, 1, right)$
[..1<.>.] (scanright_to_end_of_tape_blank, .) -> (replace_1_to_blank, ., left)$
[..<1>..] (replace_1_to_blank, 1) -> (search_for_success_or_continue, ., left)$
[.<.>...] (search_for_success_or_continue, .) -> (write_y_at_the_end_tape_and_halt, ., right)$
[..<.>..] (write_y_at_the_end_tape_and_halt, .) -> (HALT, y, left)$
[.<.>y..] Final state reached !$
Interpreter closing...$'
}

@test "Success on input 01" {
    run bash -c './_build/default/bin/turing run our-machines/0n_1n.json "01" | cat -e'
    assert_output 'Interpreter starting...$
[<0>1] (replace_0_to_blank, 0) -> (scanright_to_end_of_tape_blank, ., right)$
[.<1>] (scanright_to_end_of_tape_blank, 1) -> (scanright_to_end_of_tape_blank, 1, right)$
[.1<.>] (scanright_to_end_of_tape_blank, .) -> (replace_1_to_blank, ., left)$
[.<1>.] (replace_1_to_blank, 1) -> (search_for_success_or_continue, ., left)$
[<.>..] (search_for_success_or_continue, .) -> (write_y_at_the_end_tape_and_halt, ., right)$
[.<.>.] (write_y_at_the_end_tape_and_halt, .) -> (HALT, y, left)$
[<.>y.] Final state reached !$
Interpreter closing...$'
}

@test "Success on input 00001111" {
    run bash -c './_build/default/bin/turing run our-machines/0n_1n.json "00001111" | cat -e'
    assert_output 'Interpreter starting...$
[<0>0001111] (replace_0_to_blank, 0) -> (scanright_to_end_of_tape_blank, ., right)$
[.<0>001111] (scanright_to_end_of_tape_blank, 0) -> (scanright_to_end_of_tape_blank, 0, right)$
[.0<0>01111] (scanright_to_end_of_tape_blank, 0) -> (scanright_to_end_of_tape_blank, 0, right)$
[.00<0>1111] (scanright_to_end_of_tape_blank, 0) -> (scanright_to_end_of_tape_blank, 0, right)$
[.000<1>111] (scanright_to_end_of_tape_blank, 1) -> (scanright_to_end_of_tape_blank, 1, right)$
[.0001<1>11] (scanright_to_end_of_tape_blank, 1) -> (scanright_to_end_of_tape_blank, 1, right)$
[.00011<1>1] (scanright_to_end_of_tape_blank, 1) -> (scanright_to_end_of_tape_blank, 1, right)$
[.000111<1>] (scanright_to_end_of_tape_blank, 1) -> (scanright_to_end_of_tape_blank, 1, right)$
[.0001111<.>] (scanright_to_end_of_tape_blank, .) -> (replace_1_to_blank, ., left)$
[.000111<1>.] (replace_1_to_blank, 1) -> (search_for_success_or_continue, ., left)$
[.00011<1>..] (search_for_success_or_continue, 1) -> (scanleft_to_begin_of_tape_blank, 1, left)$
[.0001<1>1..] (scanleft_to_begin_of_tape_blank, 1) -> (scanleft_to_begin_of_tape_blank, 1, left)$
[.000<1>11..] (scanleft_to_begin_of_tape_blank, 1) -> (scanleft_to_begin_of_tape_blank, 1, left)$
[.00<0>111..] (scanleft_to_begin_of_tape_blank, 0) -> (scanleft_to_begin_of_tape_blank, 0, left)$
[.0<0>0111..] (scanleft_to_begin_of_tape_blank, 0) -> (scanleft_to_begin_of_tape_blank, 0, left)$
[.<0>00111..] (scanleft_to_begin_of_tape_blank, 0) -> (scanleft_to_begin_of_tape_blank, 0, left)$
[<.>000111..] (scanleft_to_begin_of_tape_blank, .) -> (replace_0_to_blank, ., right)$
[.<0>00111..] (replace_0_to_blank, 0) -> (scanright_to_end_of_tape_blank, ., right)$
[..<0>0111..] (scanright_to_end_of_tape_blank, 0) -> (scanright_to_end_of_tape_blank, 0, right)$
[..0<0>111..] (scanright_to_end_of_tape_blank, 0) -> (scanright_to_end_of_tape_blank, 0, right)$
[..00<1>11..] (scanright_to_end_of_tape_blank, 1) -> (scanright_to_end_of_tape_blank, 1, right)$
[..001<1>1..] (scanright_to_end_of_tape_blank, 1) -> (scanright_to_end_of_tape_blank, 1, right)$
[..0011<1>..] (scanright_to_end_of_tape_blank, 1) -> (scanright_to_end_of_tape_blank, 1, right)$
[..00111<.>.] (scanright_to_end_of_tape_blank, .) -> (replace_1_to_blank, ., left)$
[..0011<1>..] (replace_1_to_blank, 1) -> (search_for_success_or_continue, ., left)$
[..001<1>...] (search_for_success_or_continue, 1) -> (scanleft_to_begin_of_tape_blank, 1, left)$
[..00<1>1...] (scanleft_to_begin_of_tape_blank, 1) -> (scanleft_to_begin_of_tape_blank, 1, left)$
[..0<0>11...] (scanleft_to_begin_of_tape_blank, 0) -> (scanleft_to_begin_of_tape_blank, 0, left)$
[..<0>011...] (scanleft_to_begin_of_tape_blank, 0) -> (scanleft_to_begin_of_tape_blank, 0, left)$
[.<.>0011...] (scanleft_to_begin_of_tape_blank, .) -> (replace_0_to_blank, ., right)$
[..<0>011...] (replace_0_to_blank, 0) -> (scanright_to_end_of_tape_blank, ., right)$
[...<0>11...] (scanright_to_end_of_tape_blank, 0) -> (scanright_to_end_of_tape_blank, 0, right)$
[...0<1>1...] (scanright_to_end_of_tape_blank, 1) -> (scanright_to_end_of_tape_blank, 1, right)$
[...01<1>...] (scanright_to_end_of_tape_blank, 1) -> (scanright_to_end_of_tape_blank, 1, right)$
[...011<.>..] (scanright_to_end_of_tape_blank, .) -> (replace_1_to_blank, ., left)$
[...01<1>...] (replace_1_to_blank, 1) -> (search_for_success_or_continue, ., left)$
[...0<1>....] (search_for_success_or_continue, 1) -> (scanleft_to_begin_of_tape_blank, 1, left)$
[...<0>1....] (scanleft_to_begin_of_tape_blank, 0) -> (scanleft_to_begin_of_tape_blank, 0, left)$
[..<.>01....] (scanleft_to_begin_of_tape_blank, .) -> (replace_0_to_blank, ., right)$
[...<0>1....] (replace_0_to_blank, 0) -> (scanright_to_end_of_tape_blank, ., right)$
[....<1>....] (scanright_to_end_of_tape_blank, 1) -> (scanright_to_end_of_tape_blank, 1, right)$
[....1<.>...] (scanright_to_end_of_tape_blank, .) -> (replace_1_to_blank, ., left)$
[....<1>....] (replace_1_to_blank, 1) -> (search_for_success_or_continue, ., left)$
[...<.>.....] (search_for_success_or_continue, .) -> (write_y_at_the_end_tape_and_halt, ., right)$
[....<.>....] (write_y_at_the_end_tape_and_halt, .) -> (HALT, y, left)$
[...<.>y....] Final state reached !$
Interpreter closing...$'
}

