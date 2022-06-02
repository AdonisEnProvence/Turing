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
    run bash -c './_build/default/bin/turing our-machines/0n_1n.json "" | cat -e'
    assert_output 'Interpreter starting...$
Tape: [<".">] ("replace_0_to_blank", ".") -> ("write_n_at_the_end_tape_and_halt", ".", right)$
Tape: [".",<".">] ("write_n_at_the_end_tape_and_halt", ".") -> ("HALT", "n", left)$
Tape: [<".">,"n"] Final state reached !$
Interpreter closing...$'
}


@test "Fail on input 11111" {
    run bash -c './_build/default/bin/turing our-machines/0n_1n.json "11111" | cat -e'
    assert_output 'Interpreter starting...$
Tape: [<"1">,"1","1","1","1"] ("replace_0_to_blank", "1") -> ("write_n_at_the_end_tape_and_halt", "1", right)$
Tape: ["1",<"1">,"1","1","1"] ("write_n_at_the_end_tape_and_halt", "1") -> ("write_n_at_the_end_tape_and_halt", "1", right)$
Tape: ["1","1",<"1">,"1","1"] ("write_n_at_the_end_tape_and_halt", "1") -> ("write_n_at_the_end_tape_and_halt", "1", right)$
Tape: ["1","1","1",<"1">,"1"] ("write_n_at_the_end_tape_and_halt", "1") -> ("write_n_at_the_end_tape_and_halt", "1", right)$
Tape: ["1","1","1","1",<"1">] ("write_n_at_the_end_tape_and_halt", "1") -> ("write_n_at_the_end_tape_and_halt", "1", right)$
Tape: ["1","1","1","1","1",<".">] ("write_n_at_the_end_tape_and_halt", ".") -> ("HALT", "n", left)$
Tape: ["1","1","1","1",<"1">,"n"] Final state reached !$
Interpreter closing...$'
}

@test "Fail on input 10011" {
    run bash -c './_build/default/bin/turing our-machines/0n_1n.json "10011" | cat -e'
    assert_output 'Interpreter starting...$
Tape: [<"1">,"0","0","1","1"] ("replace_0_to_blank", "1") -> ("write_n_at_the_end_tape_and_halt", "1", right)$
Tape: ["1",<"0">,"0","1","1"] ("write_n_at_the_end_tape_and_halt", "0") -> ("write_n_at_the_end_tape_and_halt", "0", right)$
Tape: ["1","0",<"0">,"1","1"] ("write_n_at_the_end_tape_and_halt", "0") -> ("write_n_at_the_end_tape_and_halt", "0", right)$
Tape: ["1","0","0",<"1">,"1"] ("write_n_at_the_end_tape_and_halt", "1") -> ("write_n_at_the_end_tape_and_halt", "1", right)$
Tape: ["1","0","0","1",<"1">] ("write_n_at_the_end_tape_and_halt", "1") -> ("write_n_at_the_end_tape_and_halt", "1", right)$
Tape: ["1","0","0","1","1",<".">] ("write_n_at_the_end_tape_and_halt", ".") -> ("HALT", "n", left)$
Tape: ["1","0","0","1",<"1">,"n"] Final state reached !$
Interpreter closing...$'
}

@test "Fail on input y0011" {
    run bash -c './_build/default/bin/turing our-machines/0n_1n.json "y0011" | cat -e'
    assert_output 'Interpreter starting...$
Tape: [<"y">,"0","0","1","1"] ("replace_0_to_blank", "y") -> ("write_n_at_the_end_tape_and_halt", "y", right)$
Tape: ["y",<"0">,"0","1","1"] ("write_n_at_the_end_tape_and_halt", "0") -> ("write_n_at_the_end_tape_and_halt", "0", right)$
Tape: ["y","0",<"0">,"1","1"] ("write_n_at_the_end_tape_and_halt", "0") -> ("write_n_at_the_end_tape_and_halt", "0", right)$
Tape: ["y","0","0",<"1">,"1"] ("write_n_at_the_end_tape_and_halt", "1") -> ("write_n_at_the_end_tape_and_halt", "1", right)$
Tape: ["y","0","0","1",<"1">] ("write_n_at_the_end_tape_and_halt", "1") -> ("write_n_at_the_end_tape_and_halt", "1", right)$
Tape: ["y","0","0","1","1",<".">] ("write_n_at_the_end_tape_and_halt", ".") -> ("HALT", "n", left)$
Tape: ["y","0","0","1",<"1">,"n"] Final state reached !$
Interpreter closing...$'
}

@test "Fail on input n0011" {
    run bash -c './_build/default/bin/turing our-machines/0n_1n.json "n0011" | cat -e'
    assert_output 'Interpreter starting...$
Tape: [<"n">,"0","0","1","1"] ("replace_0_to_blank", "n") -> ("write_n_at_the_end_tape_and_halt", "n", right)$
Tape: ["n",<"0">,"0","1","1"] ("write_n_at_the_end_tape_and_halt", "0") -> ("write_n_at_the_end_tape_and_halt", "0", right)$
Tape: ["n","0",<"0">,"1","1"] ("write_n_at_the_end_tape_and_halt", "0") -> ("write_n_at_the_end_tape_and_halt", "0", right)$
Tape: ["n","0","0",<"1">,"1"] ("write_n_at_the_end_tape_and_halt", "1") -> ("write_n_at_the_end_tape_and_halt", "1", right)$
Tape: ["n","0","0","1",<"1">] ("write_n_at_the_end_tape_and_halt", "1") -> ("write_n_at_the_end_tape_and_halt", "1", right)$
Tape: ["n","0","0","1","1",<".">] ("write_n_at_the_end_tape_and_halt", ".") -> ("HALT", "n", left)$
Tape: ["n","0","0","1",<"1">,"n"] Final state reached !$
Interpreter closing...$'
}

@test "Fail on input 00000" {
    run bash -c './_build/default/bin/turing our-machines/0n_1n.json "00000" | cat -e'
    assert_output 'Interpreter starting...$
Tape: [<"0">,"0","0","0","0"] ("replace_0_to_blank", "0") -> ("scanright_to_end_of_tape_blank", ".", right)$
Tape: [".",<"0">,"0","0","0"] ("scanright_to_end_of_tape_blank", "0") -> ("scanright_to_end_of_tape_blank", "0", right)$
Tape: [".","0",<"0">,"0","0"] ("scanright_to_end_of_tape_blank", "0") -> ("scanright_to_end_of_tape_blank", "0", right)$
Tape: [".","0","0",<"0">,"0"] ("scanright_to_end_of_tape_blank", "0") -> ("scanright_to_end_of_tape_blank", "0", right)$
Tape: [".","0","0","0",<"0">] ("scanright_to_end_of_tape_blank", "0") -> ("scanright_to_end_of_tape_blank", "0", right)$
Tape: [".","0","0","0","0",<".">] ("scanright_to_end_of_tape_blank", ".") -> ("replace_1_to_blank", ".", left)$
Tape: [".","0","0","0",<"0">,"."] ("replace_1_to_blank", "0") -> ("write_n_at_the_end_tape_and_halt", "0", right)$
Tape: [".","0","0","0","0",<".">] ("write_n_at_the_end_tape_and_halt", ".") -> ("HALT", "n", left)$
Tape: [".","0","0","0",<"0">,"n"] Final state reached !$
Interpreter closing...$'
}

@test "Fail on input 0011n" {
    run bash -c './_build/default/bin/turing our-machines/0n_1n.json "0011n" | cat -e'
    assert_output 'Interpreter starting...$
Tape: [<"0">,"0","1","1","n"] ("replace_0_to_blank", "0") -> ("scanright_to_end_of_tape_blank", ".", right)$
Tape: [".",<"0">,"1","1","n"] ("scanright_to_end_of_tape_blank", "0") -> ("scanright_to_end_of_tape_blank", "0", right)$
Tape: [".","0",<"1">,"1","n"] ("scanright_to_end_of_tape_blank", "1") -> ("scanright_to_end_of_tape_blank", "1", right)$
Tape: [".","0","1",<"1">,"n"] ("scanright_to_end_of_tape_blank", "1") -> ("scanright_to_end_of_tape_blank", "1", right)$
Tape: [".","0","1","1",<"n">] ("scanright_to_end_of_tape_blank", "n") -> ("write_n_at_the_end_tape_and_halt", "n", right)$
Tape: [".","0","1","1","n",<".">] ("write_n_at_the_end_tape_and_halt", ".") -> ("HALT", "n", left)$
Tape: [".","0","1","1",<"n">,"n"] Final state reached !$
Interpreter closing...$'
}


@test "Fail on input 0011y" {
    run bash -c './_build/default/bin/turing our-machines/0n_1n.json "0011y" | cat -e'
    assert_output 'Interpreter starting...$
Tape: [<"0">,"0","1","1","y"] ("replace_0_to_blank", "0") -> ("scanright_to_end_of_tape_blank", ".", right)$
Tape: [".",<"0">,"1","1","y"] ("scanright_to_end_of_tape_blank", "0") -> ("scanright_to_end_of_tape_blank", "0", right)$
Tape: [".","0",<"1">,"1","y"] ("scanright_to_end_of_tape_blank", "1") -> ("scanright_to_end_of_tape_blank", "1", right)$
Tape: [".","0","1",<"1">,"y"] ("scanright_to_end_of_tape_blank", "1") -> ("scanright_to_end_of_tape_blank", "1", right)$
Tape: [".","0","1","1",<"y">] ("scanright_to_end_of_tape_blank", "y") -> ("write_n_at_the_end_tape_and_halt", "y", right)$
Tape: [".","0","1","1","y",<".">] ("write_n_at_the_end_tape_and_halt", ".") -> ("HALT", "n", left)$
Tape: [".","0","1","1",<"y">,"n"] Final state reached !$
Interpreter closing...$'
}

@test "Fail on input 00110" {
    run bash -c './_build/default/bin/turing our-machines/0n_1n.json "00110" | cat -e'
    assert_output 'Interpreter starting...$
Tape: [<"0">,"0","1","1","0"] ("replace_0_to_blank", "0") -> ("scanright_to_end_of_tape_blank", ".", right)$
Tape: [".",<"0">,"1","1","0"] ("scanright_to_end_of_tape_blank", "0") -> ("scanright_to_end_of_tape_blank", "0", right)$
Tape: [".","0",<"1">,"1","0"] ("scanright_to_end_of_tape_blank", "1") -> ("scanright_to_end_of_tape_blank", "1", right)$
Tape: [".","0","1",<"1">,"0"] ("scanright_to_end_of_tape_blank", "1") -> ("scanright_to_end_of_tape_blank", "1", right)$
Tape: [".","0","1","1",<"0">] ("scanright_to_end_of_tape_blank", "0") -> ("scanright_to_end_of_tape_blank", "0", right)$
Tape: [".","0","1","1","0",<".">] ("scanright_to_end_of_tape_blank", ".") -> ("replace_1_to_blank", ".", left)$
Tape: [".","0","1","1",<"0">,"."] ("replace_1_to_blank", "0") -> ("write_n_at_the_end_tape_and_halt", "0", right)$
Tape: [".","0","1","1","0",<".">] ("write_n_at_the_end_tape_and_halt", ".") -> ("HALT", "n", left)$
Tape: [".","0","1","1",<"0">,"n"] Final state reached !$
Interpreter closing...$'
}

@test "Fail on input 00y11" {
    run bash -c './_build/default/bin/turing our-machines/0n_1n.json "00y11" | cat -e'
    assert_output 'Interpreter starting...$
Tape: [<"0">,"0","y","1","1"] ("replace_0_to_blank", "0") -> ("scanright_to_end_of_tape_blank", ".", right)$
Tape: [".",<"0">,"y","1","1"] ("scanright_to_end_of_tape_blank", "0") -> ("scanright_to_end_of_tape_blank", "0", right)$
Tape: [".","0",<"y">,"1","1"] ("scanright_to_end_of_tape_blank", "y") -> ("write_n_at_the_end_tape_and_halt", "y", right)$
Tape: [".","0","y",<"1">,"1"] ("write_n_at_the_end_tape_and_halt", "1") -> ("write_n_at_the_end_tape_and_halt", "1", right)$
Tape: [".","0","y","1",<"1">] ("write_n_at_the_end_tape_and_halt", "1") -> ("write_n_at_the_end_tape_and_halt", "1", right)$
Tape: [".","0","y","1","1",<".">] ("write_n_at_the_end_tape_and_halt", ".") -> ("HALT", "n", left)$
Tape: [".","0","y","1",<"1">,"n"] Final state reached !$
Interpreter closing...$'
}




