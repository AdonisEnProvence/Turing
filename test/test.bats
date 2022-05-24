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

@test "Default turing machine static input, output test" {
    run bash -c './_build/default/bin/turing | cat -e' # notice `run`!
    assert_output 'Interpreter starting...$
Tape: [<"1">,"1","0"] STATE="IDLE"$
Tape: ["0",<"1">,"0"] STATE="IDLE"$
Tape: ["0","0",<"0">] STATE="IDLE"$
Tape: ["0",<"0">,"."] STATE="IDLE"$
Tape: [<"0">,".","."] STATE="IDLE"$
Tape: [<".">,".",".","."] STATE="IDLE"$
Tape: [<".">,".",".",".","."] STATE="HALT"$
Machine is blocked no more transitions available$
Interpreter closing...$'
}