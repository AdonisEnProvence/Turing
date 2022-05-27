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
    run bash -c './_build/default/bin/turing machines/unary_sub.json | cat -e' # notice `run`!
    assert_output 'Interpreter starting...$
Tape: [<"1">,"1","1","-","1","1","="] ("scanright", "1") -> ("scanright", "1", right)$
Tape: ["1",<"1">,"1","-","1","1","="] ("scanright", "1") -> ("scanright", "1", right)$
Tape: ["1","1",<"1">,"-","1","1","="] ("scanright", "1") -> ("scanright", "1", right)$
Tape: ["1","1","1",<"-">,"1","1","="] ("scanright", "-") -> ("scanright", "-", right)$
Tape: ["1","1","1","-",<"1">,"1","="] ("scanright", "1") -> ("scanright", "1", right)$
Tape: ["1","1","1","-","1",<"1">,"="] ("scanright", "1") -> ("scanright", "1", right)$
Tape: ["1","1","1","-","1","1",<"=">] ("scanright", "=") -> ("eraseone", ".", left)$
Tape: ["1","1","1","-","1",<"1">,"."] ("eraseone", "1") -> ("subone", "=", left)$
Tape: ["1","1","1","-",<"1">,"=","."] ("subone", "1") -> ("subone", "1", left)$
Tape: ["1","1","1",<"-">,"1","=","."] ("subone", "-") -> ("skip", "-", left)$
Tape: ["1","1",<"1">,"-","1","=","."] ("skip", "1") -> ("scanright", ".", right)$
Tape: ["1","1",".",<"-">,"1","=","."] ("scanright", "-") -> ("scanright", "-", right)$
Tape: ["1","1",".","-",<"1">,"=","."] ("scanright", "1") -> ("scanright", "1", right)$
Tape: ["1","1",".","-","1",<"=">,"."] ("scanright", "=") -> ("eraseone", ".", left)$
Tape: ["1","1",".","-",<"1">,".","."] ("eraseone", "1") -> ("subone", "=", left)$
Tape: ["1","1",".",<"-">,"=",".","."] ("subone", "-") -> ("skip", "-", left)$
Tape: ["1","1",<".">,"-","=",".","."] ("skip", ".") -> ("skip", ".", left)$
Tape: ["1",<"1">,".","-","=",".","."] ("skip", "1") -> ("scanright", ".", right)$
Tape: ["1",".",<".">,"-","=",".","."] ("scanright", ".") -> ("scanright", ".", right)$
Tape: ["1",".",".",<"-">,"=",".","."] ("scanright", "-") -> ("scanright", "-", right)$
Tape: ["1",".",".","-",<"=">,".","."] ("scanright", "=") -> ("eraseone", ".", left)$
Tape: ["1",".",".",<"-">,".",".","."] ("eraseone", "-") -> ("HALT", ".", left)$
Tape: ["1",".",<".">,".",".",".","."] Final state reached !$
Interpreter closing...$'
}