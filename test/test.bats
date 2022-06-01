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
    run bash -c './_build/default/bin/turing machines/unary_sub.json "111-11=" | cat -e'
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

@test "Display usage on empty args execution" {
    run bash -c './_build/default/bin/turing | cat -e'
    assert_output 'usage: ft_turing [-h] jsonfile input$
$
positional arguments:$
  jsonfile          json description of the machine$
$
  input             input of the machine$
$
optional arguments:$
  -h, --help        show this help message and exit$'
}

@test "Display usage on too many args execution" {
    run bash -c './_build/default/bin/turing abc.json "abc" --help cocorico | cat -e'
    assert_output 'usage: ft_turing [-h] jsonfile input$
$
positional arguments:$
  jsonfile          json description of the machine$
$
  input             input of the machine$
$
optional arguments:$
  -h, --help        show this help message and exit$'
}

@test "Display usage on --help flag execution" {
    run bash -c './_build/default/bin/turing --help abc.json "abc" cocorico | cat -e'
    assert_output 'usage: ft_turing [-h] jsonfile input$
$
positional arguments:$
  jsonfile          json description of the machine$
$
  input             input of the machine$
$
optional arguments:$
  -h, --help        show this help message and exit$'
}

@test "Machine configuration json file not found" {
    run bash -c './_build/default/bin/turing abc.json "abc" | cat -e'
    assert_output 'Error while reading machine configuration json file: enoent$'
}

@test "Machine configuration json file cannot be decoded" {
    run bash -c './_build/default/bin/turing machines/not-a-json.json "abc" | cat -e'
    assert_output 'Error while decoding machine configuration json file: badarg$'
}

@test "Transform empty input into an array with a blank character" {
    run bash -c '_build/default/bin/turing machines/basic-machine.json "" | cat -e'
    assert_output 'Interpreter starting...$
Tape: [<".">] ("IDLE", ".") -> ("HALT", ".", left)$
Tape: [<".">,"."] Final state reached !$
Interpreter closing...$'
}

@test "Prevent unknown character in input" {
    run bash -c '_build/default/bin/turing machines/basic-machine.json "z" | cat -e'
    assert_output 'Character "z" is not in the alphabet$'
}

@test "Prevent blank character in input" {
    run bash -c '_build/default/bin/turing machines/basic-machine.json "." | cat -e'
    assert_output 'Blank character is forbidden in input$'
}

@test "Log machine configuration parsing error" {
    run bash -c '_build/default/bin/turing machines/machine-failing-parsing.json "." | cat -e'
    assert_output 'Error occured during machine configuration parsing:$
$
machine alphabet is empty; a machine must have a non-empty alphabet, made of strings with exactly one character$'
}

@test "Log machine validation on transition read error" {
    run bash -c '_build/default/bin/turing machines/invalid-machine-transition.json "11-1=" | cat -e'
    assert_output 'Error occured during machine configuration validation:$
$
machine transition "skip" has duplicated read operation (["."]); machine transitions must be scoped to a listed state, must only contain unique read character per transition and a listed to_state target$'
}

@test "Log machine validation on states error" {
    run bash -c '_build/default/bin/turing machines/invalid-machine-states.json "11-1=" | cat -e'
    assert_output 'Error occured during machine configuration validation:$
$
machine states has duplicated elements (["subone"]); machine states must contains unique elements$'
}
