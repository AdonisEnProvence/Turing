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
    run bash -c './_build/default/bin/turing run machines/unary_sub.json "111-11=" | cat -e'
    assert_output 'Interpreter starting...$
[<1>11-11=] (scanright, 1) -> (scanright, 1, right)$
[1<1>1-11=] (scanright, 1) -> (scanright, 1, right)$
[11<1>-11=] (scanright, 1) -> (scanright, 1, right)$
[111<->11=] (scanright, -) -> (scanright, -, right)$
[111-<1>1=] (scanright, 1) -> (scanright, 1, right)$
[111-1<1>=] (scanright, 1) -> (scanright, 1, right)$
[111-11<=>] (scanright, =) -> (eraseone, ., left)$
[111-1<1>.] (eraseone, 1) -> (subone, =, left)$
[111-<1>=.] (subone, 1) -> (subone, 1, left)$
[111<->1=.] (subone, -) -> (skip, -, left)$
[11<1>-1=.] (skip, 1) -> (scanright, ., right)$
[11.<->1=.] (scanright, -) -> (scanright, -, right)$
[11.-<1>=.] (scanright, 1) -> (scanright, 1, right)$
[11.-1<=>.] (scanright, =) -> (eraseone, ., left)$
[11.-<1>..] (eraseone, 1) -> (subone, =, left)$
[11.<->=..] (subone, -) -> (skip, -, left)$
[11<.>-=..] (skip, .) -> (skip, ., left)$
[1<1>.-=..] (skip, 1) -> (scanright, ., right)$
[1.<.>-=..] (scanright, .) -> (scanright, ., right)$
[1..<->=..] (scanright, -) -> (scanright, -, right)$
[1..-<=>..] (scanright, =) -> (eraseone, ., left)$
[1..<->...] (eraseone, -) -> (HALT, ., left)$
[1.<.>....] Final state reached !$
Interpreter closing...$'
}

@test "Display global usage on empty required args" {
    run bash -c './_build/default/bin/turing | cat -e'
    assert_output "usage: ft_turing COMMAND$
$
commands:$
  serve          starts the turing machine http REST API$
  run            runs cli turing machine command$
$
Run 'ft_turing COMMAND --help' for more information on a command.$"
}

@test "Display global usage on invalid required args" {
    run bash -c './_build/default/bin/turing coco | cat -e'
    assert_output "usage: ft_turing COMMAND$
$
commands:$
  serve          starts the turing machine http REST API$
  run            runs cli turing machine command$
$
Run 'ft_turing COMMAND --help' for more information on a command.$"
}


@test "Display usage on empty args execution and say that jsonfile argument is required" {
    run bash -c './_build/default/bin/turing run | cat -e'
    assert_output 'Error: missing jsonfile argument$
usage: ft_turing run [-h] jsonfile input$
$
positional arguments:$
  jsonfile          json description of the machine$
$
  input             input of the machine$
$
optional arguments:$
  -c, --color       output head position with color$
  -h, --help        show this help message and exit$'
}

@test "Display usage when input is not provided and say that input argument is required" {
    run bash -c './_build/default/bin/turing run abc.json | cat -e'
    assert_output 'Error: missing input argument$
usage: ft_turing run [-h] jsonfile input$
$
positional arguments:$
  jsonfile          json description of the machine$
$
  input             input of the machine$
$
optional arguments:$
  -c, --color       output head position with color$
  -h, --help        show this help message and exit$'
}

@test "Does nothing on too many args execution" {
    run bash -c './_build/default/bin/turing run abc.json "abc" cocorico | cat -e'
    assert_output 'Error while reading machine configuration: no such file or directory$'
}

@test "Display usage on --help flag execution" {
    run bash -c './_build/default/bin/turing run --help abc.json "abc" cocorico | cat -e'
    assert_output 'usage: ft_turing run [-h] jsonfile input$
$
positional arguments:$
  jsonfile          json description of the machine$
$
  input             input of the machine$
$
optional arguments:$
  -c, --color       output head position with color$
  -h, --help        show this help message and exit$'
}

@test "Display usage on -h flag execution" {
    run bash -c './_build/default/bin/turing run -h abc.json "abc" | cat -e'
    assert_output 'usage: ft_turing run [-h] jsonfile input$
$
positional arguments:$
  jsonfile          json description of the machine$
$
  input             input of the machine$
$
optional arguments:$
  -c, --color       output head position with color$
  -h, --help        show this help message and exit$'
}

@test "Help flag is taken into account wherever it is in the arguments" {
    run bash -c './_build/default/bin/turing run abc.json -h "abc" | cat -e'
    assert_output 'usage: ft_turing run [-h] jsonfile input$
$
positional arguments:$
  jsonfile          json description of the machine$
$
  input             input of the machine$
$
optional arguments:$
  -c, --color       output head position with color$
  -h, --help        show this help message and exit$'
}

@test "Machine configuration json file not found" {
    run bash -c './_build/default/bin/turing run abc.json "abc" | cat -e'
    assert_output 'Error while reading machine configuration: no such file or directory$'
}

@test "Try to read a directory" {
    run bash -c './_build/default/bin/turing run machines "abc" | cat -e'
    assert_output 'Error while reading machine configuration: illegal operation on a directory$'
}

@test "Machine configuration json file cannot be decoded" {
    run bash -c './_build/default/bin/turing run machines/not-a-json.json "abc" | cat -e'
    assert_output 'Error while decoding machine configuration: can not decode invalid json file$'
}

@test "Transform empty input into an array with a blank character" {
    run bash -c '_build/default/bin/turing run machines/basic-machine.json "" | cat -e'
    assert_output 'Interpreter starting...$
[<.>] (IDLE, .) -> (HALT, ., left)$
[<.>.] Final state reached !$
Interpreter closing...$'
}

@test "Prevent unknown character in input" {
    run bash -c '_build/default/bin/turing run machines/basic-machine.json "z" | cat -e'
    assert_output 'Character "z" is not in the alphabet$'
}

@test "Prevent blank character in input" {
    run bash -c '_build/default/bin/turing run machines/basic-machine.json "." | cat -e'
    assert_output 'Blank character is forbidden in input$'
}

@test "Log machine configuration parsing error" {
    run bash -c '_build/default/bin/turing run machines/machine-failing-parsing.json "." | cat -e'
    assert_output 'Error occured during machine configuration parsing:$
$
machine alphabet is empty; a machine must have a non-empty alphabet, made of strings with exactly one character$'
}

@test "Log machine validation on transition read error" {
    run bash -c '_build/default/bin/turing run machines/invalid-machine-transition.json "11-1=" | cat -e'
    assert_output 'Error occured during machine configuration validation:$
$
machine transition "skip" has duplicated read operation (["."]); machine transitions must be scoped to a listed state, must only contain unique read character per transition and a listed to_state target$'
}

@test "Log machine validation on states error" {
    run bash -c '_build/default/bin/turing run machines/invalid-machine-states.json "11-1=" | cat -e'
    assert_output 'Error occured during machine configuration validation:$
$
machine states has duplicated elements (["subone"]); machine states must contains unique elements$'
}

@test "Color mode is disabled by default" {
    run bash -c '_build/default/bin/turing run machines/unary_sub.json "1-1=" | cat -e'
    assert_output 'Interpreter starting...$
[<1>-1=] (scanright, 1) -> (scanright, 1, right)$
[1<->1=] (scanright, -) -> (scanright, -, right)$
[1-<1>=] (scanright, 1) -> (scanright, 1, right)$
[1-1<=>] (scanright, =) -> (eraseone, ., left)$
[1-<1>.] (eraseone, 1) -> (subone, =, left)$
[1<->=.] (subone, -) -> (skip, -, left)$
[<1>-=.] (skip, 1) -> (scanright, ., right)$
[.<->=.] (scanright, -) -> (scanright, -, right)$
[.-<=>.] (scanright, =) -> (eraseone, ., left)$
[.<->..] (eraseone, -) -> (HALT, ., left)$
[<.>...] Final state reached !$
Interpreter closing...$'
}

@test "Output head with color when color flag is set" {
    run bash -c '_build/default/bin/turing run machines/unary_sub.json "1-1=" --color | cat -e'
    assert_output 'Interpreter starting...$
[^[[0;101m1^[[0m-1=] (scanright, 1) -> (scanright, 1, right)$
[1^[[0;101m-^[[0m1=] (scanright, -) -> (scanright, -, right)$
[1-^[[0;101m1^[[0m=] (scanright, 1) -> (scanright, 1, right)$
[1-1^[[0;101m=^[[0m] (scanright, =) -> (eraseone, ., left)$
[1-^[[0;101m1^[[0m.] (eraseone, 1) -> (subone, =, left)$
[1^[[0;101m-^[[0m=.] (subone, -) -> (skip, -, left)$
[^[[0;101m1^[[0m-=.] (skip, 1) -> (scanright, ., right)$
[.^[[0;101m-^[[0m=.] (scanright, -) -> (scanright, -, right)$
[.-^[[0;101m=^[[0m.] (scanright, =) -> (eraseone, ., left)$
[.^[[0;101m-^[[0m..] (eraseone, -) -> (HALT, ., left)$
[^[[0;101m.^[[0m...] Final state reached !$
Interpreter closing...$'
}

@test "Output head with color when short color flag is set" {
    run bash -c '_build/default/bin/turing run machines/unary_sub.json "1-1=" -c | cat -e'
    assert_output 'Interpreter starting...$
[^[[0;101m1^[[0m-1=] (scanright, 1) -> (scanright, 1, right)$
[1^[[0;101m-^[[0m1=] (scanright, -) -> (scanright, -, right)$
[1-^[[0;101m1^[[0m=] (scanright, 1) -> (scanright, 1, right)$
[1-1^[[0;101m=^[[0m] (scanright, =) -> (eraseone, ., left)$
[1-^[[0;101m1^[[0m.] (eraseone, 1) -> (subone, =, left)$
[1^[[0;101m-^[[0m=.] (subone, -) -> (skip, -, left)$
[^[[0;101m1^[[0m-=.] (skip, 1) -> (scanright, ., right)$
[.^[[0;101m-^[[0m=.] (scanright, -) -> (scanright, -, right)$
[.-^[[0;101m=^[[0m.] (scanright, =) -> (eraseone, ., left)$
[.^[[0;101m-^[[0m..] (eraseone, -) -> (HALT, ., left)$
[^[[0;101m.^[[0m...] Final state reached !$
Interpreter closing...$'
}

@test "Color flag can be explicitly disabled" {
    run bash -c '_build/default/bin/turing run machines/unary_sub.json "1-1=" --color=false | cat -e'
    assert_output 'Interpreter starting...$
[<1>-1=] (scanright, 1) -> (scanright, 1, right)$
[1<->1=] (scanright, -) -> (scanright, -, right)$
[1-<1>=] (scanright, 1) -> (scanright, 1, right)$
[1-1<=>] (scanright, =) -> (eraseone, ., left)$
[1-<1>.] (eraseone, 1) -> (subone, =, left)$
[1<->=.] (subone, -) -> (skip, -, left)$
[<1>-=.] (skip, 1) -> (scanright, ., right)$
[.<->=.] (scanright, -) -> (scanright, -, right)$
[.-<=>.] (scanright, =) -> (eraseone, ., left)$
[.<->..] (eraseone, -) -> (HALT, ., left)$
[<.>...] Final state reached !$
Interpreter closing...$'
}
