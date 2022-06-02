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

@test "'' is palindrome" {
    run bash -c './_build/default/bin/turing our-machines/palindrome.json "" | cat -e'
    assert_output 'Interpreter starting...$
Tape: [<".">] ("pick_character", ".") -> ("write_is_palindrome", ".", right)$
Tape: [".",<".">] ("write_is_palindrome", ".") -> ("HALT", "y", right)$
Tape: [".","y",<".">] Final state reached !$
Interpreter closing...$'
}

@test "'a' is palindrome" {
    run bash -c './_build/default/bin/turing our-machines/palindrome.json "a" | cat -e'
    assert_output 'Interpreter starting...$
Tape: [<"a">] ("pick_character", "a") -> ("go_to_end_and_find_a", ".", right)$
Tape: [".",<".">] ("go_to_end_and_find_a", ".") -> ("is_a", ".", left)$
Tape: [<".">,"."] ("is_a", ".") -> ("write_is_palindrome", ".", right)$
Tape: [".",<".">] ("write_is_palindrome", ".") -> ("HALT", "y", right)$
Tape: [".","y",<".">] Final state reached !$
Interpreter closing...$'
}

@test "'aa' is palindrome" {
    run bash -c './_build/default/bin/turing our-machines/palindrome.json "aa" | cat -e'
    assert_output 'Interpreter starting...$
Tape: [<"a">,"a"] ("pick_character", "a") -> ("go_to_end_and_find_a", ".", right)$
Tape: [".",<"a">] ("go_to_end_and_find_a", "a") -> ("go_to_end_and_find_a", "a", right)$
Tape: [".","a",<".">] ("go_to_end_and_find_a", ".") -> ("is_a", ".", left)$
Tape: [".",<"a">,"."] ("is_a", "a") -> ("go_to_beginning", ".", left)$
Tape: [<".">,".","."] ("go_to_beginning", ".") -> ("pick_character", ".", right)$
Tape: [".",<".">,"."] ("pick_character", ".") -> ("write_is_palindrome", ".", right)$
Tape: [".",".",<".">] ("write_is_palindrome", ".") -> ("HALT", "y", right)$
Tape: [".",".","y",<".">] Final state reached !$
Interpreter closing...$'
}

@test "'aba' is palindrome" {
    run bash -c './_build/default/bin/turing our-machines/palindrome.json "aba" | cat -e'
    assert_output 'Interpreter starting...$
Tape: [<"a">,"b","a"] ("pick_character", "a") -> ("go_to_end_and_find_a", ".", right)$
Tape: [".",<"b">,"a"] ("go_to_end_and_find_a", "b") -> ("go_to_end_and_find_a", "b", right)$
Tape: [".","b",<"a">] ("go_to_end_and_find_a", "a") -> ("go_to_end_and_find_a", "a", right)$
Tape: [".","b","a",<".">] ("go_to_end_and_find_a", ".") -> ("is_a", ".", left)$
Tape: [".","b",<"a">,"."] ("is_a", "a") -> ("go_to_beginning", ".", left)$
Tape: [".",<"b">,".","."] ("go_to_beginning", "b") -> ("go_to_beginning", "b", left)$
Tape: [<".">,"b",".","."] ("go_to_beginning", ".") -> ("pick_character", ".", right)$
Tape: [".",<"b">,".","."] ("pick_character", "b") -> ("go_to_end_and_find_b", ".", right)$
Tape: [".",".",<".">,"."] ("go_to_end_and_find_b", ".") -> ("is_b", ".", left)$
Tape: [".",<".">,".","."] ("is_b", ".") -> ("write_is_palindrome", ".", right)$
Tape: [".",".",<".">,"."] ("write_is_palindrome", ".") -> ("HALT", "y", right)$
Tape: [".",".","y",<".">] Final state reached !$
Interpreter closing...$'
}

@test "'abba' is palindrome" {
    run bash -c './_build/default/bin/turing our-machines/palindrome.json "abba" | cat -e'
    assert_output 'Interpreter starting...$
Tape: [<"a">,"b","b","a"] ("pick_character", "a") -> ("go_to_end_and_find_a", ".", right)$
Tape: [".",<"b">,"b","a"] ("go_to_end_and_find_a", "b") -> ("go_to_end_and_find_a", "b", right)$
Tape: [".","b",<"b">,"a"] ("go_to_end_and_find_a", "b") -> ("go_to_end_and_find_a", "b", right)$
Tape: [".","b","b",<"a">] ("go_to_end_and_find_a", "a") -> ("go_to_end_and_find_a", "a", right)$
Tape: [".","b","b","a",<".">] ("go_to_end_and_find_a", ".") -> ("is_a", ".", left)$
Tape: [".","b","b",<"a">,"."] ("is_a", "a") -> ("go_to_beginning", ".", left)$
Tape: [".","b",<"b">,".","."] ("go_to_beginning", "b") -> ("go_to_beginning", "b", left)$
Tape: [".",<"b">,"b",".","."] ("go_to_beginning", "b") -> ("go_to_beginning", "b", left)$
Tape: [<".">,"b","b",".","."] ("go_to_beginning", ".") -> ("pick_character", ".", right)$
Tape: [".",<"b">,"b",".","."] ("pick_character", "b") -> ("go_to_end_and_find_b", ".", right)$
Tape: [".",".",<"b">,".","."] ("go_to_end_and_find_b", "b") -> ("go_to_end_and_find_b", "b", right)$
Tape: [".",".","b",<".">,"."] ("go_to_end_and_find_b", ".") -> ("is_b", ".", left)$
Tape: [".",".",<"b">,".","."] ("is_b", "b") -> ("go_to_beginning", ".", left)$
Tape: [".",<".">,".",".","."] ("go_to_beginning", ".") -> ("pick_character", ".", right)$
Tape: [".",".",<".">,".","."] ("pick_character", ".") -> ("write_is_palindrome", ".", right)$
Tape: [".",".",".",<".">,"."] ("write_is_palindrome", ".") -> ("HALT", "y", right)$
Tape: [".",".",".","y",<".">] Final state reached !$
Interpreter closing...$'
}

@test "'aca' is palindrome" {
    run bash -c './_build/default/bin/turing our-machines/palindrome.json "aca" | cat -e'
    assert_output 'Interpreter starting...$
Tape: [<"a">,"c","a"] ("pick_character", "a") -> ("go_to_end_and_find_a", ".", right)$
Tape: [".",<"c">,"a"] ("go_to_end_and_find_a", "c") -> ("go_to_end_and_find_a", "c", right)$
Tape: [".","c",<"a">] ("go_to_end_and_find_a", "a") -> ("go_to_end_and_find_a", "a", right)$
Tape: [".","c","a",<".">] ("go_to_end_and_find_a", ".") -> ("is_a", ".", left)$
Tape: [".","c",<"a">,"."] ("is_a", "a") -> ("go_to_beginning", ".", left)$
Tape: [".",<"c">,".","."] ("go_to_beginning", "c") -> ("go_to_beginning", "c", left)$
Tape: [<".">,"c",".","."] ("go_to_beginning", ".") -> ("pick_character", ".", right)$
Tape: [".",<"c">,".","."] ("pick_character", "c") -> ("go_to_end_and_find_c", ".", right)$
Tape: [".",".",<".">,"."] ("go_to_end_and_find_c", ".") -> ("is_c", ".", left)$
Tape: [".",<".">,".","."] ("is_c", ".") -> ("write_is_palindrome", ".", right)$
Tape: [".",".",<".">,"."] ("write_is_palindrome", ".") -> ("HALT", "y", right)$
Tape: [".",".","y",<".">] Final state reached !$
Interpreter closing...$'
}

@test "'acca' is palindrome" {
    run bash -c './_build/default/bin/turing our-machines/palindrome.json "acca" | cat -e'
    assert_output 'Interpreter starting...$
Tape: [<"a">,"c","c","a"] ("pick_character", "a") -> ("go_to_end_and_find_a", ".", right)$
Tape: [".",<"c">,"c","a"] ("go_to_end_and_find_a", "c") -> ("go_to_end_and_find_a", "c", right)$
Tape: [".","c",<"c">,"a"] ("go_to_end_and_find_a", "c") -> ("go_to_end_and_find_a", "c", right)$
Tape: [".","c","c",<"a">] ("go_to_end_and_find_a", "a") -> ("go_to_end_and_find_a", "a", right)$
Tape: [".","c","c","a",<".">] ("go_to_end_and_find_a", ".") -> ("is_a", ".", left)$
Tape: [".","c","c",<"a">,"."] ("is_a", "a") -> ("go_to_beginning", ".", left)$
Tape: [".","c",<"c">,".","."] ("go_to_beginning", "c") -> ("go_to_beginning", "c", left)$
Tape: [".",<"c">,"c",".","."] ("go_to_beginning", "c") -> ("go_to_beginning", "c", left)$
Tape: [<".">,"c","c",".","."] ("go_to_beginning", ".") -> ("pick_character", ".", right)$
Tape: [".",<"c">,"c",".","."] ("pick_character", "c") -> ("go_to_end_and_find_c", ".", right)$
Tape: [".",".",<"c">,".","."] ("go_to_end_and_find_c", "c") -> ("go_to_end_and_find_c", "c", right)$
Tape: [".",".","c",<".">,"."] ("go_to_end_and_find_c", ".") -> ("is_c", ".", left)$
Tape: [".",".",<"c">,".","."] ("is_c", "c") -> ("go_to_beginning", ".", left)$
Tape: [".",<".">,".",".","."] ("go_to_beginning", ".") -> ("pick_character", ".", right)$
Tape: [".",".",<".">,".","."] ("pick_character", ".") -> ("write_is_palindrome", ".", right)$
Tape: [".",".",".",<".">,"."] ("write_is_palindrome", ".") -> ("HALT", "y", right)$
Tape: [".",".",".","y",<".">] Final state reached !$
Interpreter closing...$'
}

@test "'abcba' is palindrome" {
    run bash -c './_build/default/bin/turing our-machines/palindrome.json "abcba" | cat -e'
    assert_output 'Interpreter starting...$
Tape: [<"a">,"b","c","b","a"] ("pick_character", "a") -> ("go_to_end_and_find_a", ".", right)$
Tape: [".",<"b">,"c","b","a"] ("go_to_end_and_find_a", "b") -> ("go_to_end_and_find_a", "b", right)$
Tape: [".","b",<"c">,"b","a"] ("go_to_end_and_find_a", "c") -> ("go_to_end_and_find_a", "c", right)$
Tape: [".","b","c",<"b">,"a"] ("go_to_end_and_find_a", "b") -> ("go_to_end_and_find_a", "b", right)$
Tape: [".","b","c","b",<"a">] ("go_to_end_and_find_a", "a") -> ("go_to_end_and_find_a", "a", right)$
Tape: [".","b","c","b","a",<".">] ("go_to_end_and_find_a", ".") -> ("is_a", ".", left)$
Tape: [".","b","c","b",<"a">,"."] ("is_a", "a") -> ("go_to_beginning", ".", left)$
Tape: [".","b","c",<"b">,".","."] ("go_to_beginning", "b") -> ("go_to_beginning", "b", left)$
Tape: [".","b",<"c">,"b",".","."] ("go_to_beginning", "c") -> ("go_to_beginning", "c", left)$
Tape: [".",<"b">,"c","b",".","."] ("go_to_beginning", "b") -> ("go_to_beginning", "b", left)$
Tape: [<".">,"b","c","b",".","."] ("go_to_beginning", ".") -> ("pick_character", ".", right)$
Tape: [".",<"b">,"c","b",".","."] ("pick_character", "b") -> ("go_to_end_and_find_b", ".", right)$
Tape: [".",".",<"c">,"b",".","."] ("go_to_end_and_find_b", "c") -> ("go_to_end_and_find_b", "c", right)$
Tape: [".",".","c",<"b">,".","."] ("go_to_end_and_find_b", "b") -> ("go_to_end_and_find_b", "b", right)$
Tape: [".",".","c","b",<".">,"."] ("go_to_end_and_find_b", ".") -> ("is_b", ".", left)$
Tape: [".",".","c",<"b">,".","."] ("is_b", "b") -> ("go_to_beginning", ".", left)$
Tape: [".",".",<"c">,".",".","."] ("go_to_beginning", "c") -> ("go_to_beginning", "c", left)$
Tape: [".",<".">,"c",".",".","."] ("go_to_beginning", ".") -> ("pick_character", ".", right)$
Tape: [".",".",<"c">,".",".","."] ("pick_character", "c") -> ("go_to_end_and_find_c", ".", right)$
Tape: [".",".",".",<".">,".","."] ("go_to_end_and_find_c", ".") -> ("is_c", ".", left)$
Tape: [".",".",<".">,".",".","."] ("is_c", ".") -> ("write_is_palindrome", ".", right)$
Tape: [".",".",".",<".">,".","."] ("write_is_palindrome", ".") -> ("HALT", "y", right)$
Tape: [".",".",".","y",<".">,"."] Final state reached !$
Interpreter closing...$'
}

@test "'b' is palindrome" {
    run bash -c './_build/default/bin/turing our-machines/palindrome.json "b" | cat -e'
    assert_output 'Interpreter starting...$
Tape: [<"b">] ("pick_character", "b") -> ("go_to_end_and_find_b", ".", right)$
Tape: [".",<".">] ("go_to_end_and_find_b", ".") -> ("is_b", ".", left)$
Tape: [<".">,"."] ("is_b", ".") -> ("write_is_palindrome", ".", right)$
Tape: [".",<".">] ("write_is_palindrome", ".") -> ("HALT", "y", right)$
Tape: [".","y",<".">] Final state reached !$
Interpreter closing...$'
}

@test "'bb' is palindrome" {
    run bash -c './_build/default/bin/turing our-machines/palindrome.json "bb" | cat -e'
    assert_output 'Interpreter starting...$
Tape: [<"b">,"b"] ("pick_character", "b") -> ("go_to_end_and_find_b", ".", right)$
Tape: [".",<"b">] ("go_to_end_and_find_b", "b") -> ("go_to_end_and_find_b", "b", right)$
Tape: [".","b",<".">] ("go_to_end_and_find_b", ".") -> ("is_b", ".", left)$
Tape: [".",<"b">,"."] ("is_b", "b") -> ("go_to_beginning", ".", left)$
Tape: [<".">,".","."] ("go_to_beginning", ".") -> ("pick_character", ".", right)$
Tape: [".",<".">,"."] ("pick_character", ".") -> ("write_is_palindrome", ".", right)$
Tape: [".",".",<".">] ("write_is_palindrome", ".") -> ("HALT", "y", right)$
Tape: [".",".","y",<".">] Final state reached !$
Interpreter closing...$'
}

@test "'bab' is palindrome" {
    run bash -c './_build/default/bin/turing our-machines/palindrome.json "bab" | cat -e'
    assert_output 'Interpreter starting...$
Tape: [<"b">,"a","b"] ("pick_character", "b") -> ("go_to_end_and_find_b", ".", right)$
Tape: [".",<"a">,"b"] ("go_to_end_and_find_b", "a") -> ("go_to_end_and_find_b", "a", right)$
Tape: [".","a",<"b">] ("go_to_end_and_find_b", "b") -> ("go_to_end_and_find_b", "b", right)$
Tape: [".","a","b",<".">] ("go_to_end_and_find_b", ".") -> ("is_b", ".", left)$
Tape: [".","a",<"b">,"."] ("is_b", "b") -> ("go_to_beginning", ".", left)$
Tape: [".",<"a">,".","."] ("go_to_beginning", "a") -> ("go_to_beginning", "a", left)$
Tape: [<".">,"a",".","."] ("go_to_beginning", ".") -> ("pick_character", ".", right)$
Tape: [".",<"a">,".","."] ("pick_character", "a") -> ("go_to_end_and_find_a", ".", right)$
Tape: [".",".",<".">,"."] ("go_to_end_and_find_a", ".") -> ("is_a", ".", left)$
Tape: [".",<".">,".","."] ("is_a", ".") -> ("write_is_palindrome", ".", right)$
Tape: [".",".",<".">,"."] ("write_is_palindrome", ".") -> ("HALT", "y", right)$
Tape: [".",".","y",<".">] Final state reached !$
Interpreter closing...$'
}

@test "'baab' is palindrome" {
    run bash -c './_build/default/bin/turing our-machines/palindrome.json "baab" | cat -e'
    assert_output 'Interpreter starting...$
Tape: [<"b">,"a","a","b"] ("pick_character", "b") -> ("go_to_end_and_find_b", ".", right)$
Tape: [".",<"a">,"a","b"] ("go_to_end_and_find_b", "a") -> ("go_to_end_and_find_b", "a", right)$
Tape: [".","a",<"a">,"b"] ("go_to_end_and_find_b", "a") -> ("go_to_end_and_find_b", "a", right)$
Tape: [".","a","a",<"b">] ("go_to_end_and_find_b", "b") -> ("go_to_end_and_find_b", "b", right)$
Tape: [".","a","a","b",<".">] ("go_to_end_and_find_b", ".") -> ("is_b", ".", left)$
Tape: [".","a","a",<"b">,"."] ("is_b", "b") -> ("go_to_beginning", ".", left)$
Tape: [".","a",<"a">,".","."] ("go_to_beginning", "a") -> ("go_to_beginning", "a", left)$
Tape: [".",<"a">,"a",".","."] ("go_to_beginning", "a") -> ("go_to_beginning", "a", left)$
Tape: [<".">,"a","a",".","."] ("go_to_beginning", ".") -> ("pick_character", ".", right)$
Tape: [".",<"a">,"a",".","."] ("pick_character", "a") -> ("go_to_end_and_find_a", ".", right)$
Tape: [".",".",<"a">,".","."] ("go_to_end_and_find_a", "a") -> ("go_to_end_and_find_a", "a", right)$
Tape: [".",".","a",<".">,"."] ("go_to_end_and_find_a", ".") -> ("is_a", ".", left)$
Tape: [".",".",<"a">,".","."] ("is_a", "a") -> ("go_to_beginning", ".", left)$
Tape: [".",<".">,".",".","."] ("go_to_beginning", ".") -> ("pick_character", ".", right)$
Tape: [".",".",<".">,".","."] ("pick_character", ".") -> ("write_is_palindrome", ".", right)$
Tape: [".",".",".",<".">,"."] ("write_is_palindrome", ".") -> ("HALT", "y", right)$
Tape: [".",".",".","y",<".">] Final state reached !$
Interpreter closing...$'
}

@test "'bcb' is palindrome" {
    run bash -c './_build/default/bin/turing our-machines/palindrome.json "bcb" | cat -e'
    assert_output 'Interpreter starting...$
Tape: [<"b">,"c","b"] ("pick_character", "b") -> ("go_to_end_and_find_b", ".", right)$
Tape: [".",<"c">,"b"] ("go_to_end_and_find_b", "c") -> ("go_to_end_and_find_b", "c", right)$
Tape: [".","c",<"b">] ("go_to_end_and_find_b", "b") -> ("go_to_end_and_find_b", "b", right)$
Tape: [".","c","b",<".">] ("go_to_end_and_find_b", ".") -> ("is_b", ".", left)$
Tape: [".","c",<"b">,"."] ("is_b", "b") -> ("go_to_beginning", ".", left)$
Tape: [".",<"c">,".","."] ("go_to_beginning", "c") -> ("go_to_beginning", "c", left)$
Tape: [<".">,"c",".","."] ("go_to_beginning", ".") -> ("pick_character", ".", right)$
Tape: [".",<"c">,".","."] ("pick_character", "c") -> ("go_to_end_and_find_c", ".", right)$
Tape: [".",".",<".">,"."] ("go_to_end_and_find_c", ".") -> ("is_c", ".", left)$
Tape: [".",<".">,".","."] ("is_c", ".") -> ("write_is_palindrome", ".", right)$
Tape: [".",".",<".">,"."] ("write_is_palindrome", ".") -> ("HALT", "y", right)$
Tape: [".",".","y",<".">] Final state reached !$
Interpreter closing...$'
}

@test "'bccb' is palindrome" {
    run bash -c './_build/default/bin/turing our-machines/palindrome.json "bccb" | cat -e'
    assert_output 'Interpreter starting...$
Tape: [<"b">,"c","c","b"] ("pick_character", "b") -> ("go_to_end_and_find_b", ".", right)$
Tape: [".",<"c">,"c","b"] ("go_to_end_and_find_b", "c") -> ("go_to_end_and_find_b", "c", right)$
Tape: [".","c",<"c">,"b"] ("go_to_end_and_find_b", "c") -> ("go_to_end_and_find_b", "c", right)$
Tape: [".","c","c",<"b">] ("go_to_end_and_find_b", "b") -> ("go_to_end_and_find_b", "b", right)$
Tape: [".","c","c","b",<".">] ("go_to_end_and_find_b", ".") -> ("is_b", ".", left)$
Tape: [".","c","c",<"b">,"."] ("is_b", "b") -> ("go_to_beginning", ".", left)$
Tape: [".","c",<"c">,".","."] ("go_to_beginning", "c") -> ("go_to_beginning", "c", left)$
Tape: [".",<"c">,"c",".","."] ("go_to_beginning", "c") -> ("go_to_beginning", "c", left)$
Tape: [<".">,"c","c",".","."] ("go_to_beginning", ".") -> ("pick_character", ".", right)$
Tape: [".",<"c">,"c",".","."] ("pick_character", "c") -> ("go_to_end_and_find_c", ".", right)$
Tape: [".",".",<"c">,".","."] ("go_to_end_and_find_c", "c") -> ("go_to_end_and_find_c", "c", right)$
Tape: [".",".","c",<".">,"."] ("go_to_end_and_find_c", ".") -> ("is_c", ".", left)$
Tape: [".",".",<"c">,".","."] ("is_c", "c") -> ("go_to_beginning", ".", left)$
Tape: [".",<".">,".",".","."] ("go_to_beginning", ".") -> ("pick_character", ".", right)$
Tape: [".",".",<".">,".","."] ("pick_character", ".") -> ("write_is_palindrome", ".", right)$
Tape: [".",".",".",<".">,"."] ("write_is_palindrome", ".") -> ("HALT", "y", right)$
Tape: [".",".",".","y",<".">] Final state reached !$
Interpreter closing...$'
}

@test "'bcacb' is palindrome" {
    run bash -c './_build/default/bin/turing our-machines/palindrome.json "bcacb" | cat -e'
    assert_output 'Interpreter starting...$
Tape: [<"b">,"c","a","c","b"] ("pick_character", "b") -> ("go_to_end_and_find_b", ".", right)$
Tape: [".",<"c">,"a","c","b"] ("go_to_end_and_find_b", "c") -> ("go_to_end_and_find_b", "c", right)$
Tape: [".","c",<"a">,"c","b"] ("go_to_end_and_find_b", "a") -> ("go_to_end_and_find_b", "a", right)$
Tape: [".","c","a",<"c">,"b"] ("go_to_end_and_find_b", "c") -> ("go_to_end_and_find_b", "c", right)$
Tape: [".","c","a","c",<"b">] ("go_to_end_and_find_b", "b") -> ("go_to_end_and_find_b", "b", right)$
Tape: [".","c","a","c","b",<".">] ("go_to_end_and_find_b", ".") -> ("is_b", ".", left)$
Tape: [".","c","a","c",<"b">,"."] ("is_b", "b") -> ("go_to_beginning", ".", left)$
Tape: [".","c","a",<"c">,".","."] ("go_to_beginning", "c") -> ("go_to_beginning", "c", left)$
Tape: [".","c",<"a">,"c",".","."] ("go_to_beginning", "a") -> ("go_to_beginning", "a", left)$
Tape: [".",<"c">,"a","c",".","."] ("go_to_beginning", "c") -> ("go_to_beginning", "c", left)$
Tape: [<".">,"c","a","c",".","."] ("go_to_beginning", ".") -> ("pick_character", ".", right)$
Tape: [".",<"c">,"a","c",".","."] ("pick_character", "c") -> ("go_to_end_and_find_c", ".", right)$
Tape: [".",".",<"a">,"c",".","."] ("go_to_end_and_find_c", "a") -> ("go_to_end_and_find_c", "a", right)$
Tape: [".",".","a",<"c">,".","."] ("go_to_end_and_find_c", "c") -> ("go_to_end_and_find_c", "c", right)$
Tape: [".",".","a","c",<".">,"."] ("go_to_end_and_find_c", ".") -> ("is_c", ".", left)$
Tape: [".",".","a",<"c">,".","."] ("is_c", "c") -> ("go_to_beginning", ".", left)$
Tape: [".",".",<"a">,".",".","."] ("go_to_beginning", "a") -> ("go_to_beginning", "a", left)$
Tape: [".",<".">,"a",".",".","."] ("go_to_beginning", ".") -> ("pick_character", ".", right)$
Tape: [".",".",<"a">,".",".","."] ("pick_character", "a") -> ("go_to_end_and_find_a", ".", right)$
Tape: [".",".",".",<".">,".","."] ("go_to_end_and_find_a", ".") -> ("is_a", ".", left)$
Tape: [".",".",<".">,".",".","."] ("is_a", ".") -> ("write_is_palindrome", ".", right)$
Tape: [".",".",".",<".">,".","."] ("write_is_palindrome", ".") -> ("HALT", "y", right)$
Tape: [".",".",".","y",<".">,"."] Final state reached !$
Interpreter closing...$'
}

@test "'c' is palindrome" {
    run bash -c './_build/default/bin/turing our-machines/palindrome.json "c" | cat -e'
    assert_output 'Interpreter starting...$
Tape: [<"c">] ("pick_character", "c") -> ("go_to_end_and_find_c", ".", right)$
Tape: [".",<".">] ("go_to_end_and_find_c", ".") -> ("is_c", ".", left)$
Tape: [<".">,"."] ("is_c", ".") -> ("write_is_palindrome", ".", right)$
Tape: [".",<".">] ("write_is_palindrome", ".") -> ("HALT", "y", right)$
Tape: [".","y",<".">] Final state reached !$
Interpreter closing...$'
}

@test "'cc' is palindrome" {
    run bash -c './_build/default/bin/turing our-machines/palindrome.json "cc" | cat -e'
    assert_output 'Interpreter starting...$
Tape: [<"c">,"c"] ("pick_character", "c") -> ("go_to_end_and_find_c", ".", right)$
Tape: [".",<"c">] ("go_to_end_and_find_c", "c") -> ("go_to_end_and_find_c", "c", right)$
Tape: [".","c",<".">] ("go_to_end_and_find_c", ".") -> ("is_c", ".", left)$
Tape: [".",<"c">,"."] ("is_c", "c") -> ("go_to_beginning", ".", left)$
Tape: [<".">,".","."] ("go_to_beginning", ".") -> ("pick_character", ".", right)$
Tape: [".",<".">,"."] ("pick_character", ".") -> ("write_is_palindrome", ".", right)$
Tape: [".",".",<".">] ("write_is_palindrome", ".") -> ("HALT", "y", right)$
Tape: [".",".","y",<".">] Final state reached !$
Interpreter closing...$'
}

@test "'cac' is palindrome" {
    run bash -c './_build/default/bin/turing our-machines/palindrome.json "cac" | cat -e'
    assert_output 'Interpreter starting...$
Tape: [<"c">,"a","c"] ("pick_character", "c") -> ("go_to_end_and_find_c", ".", right)$
Tape: [".",<"a">,"c"] ("go_to_end_and_find_c", "a") -> ("go_to_end_and_find_c", "a", right)$
Tape: [".","a",<"c">] ("go_to_end_and_find_c", "c") -> ("go_to_end_and_find_c", "c", right)$
Tape: [".","a","c",<".">] ("go_to_end_and_find_c", ".") -> ("is_c", ".", left)$
Tape: [".","a",<"c">,"."] ("is_c", "c") -> ("go_to_beginning", ".", left)$
Tape: [".",<"a">,".","."] ("go_to_beginning", "a") -> ("go_to_beginning", "a", left)$
Tape: [<".">,"a",".","."] ("go_to_beginning", ".") -> ("pick_character", ".", right)$
Tape: [".",<"a">,".","."] ("pick_character", "a") -> ("go_to_end_and_find_a", ".", right)$
Tape: [".",".",<".">,"."] ("go_to_end_and_find_a", ".") -> ("is_a", ".", left)$
Tape: [".",<".">,".","."] ("is_a", ".") -> ("write_is_palindrome", ".", right)$
Tape: [".",".",<".">,"."] ("write_is_palindrome", ".") -> ("HALT", "y", right)$
Tape: [".",".","y",<".">] Final state reached !$
Interpreter closing...$'
}

@test "'caac' is palindrome" {
    run bash -c './_build/default/bin/turing our-machines/palindrome.json "caac" | cat -e'
    assert_output 'Interpreter starting...$
Tape: [<"c">,"a","a","c"] ("pick_character", "c") -> ("go_to_end_and_find_c", ".", right)$
Tape: [".",<"a">,"a","c"] ("go_to_end_and_find_c", "a") -> ("go_to_end_and_find_c", "a", right)$
Tape: [".","a",<"a">,"c"] ("go_to_end_and_find_c", "a") -> ("go_to_end_and_find_c", "a", right)$
Tape: [".","a","a",<"c">] ("go_to_end_and_find_c", "c") -> ("go_to_end_and_find_c", "c", right)$
Tape: [".","a","a","c",<".">] ("go_to_end_and_find_c", ".") -> ("is_c", ".", left)$
Tape: [".","a","a",<"c">,"."] ("is_c", "c") -> ("go_to_beginning", ".", left)$
Tape: [".","a",<"a">,".","."] ("go_to_beginning", "a") -> ("go_to_beginning", "a", left)$
Tape: [".",<"a">,"a",".","."] ("go_to_beginning", "a") -> ("go_to_beginning", "a", left)$
Tape: [<".">,"a","a",".","."] ("go_to_beginning", ".") -> ("pick_character", ".", right)$
Tape: [".",<"a">,"a",".","."] ("pick_character", "a") -> ("go_to_end_and_find_a", ".", right)$
Tape: [".",".",<"a">,".","."] ("go_to_end_and_find_a", "a") -> ("go_to_end_and_find_a", "a", right)$
Tape: [".",".","a",<".">,"."] ("go_to_end_and_find_a", ".") -> ("is_a", ".", left)$
Tape: [".",".",<"a">,".","."] ("is_a", "a") -> ("go_to_beginning", ".", left)$
Tape: [".",<".">,".",".","."] ("go_to_beginning", ".") -> ("pick_character", ".", right)$
Tape: [".",".",<".">,".","."] ("pick_character", ".") -> ("write_is_palindrome", ".", right)$
Tape: [".",".",".",<".">,"."] ("write_is_palindrome", ".") -> ("HALT", "y", right)$
Tape: [".",".",".","y",<".">] Final state reached !$
Interpreter closing...$'
}

@test "'cbc' is palindrome" {
    run bash -c './_build/default/bin/turing our-machines/palindrome.json "cbc" | cat -e'
    assert_output 'Interpreter starting...$
Tape: [<"c">,"b","c"] ("pick_character", "c") -> ("go_to_end_and_find_c", ".", right)$
Tape: [".",<"b">,"c"] ("go_to_end_and_find_c", "b") -> ("go_to_end_and_find_c", "b", right)$
Tape: [".","b",<"c">] ("go_to_end_and_find_c", "c") -> ("go_to_end_and_find_c", "c", right)$
Tape: [".","b","c",<".">] ("go_to_end_and_find_c", ".") -> ("is_c", ".", left)$
Tape: [".","b",<"c">,"."] ("is_c", "c") -> ("go_to_beginning", ".", left)$
Tape: [".",<"b">,".","."] ("go_to_beginning", "b") -> ("go_to_beginning", "b", left)$
Tape: [<".">,"b",".","."] ("go_to_beginning", ".") -> ("pick_character", ".", right)$
Tape: [".",<"b">,".","."] ("pick_character", "b") -> ("go_to_end_and_find_b", ".", right)$
Tape: [".",".",<".">,"."] ("go_to_end_and_find_b", ".") -> ("is_b", ".", left)$
Tape: [".",<".">,".","."] ("is_b", ".") -> ("write_is_palindrome", ".", right)$
Tape: [".",".",<".">,"."] ("write_is_palindrome", ".") -> ("HALT", "y", right)$
Tape: [".",".","y",<".">] Final state reached !$
Interpreter closing...$'
}

@test "'cbbc' is palindrome" {
    run bash -c './_build/default/bin/turing our-machines/palindrome.json "cbbc" | cat -e'
    assert_output 'Interpreter starting...$
Tape: [<"c">,"b","b","c"] ("pick_character", "c") -> ("go_to_end_and_find_c", ".", right)$
Tape: [".",<"b">,"b","c"] ("go_to_end_and_find_c", "b") -> ("go_to_end_and_find_c", "b", right)$
Tape: [".","b",<"b">,"c"] ("go_to_end_and_find_c", "b") -> ("go_to_end_and_find_c", "b", right)$
Tape: [".","b","b",<"c">] ("go_to_end_and_find_c", "c") -> ("go_to_end_and_find_c", "c", right)$
Tape: [".","b","b","c",<".">] ("go_to_end_and_find_c", ".") -> ("is_c", ".", left)$
Tape: [".","b","b",<"c">,"."] ("is_c", "c") -> ("go_to_beginning", ".", left)$
Tape: [".","b",<"b">,".","."] ("go_to_beginning", "b") -> ("go_to_beginning", "b", left)$
Tape: [".",<"b">,"b",".","."] ("go_to_beginning", "b") -> ("go_to_beginning", "b", left)$
Tape: [<".">,"b","b",".","."] ("go_to_beginning", ".") -> ("pick_character", ".", right)$
Tape: [".",<"b">,"b",".","."] ("pick_character", "b") -> ("go_to_end_and_find_b", ".", right)$
Tape: [".",".",<"b">,".","."] ("go_to_end_and_find_b", "b") -> ("go_to_end_and_find_b", "b", right)$
Tape: [".",".","b",<".">,"."] ("go_to_end_and_find_b", ".") -> ("is_b", ".", left)$
Tape: [".",".",<"b">,".","."] ("is_b", "b") -> ("go_to_beginning", ".", left)$
Tape: [".",<".">,".",".","."] ("go_to_beginning", ".") -> ("pick_character", ".", right)$
Tape: [".",".",<".">,".","."] ("pick_character", ".") -> ("write_is_palindrome", ".", right)$
Tape: [".",".",".",<".">,"."] ("write_is_palindrome", ".") -> ("HALT", "y", right)$
Tape: [".",".",".","y",<".">] Final state reached !$
Interpreter closing...$'
}

@test "'cabac' is palindrome" {
    run bash -c './_build/default/bin/turing our-machines/palindrome.json "cabac" | cat -e'
    assert_output 'Interpreter starting...$
Tape: [<"c">,"a","b","a","c"] ("pick_character", "c") -> ("go_to_end_and_find_c", ".", right)$
Tape: [".",<"a">,"b","a","c"] ("go_to_end_and_find_c", "a") -> ("go_to_end_and_find_c", "a", right)$
Tape: [".","a",<"b">,"a","c"] ("go_to_end_and_find_c", "b") -> ("go_to_end_and_find_c", "b", right)$
Tape: [".","a","b",<"a">,"c"] ("go_to_end_and_find_c", "a") -> ("go_to_end_and_find_c", "a", right)$
Tape: [".","a","b","a",<"c">] ("go_to_end_and_find_c", "c") -> ("go_to_end_and_find_c", "c", right)$
Tape: [".","a","b","a","c",<".">] ("go_to_end_and_find_c", ".") -> ("is_c", ".", left)$
Tape: [".","a","b","a",<"c">,"."] ("is_c", "c") -> ("go_to_beginning", ".", left)$
Tape: [".","a","b",<"a">,".","."] ("go_to_beginning", "a") -> ("go_to_beginning", "a", left)$
Tape: [".","a",<"b">,"a",".","."] ("go_to_beginning", "b") -> ("go_to_beginning", "b", left)$
Tape: [".",<"a">,"b","a",".","."] ("go_to_beginning", "a") -> ("go_to_beginning", "a", left)$
Tape: [<".">,"a","b","a",".","."] ("go_to_beginning", ".") -> ("pick_character", ".", right)$
Tape: [".",<"a">,"b","a",".","."] ("pick_character", "a") -> ("go_to_end_and_find_a", ".", right)$
Tape: [".",".",<"b">,"a",".","."] ("go_to_end_and_find_a", "b") -> ("go_to_end_and_find_a", "b", right)$
Tape: [".",".","b",<"a">,".","."] ("go_to_end_and_find_a", "a") -> ("go_to_end_and_find_a", "a", right)$
Tape: [".",".","b","a",<".">,"."] ("go_to_end_and_find_a", ".") -> ("is_a", ".", left)$
Tape: [".",".","b",<"a">,".","."] ("is_a", "a") -> ("go_to_beginning", ".", left)$
Tape: [".",".",<"b">,".",".","."] ("go_to_beginning", "b") -> ("go_to_beginning", "b", left)$
Tape: [".",<".">,"b",".",".","."] ("go_to_beginning", ".") -> ("pick_character", ".", right)$
Tape: [".",".",<"b">,".",".","."] ("pick_character", "b") -> ("go_to_end_and_find_b", ".", right)$
Tape: [".",".",".",<".">,".","."] ("go_to_end_and_find_b", ".") -> ("is_b", ".", left)$
Tape: [".",".",<".">,".",".","."] ("is_b", ".") -> ("write_is_palindrome", ".", right)$
Tape: [".",".",".",<".">,".","."] ("write_is_palindrome", ".") -> ("HALT", "y", right)$
Tape: [".",".",".","y",<".">,"."] Final state reached !$
Interpreter closing...$'
}

@test "'ab' is not palindrome" {
    run bash -c './_build/default/bin/turing our-machines/palindrome.json "ab" | cat -e'
    assert_output 'Interpreter starting...$
Tape: [<"a">,"b"] ("pick_character", "a") -> ("go_to_end_and_find_a", ".", right)$
Tape: [".",<"b">] ("go_to_end_and_find_a", "b") -> ("go_to_end_and_find_a", "b", right)$
Tape: [".","b",<".">] ("go_to_end_and_find_a", ".") -> ("is_a", ".", left)$
Tape: [".",<"b">,"."] ("is_a", "b") -> ("write_is_not_palindrome", "b", right)$
Tape: [".","b",<".">] ("write_is_not_palindrome", ".") -> ("HALT", "n", right)$
Tape: [".","b","n",<".">] Final state reached !$
Interpreter closing...$'
}

@test "'ac' is not palindrome" {
    run bash -c './_build/default/bin/turing our-machines/palindrome.json "ac" | cat -e'
    assert_output 'Interpreter starting...$
Tape: [<"a">,"c"] ("pick_character", "a") -> ("go_to_end_and_find_a", ".", right)$
Tape: [".",<"c">] ("go_to_end_and_find_a", "c") -> ("go_to_end_and_find_a", "c", right)$
Tape: [".","c",<".">] ("go_to_end_and_find_a", ".") -> ("is_a", ".", left)$
Tape: [".",<"c">,"."] ("is_a", "c") -> ("write_is_not_palindrome", "c", right)$
Tape: [".","c",<".">] ("write_is_not_palindrome", ".") -> ("HALT", "n", right)$
Tape: [".","c","n",<".">] Final state reached !$
Interpreter closing...$'
}

@test "'ba' is not palindrome" {
    run bash -c './_build/default/bin/turing our-machines/palindrome.json "ba" | cat -e'
    assert_output 'Interpreter starting...$
Tape: [<"b">,"a"] ("pick_character", "b") -> ("go_to_end_and_find_b", ".", right)$
Tape: [".",<"a">] ("go_to_end_and_find_b", "a") -> ("go_to_end_and_find_b", "a", right)$
Tape: [".","a",<".">] ("go_to_end_and_find_b", ".") -> ("is_b", ".", left)$
Tape: [".",<"a">,"."] ("is_b", "a") -> ("write_is_not_palindrome", "a", right)$
Tape: [".","a",<".">] ("write_is_not_palindrome", ".") -> ("HALT", "n", right)$
Tape: [".","a","n",<".">] Final state reached !$
Interpreter closing...$'
}

@test "'bc' is not palindrome" {
    run bash -c './_build/default/bin/turing our-machines/palindrome.json "bc" | cat -e'
    assert_output 'Interpreter starting...$
Tape: [<"b">,"c"] ("pick_character", "b") -> ("go_to_end_and_find_b", ".", right)$
Tape: [".",<"c">] ("go_to_end_and_find_b", "c") -> ("go_to_end_and_find_b", "c", right)$
Tape: [".","c",<".">] ("go_to_end_and_find_b", ".") -> ("is_b", ".", left)$
Tape: [".",<"c">,"."] ("is_b", "c") -> ("write_is_not_palindrome", "c", right)$
Tape: [".","c",<".">] ("write_is_not_palindrome", ".") -> ("HALT", "n", right)$
Tape: [".","c","n",<".">] Final state reached !$
Interpreter closing...$'
}

@test "'ca' is not palindrome" {
    run bash -c './_build/default/bin/turing our-machines/palindrome.json "ca" | cat -e'
    assert_output 'Interpreter starting...$
Tape: [<"c">,"a"] ("pick_character", "c") -> ("go_to_end_and_find_c", ".", right)$
Tape: [".",<"a">] ("go_to_end_and_find_c", "a") -> ("go_to_end_and_find_c", "a", right)$
Tape: [".","a",<".">] ("go_to_end_and_find_c", ".") -> ("is_c", ".", left)$
Tape: [".",<"a">,"."] ("is_c", "a") -> ("write_is_not_palindrome", "a", right)$
Tape: [".","a",<".">] ("write_is_not_palindrome", ".") -> ("HALT", "n", right)$
Tape: [".","a","n",<".">] Final state reached !$
Interpreter closing...$'
}

@test "'cb' is not palindrome" {
    run bash -c './_build/default/bin/turing our-machines/palindrome.json "cb" | cat -e'
    assert_output 'Interpreter starting...$
Tape: [<"c">,"b"] ("pick_character", "c") -> ("go_to_end_and_find_c", ".", right)$
Tape: [".",<"b">] ("go_to_end_and_find_c", "b") -> ("go_to_end_and_find_c", "b", right)$
Tape: [".","b",<".">] ("go_to_end_and_find_c", ".") -> ("is_c", ".", left)$
Tape: [".",<"b">,"."] ("is_c", "b") -> ("write_is_not_palindrome", "b", right)$
Tape: [".","b",<".">] ("write_is_not_palindrome", ".") -> ("HALT", "n", right)$
Tape: [".","b","n",<".">] Final state reached !$
Interpreter closing...$'
}

@test "'acab' is not palindrome" {
    run bash -c './_build/default/bin/turing our-machines/palindrome.json "acab" | cat -e'
    assert_output 'Interpreter starting...$
Tape: [<"a">,"c","a","b"] ("pick_character", "a") -> ("go_to_end_and_find_a", ".", right)$
Tape: [".",<"c">,"a","b"] ("go_to_end_and_find_a", "c") -> ("go_to_end_and_find_a", "c", right)$
Tape: [".","c",<"a">,"b"] ("go_to_end_and_find_a", "a") -> ("go_to_end_and_find_a", "a", right)$
Tape: [".","c","a",<"b">] ("go_to_end_and_find_a", "b") -> ("go_to_end_and_find_a", "b", right)$
Tape: [".","c","a","b",<".">] ("go_to_end_and_find_a", ".") -> ("is_a", ".", left)$
Tape: [".","c","a",<"b">,"."] ("is_a", "b") -> ("write_is_not_palindrome", "b", right)$
Tape: [".","c","a","b",<".">] ("write_is_not_palindrome", ".") -> ("HALT", "n", right)$
Tape: [".","c","a","b","n",<".">] Final state reached !$
Interpreter closing...$'
}
