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
[<.>] (pick_character, .) -> (write_is_palindrome, ., right)$
[.<.>] (write_is_palindrome, .) -> (HALT, y, right)$
[.y<.>] Final state reached !$
Interpreter closing...$'
}

@test "'a' is palindrome" {
    run bash -c './_build/default/bin/turing our-machines/palindrome.json "a" | cat -e'
    assert_output 'Interpreter starting...$
[<a>] (pick_character, a) -> (go_to_end_and_find_a, ., right)$
[.<.>] (go_to_end_and_find_a, .) -> (is_a, ., left)$
[<.>.] (is_a, .) -> (write_is_palindrome, ., right)$
[.<.>] (write_is_palindrome, .) -> (HALT, y, right)$
[.y<.>] Final state reached !$
Interpreter closing...$'
}

@test "'aa' is palindrome" {
    run bash -c './_build/default/bin/turing our-machines/palindrome.json "aa" | cat -e'
    assert_output 'Interpreter starting...$
[<a>a] (pick_character, a) -> (go_to_end_and_find_a, ., right)$
[.<a>] (go_to_end_and_find_a, a) -> (go_to_end_and_find_a, a, right)$
[.a<.>] (go_to_end_and_find_a, .) -> (is_a, ., left)$
[.<a>.] (is_a, a) -> (go_to_beginning, ., left)$
[<.>..] (go_to_beginning, .) -> (pick_character, ., right)$
[.<.>.] (pick_character, .) -> (write_is_palindrome, ., right)$
[..<.>] (write_is_palindrome, .) -> (HALT, y, right)$
[..y<.>] Final state reached !$
Interpreter closing...$'
}

@test "'aba' is palindrome" {
    run bash -c './_build/default/bin/turing our-machines/palindrome.json "aba" | cat -e'
    assert_output 'Interpreter starting...$
[<a>ba] (pick_character, a) -> (go_to_end_and_find_a, ., right)$
[.<b>a] (go_to_end_and_find_a, b) -> (go_to_end_and_find_a, b, right)$
[.b<a>] (go_to_end_and_find_a, a) -> (go_to_end_and_find_a, a, right)$
[.ba<.>] (go_to_end_and_find_a, .) -> (is_a, ., left)$
[.b<a>.] (is_a, a) -> (go_to_beginning, ., left)$
[.<b>..] (go_to_beginning, b) -> (go_to_beginning, b, left)$
[<.>b..] (go_to_beginning, .) -> (pick_character, ., right)$
[.<b>..] (pick_character, b) -> (go_to_end_and_find_b, ., right)$
[..<.>.] (go_to_end_and_find_b, .) -> (is_b, ., left)$
[.<.>..] (is_b, .) -> (write_is_palindrome, ., right)$
[..<.>.] (write_is_palindrome, .) -> (HALT, y, right)$
[..y<.>] Final state reached !$
Interpreter closing...$'
}

@test "'abba' is palindrome" {
    run bash -c './_build/default/bin/turing our-machines/palindrome.json "abba" | cat -e'
    assert_output 'Interpreter starting...$
[<a>bba] (pick_character, a) -> (go_to_end_and_find_a, ., right)$
[.<b>ba] (go_to_end_and_find_a, b) -> (go_to_end_and_find_a, b, right)$
[.b<b>a] (go_to_end_and_find_a, b) -> (go_to_end_and_find_a, b, right)$
[.bb<a>] (go_to_end_and_find_a, a) -> (go_to_end_and_find_a, a, right)$
[.bba<.>] (go_to_end_and_find_a, .) -> (is_a, ., left)$
[.bb<a>.] (is_a, a) -> (go_to_beginning, ., left)$
[.b<b>..] (go_to_beginning, b) -> (go_to_beginning, b, left)$
[.<b>b..] (go_to_beginning, b) -> (go_to_beginning, b, left)$
[<.>bb..] (go_to_beginning, .) -> (pick_character, ., right)$
[.<b>b..] (pick_character, b) -> (go_to_end_and_find_b, ., right)$
[..<b>..] (go_to_end_and_find_b, b) -> (go_to_end_and_find_b, b, right)$
[..b<.>.] (go_to_end_and_find_b, .) -> (is_b, ., left)$
[..<b>..] (is_b, b) -> (go_to_beginning, ., left)$
[.<.>...] (go_to_beginning, .) -> (pick_character, ., right)$
[..<.>..] (pick_character, .) -> (write_is_palindrome, ., right)$
[...<.>.] (write_is_palindrome, .) -> (HALT, y, right)$
[...y<.>] Final state reached !$
Interpreter closing...$'
}

@test "'aca' is palindrome" {
    run bash -c './_build/default/bin/turing our-machines/palindrome.json "aca" | cat -e'
    assert_output 'Interpreter starting...$
[<a>ca] (pick_character, a) -> (go_to_end_and_find_a, ., right)$
[.<c>a] (go_to_end_and_find_a, c) -> (go_to_end_and_find_a, c, right)$
[.c<a>] (go_to_end_and_find_a, a) -> (go_to_end_and_find_a, a, right)$
[.ca<.>] (go_to_end_and_find_a, .) -> (is_a, ., left)$
[.c<a>.] (is_a, a) -> (go_to_beginning, ., left)$
[.<c>..] (go_to_beginning, c) -> (go_to_beginning, c, left)$
[<.>c..] (go_to_beginning, .) -> (pick_character, ., right)$
[.<c>..] (pick_character, c) -> (go_to_end_and_find_c, ., right)$
[..<.>.] (go_to_end_and_find_c, .) -> (is_c, ., left)$
[.<.>..] (is_c, .) -> (write_is_palindrome, ., right)$
[..<.>.] (write_is_palindrome, .) -> (HALT, y, right)$
[..y<.>] Final state reached !$
Interpreter closing...$'
}

@test "'acca' is palindrome" {
    run bash -c './_build/default/bin/turing our-machines/palindrome.json "acca" | cat -e'
    assert_output 'Interpreter starting...$
[<a>cca] (pick_character, a) -> (go_to_end_and_find_a, ., right)$
[.<c>ca] (go_to_end_and_find_a, c) -> (go_to_end_and_find_a, c, right)$
[.c<c>a] (go_to_end_and_find_a, c) -> (go_to_end_and_find_a, c, right)$
[.cc<a>] (go_to_end_and_find_a, a) -> (go_to_end_and_find_a, a, right)$
[.cca<.>] (go_to_end_and_find_a, .) -> (is_a, ., left)$
[.cc<a>.] (is_a, a) -> (go_to_beginning, ., left)$
[.c<c>..] (go_to_beginning, c) -> (go_to_beginning, c, left)$
[.<c>c..] (go_to_beginning, c) -> (go_to_beginning, c, left)$
[<.>cc..] (go_to_beginning, .) -> (pick_character, ., right)$
[.<c>c..] (pick_character, c) -> (go_to_end_and_find_c, ., right)$
[..<c>..] (go_to_end_and_find_c, c) -> (go_to_end_and_find_c, c, right)$
[..c<.>.] (go_to_end_and_find_c, .) -> (is_c, ., left)$
[..<c>..] (is_c, c) -> (go_to_beginning, ., left)$
[.<.>...] (go_to_beginning, .) -> (pick_character, ., right)$
[..<.>..] (pick_character, .) -> (write_is_palindrome, ., right)$
[...<.>.] (write_is_palindrome, .) -> (HALT, y, right)$
[...y<.>] Final state reached !$
Interpreter closing...$'
}

@test "'abcba' is palindrome" {
    run bash -c './_build/default/bin/turing our-machines/palindrome.json "abcba" | cat -e'
    assert_output 'Interpreter starting...$
[<a>bcba] (pick_character, a) -> (go_to_end_and_find_a, ., right)$
[.<b>cba] (go_to_end_and_find_a, b) -> (go_to_end_and_find_a, b, right)$
[.b<c>ba] (go_to_end_and_find_a, c) -> (go_to_end_and_find_a, c, right)$
[.bc<b>a] (go_to_end_and_find_a, b) -> (go_to_end_and_find_a, b, right)$
[.bcb<a>] (go_to_end_and_find_a, a) -> (go_to_end_and_find_a, a, right)$
[.bcba<.>] (go_to_end_and_find_a, .) -> (is_a, ., left)$
[.bcb<a>.] (is_a, a) -> (go_to_beginning, ., left)$
[.bc<b>..] (go_to_beginning, b) -> (go_to_beginning, b, left)$
[.b<c>b..] (go_to_beginning, c) -> (go_to_beginning, c, left)$
[.<b>cb..] (go_to_beginning, b) -> (go_to_beginning, b, left)$
[<.>bcb..] (go_to_beginning, .) -> (pick_character, ., right)$
[.<b>cb..] (pick_character, b) -> (go_to_end_and_find_b, ., right)$
[..<c>b..] (go_to_end_and_find_b, c) -> (go_to_end_and_find_b, c, right)$
[..c<b>..] (go_to_end_and_find_b, b) -> (go_to_end_and_find_b, b, right)$
[..cb<.>.] (go_to_end_and_find_b, .) -> (is_b, ., left)$
[..c<b>..] (is_b, b) -> (go_to_beginning, ., left)$
[..<c>...] (go_to_beginning, c) -> (go_to_beginning, c, left)$
[.<.>c...] (go_to_beginning, .) -> (pick_character, ., right)$
[..<c>...] (pick_character, c) -> (go_to_end_and_find_c, ., right)$
[...<.>..] (go_to_end_and_find_c, .) -> (is_c, ., left)$
[..<.>...] (is_c, .) -> (write_is_palindrome, ., right)$
[...<.>..] (write_is_palindrome, .) -> (HALT, y, right)$
[...y<.>.] Final state reached !$
Interpreter closing...$'
}

@test "'b' is palindrome" {
    run bash -c './_build/default/bin/turing our-machines/palindrome.json "b" | cat -e'
    assert_output 'Interpreter starting...$
[<b>] (pick_character, b) -> (go_to_end_and_find_b, ., right)$
[.<.>] (go_to_end_and_find_b, .) -> (is_b, ., left)$
[<.>.] (is_b, .) -> (write_is_palindrome, ., right)$
[.<.>] (write_is_palindrome, .) -> (HALT, y, right)$
[.y<.>] Final state reached !$
Interpreter closing...$'
}

@test "'bb' is palindrome" {
    run bash -c './_build/default/bin/turing our-machines/palindrome.json "bb" | cat -e'
    assert_output 'Interpreter starting...$
[<b>b] (pick_character, b) -> (go_to_end_and_find_b, ., right)$
[.<b>] (go_to_end_and_find_b, b) -> (go_to_end_and_find_b, b, right)$
[.b<.>] (go_to_end_and_find_b, .) -> (is_b, ., left)$
[.<b>.] (is_b, b) -> (go_to_beginning, ., left)$
[<.>..] (go_to_beginning, .) -> (pick_character, ., right)$
[.<.>.] (pick_character, .) -> (write_is_palindrome, ., right)$
[..<.>] (write_is_palindrome, .) -> (HALT, y, right)$
[..y<.>] Final state reached !$
Interpreter closing...$'
}

@test "'bab' is palindrome" {
    run bash -c './_build/default/bin/turing our-machines/palindrome.json "bab" | cat -e'
    assert_output 'Interpreter starting...$
[<b>ab] (pick_character, b) -> (go_to_end_and_find_b, ., right)$
[.<a>b] (go_to_end_and_find_b, a) -> (go_to_end_and_find_b, a, right)$
[.a<b>] (go_to_end_and_find_b, b) -> (go_to_end_and_find_b, b, right)$
[.ab<.>] (go_to_end_and_find_b, .) -> (is_b, ., left)$
[.a<b>.] (is_b, b) -> (go_to_beginning, ., left)$
[.<a>..] (go_to_beginning, a) -> (go_to_beginning, a, left)$
[<.>a..] (go_to_beginning, .) -> (pick_character, ., right)$
[.<a>..] (pick_character, a) -> (go_to_end_and_find_a, ., right)$
[..<.>.] (go_to_end_and_find_a, .) -> (is_a, ., left)$
[.<.>..] (is_a, .) -> (write_is_palindrome, ., right)$
[..<.>.] (write_is_palindrome, .) -> (HALT, y, right)$
[..y<.>] Final state reached !$
Interpreter closing...$'
}

@test "'baab' is palindrome" {
    run bash -c './_build/default/bin/turing our-machines/palindrome.json "baab" | cat -e'
    assert_output 'Interpreter starting...$
[<b>aab] (pick_character, b) -> (go_to_end_and_find_b, ., right)$
[.<a>ab] (go_to_end_and_find_b, a) -> (go_to_end_and_find_b, a, right)$
[.a<a>b] (go_to_end_and_find_b, a) -> (go_to_end_and_find_b, a, right)$
[.aa<b>] (go_to_end_and_find_b, b) -> (go_to_end_and_find_b, b, right)$
[.aab<.>] (go_to_end_and_find_b, .) -> (is_b, ., left)$
[.aa<b>.] (is_b, b) -> (go_to_beginning, ., left)$
[.a<a>..] (go_to_beginning, a) -> (go_to_beginning, a, left)$
[.<a>a..] (go_to_beginning, a) -> (go_to_beginning, a, left)$
[<.>aa..] (go_to_beginning, .) -> (pick_character, ., right)$
[.<a>a..] (pick_character, a) -> (go_to_end_and_find_a, ., right)$
[..<a>..] (go_to_end_and_find_a, a) -> (go_to_end_and_find_a, a, right)$
[..a<.>.] (go_to_end_and_find_a, .) -> (is_a, ., left)$
[..<a>..] (is_a, a) -> (go_to_beginning, ., left)$
[.<.>...] (go_to_beginning, .) -> (pick_character, ., right)$
[..<.>..] (pick_character, .) -> (write_is_palindrome, ., right)$
[...<.>.] (write_is_palindrome, .) -> (HALT, y, right)$
[...y<.>] Final state reached !$
Interpreter closing...$'
}

@test "'bcb' is palindrome" {
    run bash -c './_build/default/bin/turing our-machines/palindrome.json "bcb" | cat -e'
    assert_output 'Interpreter starting...$
[<b>cb] (pick_character, b) -> (go_to_end_and_find_b, ., right)$
[.<c>b] (go_to_end_and_find_b, c) -> (go_to_end_and_find_b, c, right)$
[.c<b>] (go_to_end_and_find_b, b) -> (go_to_end_and_find_b, b, right)$
[.cb<.>] (go_to_end_and_find_b, .) -> (is_b, ., left)$
[.c<b>.] (is_b, b) -> (go_to_beginning, ., left)$
[.<c>..] (go_to_beginning, c) -> (go_to_beginning, c, left)$
[<.>c..] (go_to_beginning, .) -> (pick_character, ., right)$
[.<c>..] (pick_character, c) -> (go_to_end_and_find_c, ., right)$
[..<.>.] (go_to_end_and_find_c, .) -> (is_c, ., left)$
[.<.>..] (is_c, .) -> (write_is_palindrome, ., right)$
[..<.>.] (write_is_palindrome, .) -> (HALT, y, right)$
[..y<.>] Final state reached !$
Interpreter closing...$'
}

@test "'bccb' is palindrome" {
    run bash -c './_build/default/bin/turing our-machines/palindrome.json "bccb" | cat -e'
    assert_output 'Interpreter starting...$
[<b>ccb] (pick_character, b) -> (go_to_end_and_find_b, ., right)$
[.<c>cb] (go_to_end_and_find_b, c) -> (go_to_end_and_find_b, c, right)$
[.c<c>b] (go_to_end_and_find_b, c) -> (go_to_end_and_find_b, c, right)$
[.cc<b>] (go_to_end_and_find_b, b) -> (go_to_end_and_find_b, b, right)$
[.ccb<.>] (go_to_end_and_find_b, .) -> (is_b, ., left)$
[.cc<b>.] (is_b, b) -> (go_to_beginning, ., left)$
[.c<c>..] (go_to_beginning, c) -> (go_to_beginning, c, left)$
[.<c>c..] (go_to_beginning, c) -> (go_to_beginning, c, left)$
[<.>cc..] (go_to_beginning, .) -> (pick_character, ., right)$
[.<c>c..] (pick_character, c) -> (go_to_end_and_find_c, ., right)$
[..<c>..] (go_to_end_and_find_c, c) -> (go_to_end_and_find_c, c, right)$
[..c<.>.] (go_to_end_and_find_c, .) -> (is_c, ., left)$
[..<c>..] (is_c, c) -> (go_to_beginning, ., left)$
[.<.>...] (go_to_beginning, .) -> (pick_character, ., right)$
[..<.>..] (pick_character, .) -> (write_is_palindrome, ., right)$
[...<.>.] (write_is_palindrome, .) -> (HALT, y, right)$
[...y<.>] Final state reached !$
Interpreter closing...$'
}

@test "'bcacb' is palindrome" {
    run bash -c './_build/default/bin/turing our-machines/palindrome.json "bcacb" | cat -e'
    assert_output 'Interpreter starting...$
[<b>cacb] (pick_character, b) -> (go_to_end_and_find_b, ., right)$
[.<c>acb] (go_to_end_and_find_b, c) -> (go_to_end_and_find_b, c, right)$
[.c<a>cb] (go_to_end_and_find_b, a) -> (go_to_end_and_find_b, a, right)$
[.ca<c>b] (go_to_end_and_find_b, c) -> (go_to_end_and_find_b, c, right)$
[.cac<b>] (go_to_end_and_find_b, b) -> (go_to_end_and_find_b, b, right)$
[.cacb<.>] (go_to_end_and_find_b, .) -> (is_b, ., left)$
[.cac<b>.] (is_b, b) -> (go_to_beginning, ., left)$
[.ca<c>..] (go_to_beginning, c) -> (go_to_beginning, c, left)$
[.c<a>c..] (go_to_beginning, a) -> (go_to_beginning, a, left)$
[.<c>ac..] (go_to_beginning, c) -> (go_to_beginning, c, left)$
[<.>cac..] (go_to_beginning, .) -> (pick_character, ., right)$
[.<c>ac..] (pick_character, c) -> (go_to_end_and_find_c, ., right)$
[..<a>c..] (go_to_end_and_find_c, a) -> (go_to_end_and_find_c, a, right)$
[..a<c>..] (go_to_end_and_find_c, c) -> (go_to_end_and_find_c, c, right)$
[..ac<.>.] (go_to_end_and_find_c, .) -> (is_c, ., left)$
[..a<c>..] (is_c, c) -> (go_to_beginning, ., left)$
[..<a>...] (go_to_beginning, a) -> (go_to_beginning, a, left)$
[.<.>a...] (go_to_beginning, .) -> (pick_character, ., right)$
[..<a>...] (pick_character, a) -> (go_to_end_and_find_a, ., right)$
[...<.>..] (go_to_end_and_find_a, .) -> (is_a, ., left)$
[..<.>...] (is_a, .) -> (write_is_palindrome, ., right)$
[...<.>..] (write_is_palindrome, .) -> (HALT, y, right)$
[...y<.>.] Final state reached !$
Interpreter closing...$'
}

@test "'c' is palindrome" {
    run bash -c './_build/default/bin/turing our-machines/palindrome.json "c" | cat -e'
    assert_output 'Interpreter starting...$
[<c>] (pick_character, c) -> (go_to_end_and_find_c, ., right)$
[.<.>] (go_to_end_and_find_c, .) -> (is_c, ., left)$
[<.>.] (is_c, .) -> (write_is_palindrome, ., right)$
[.<.>] (write_is_palindrome, .) -> (HALT, y, right)$
[.y<.>] Final state reached !$
Interpreter closing...$'
}

@test "'cc' is palindrome" {
    run bash -c './_build/default/bin/turing our-machines/palindrome.json "cc" | cat -e'
    assert_output 'Interpreter starting...$
[<c>c] (pick_character, c) -> (go_to_end_and_find_c, ., right)$
[.<c>] (go_to_end_and_find_c, c) -> (go_to_end_and_find_c, c, right)$
[.c<.>] (go_to_end_and_find_c, .) -> (is_c, ., left)$
[.<c>.] (is_c, c) -> (go_to_beginning, ., left)$
[<.>..] (go_to_beginning, .) -> (pick_character, ., right)$
[.<.>.] (pick_character, .) -> (write_is_palindrome, ., right)$
[..<.>] (write_is_palindrome, .) -> (HALT, y, right)$
[..y<.>] Final state reached !$
Interpreter closing...$'
}

@test "'cac' is palindrome" {
    run bash -c './_build/default/bin/turing our-machines/palindrome.json "cac" | cat -e'
    assert_output 'Interpreter starting...$
[<c>ac] (pick_character, c) -> (go_to_end_and_find_c, ., right)$
[.<a>c] (go_to_end_and_find_c, a) -> (go_to_end_and_find_c, a, right)$
[.a<c>] (go_to_end_and_find_c, c) -> (go_to_end_and_find_c, c, right)$
[.ac<.>] (go_to_end_and_find_c, .) -> (is_c, ., left)$
[.a<c>.] (is_c, c) -> (go_to_beginning, ., left)$
[.<a>..] (go_to_beginning, a) -> (go_to_beginning, a, left)$
[<.>a..] (go_to_beginning, .) -> (pick_character, ., right)$
[.<a>..] (pick_character, a) -> (go_to_end_and_find_a, ., right)$
[..<.>.] (go_to_end_and_find_a, .) -> (is_a, ., left)$
[.<.>..] (is_a, .) -> (write_is_palindrome, ., right)$
[..<.>.] (write_is_palindrome, .) -> (HALT, y, right)$
[..y<.>] Final state reached !$
Interpreter closing...$'
}

@test "'caac' is palindrome" {
    run bash -c './_build/default/bin/turing our-machines/palindrome.json "caac" | cat -e'
    assert_output 'Interpreter starting...$
[<c>aac] (pick_character, c) -> (go_to_end_and_find_c, ., right)$
[.<a>ac] (go_to_end_and_find_c, a) -> (go_to_end_and_find_c, a, right)$
[.a<a>c] (go_to_end_and_find_c, a) -> (go_to_end_and_find_c, a, right)$
[.aa<c>] (go_to_end_and_find_c, c) -> (go_to_end_and_find_c, c, right)$
[.aac<.>] (go_to_end_and_find_c, .) -> (is_c, ., left)$
[.aa<c>.] (is_c, c) -> (go_to_beginning, ., left)$
[.a<a>..] (go_to_beginning, a) -> (go_to_beginning, a, left)$
[.<a>a..] (go_to_beginning, a) -> (go_to_beginning, a, left)$
[<.>aa..] (go_to_beginning, .) -> (pick_character, ., right)$
[.<a>a..] (pick_character, a) -> (go_to_end_and_find_a, ., right)$
[..<a>..] (go_to_end_and_find_a, a) -> (go_to_end_and_find_a, a, right)$
[..a<.>.] (go_to_end_and_find_a, .) -> (is_a, ., left)$
[..<a>..] (is_a, a) -> (go_to_beginning, ., left)$
[.<.>...] (go_to_beginning, .) -> (pick_character, ., right)$
[..<.>..] (pick_character, .) -> (write_is_palindrome, ., right)$
[...<.>.] (write_is_palindrome, .) -> (HALT, y, right)$
[...y<.>] Final state reached !$
Interpreter closing...$'
}

@test "'cbc' is palindrome" {
    run bash -c './_build/default/bin/turing our-machines/palindrome.json "cbc" | cat -e'
    assert_output 'Interpreter starting...$
[<c>bc] (pick_character, c) -> (go_to_end_and_find_c, ., right)$
[.<b>c] (go_to_end_and_find_c, b) -> (go_to_end_and_find_c, b, right)$
[.b<c>] (go_to_end_and_find_c, c) -> (go_to_end_and_find_c, c, right)$
[.bc<.>] (go_to_end_and_find_c, .) -> (is_c, ., left)$
[.b<c>.] (is_c, c) -> (go_to_beginning, ., left)$
[.<b>..] (go_to_beginning, b) -> (go_to_beginning, b, left)$
[<.>b..] (go_to_beginning, .) -> (pick_character, ., right)$
[.<b>..] (pick_character, b) -> (go_to_end_and_find_b, ., right)$
[..<.>.] (go_to_end_and_find_b, .) -> (is_b, ., left)$
[.<.>..] (is_b, .) -> (write_is_palindrome, ., right)$
[..<.>.] (write_is_palindrome, .) -> (HALT, y, right)$
[..y<.>] Final state reached !$
Interpreter closing...$'
}

@test "'cbbc' is palindrome" {
    run bash -c './_build/default/bin/turing our-machines/palindrome.json "cbbc" | cat -e'
    assert_output 'Interpreter starting...$
[<c>bbc] (pick_character, c) -> (go_to_end_and_find_c, ., right)$
[.<b>bc] (go_to_end_and_find_c, b) -> (go_to_end_and_find_c, b, right)$
[.b<b>c] (go_to_end_and_find_c, b) -> (go_to_end_and_find_c, b, right)$
[.bb<c>] (go_to_end_and_find_c, c) -> (go_to_end_and_find_c, c, right)$
[.bbc<.>] (go_to_end_and_find_c, .) -> (is_c, ., left)$
[.bb<c>.] (is_c, c) -> (go_to_beginning, ., left)$
[.b<b>..] (go_to_beginning, b) -> (go_to_beginning, b, left)$
[.<b>b..] (go_to_beginning, b) -> (go_to_beginning, b, left)$
[<.>bb..] (go_to_beginning, .) -> (pick_character, ., right)$
[.<b>b..] (pick_character, b) -> (go_to_end_and_find_b, ., right)$
[..<b>..] (go_to_end_and_find_b, b) -> (go_to_end_and_find_b, b, right)$
[..b<.>.] (go_to_end_and_find_b, .) -> (is_b, ., left)$
[..<b>..] (is_b, b) -> (go_to_beginning, ., left)$
[.<.>...] (go_to_beginning, .) -> (pick_character, ., right)$
[..<.>..] (pick_character, .) -> (write_is_palindrome, ., right)$
[...<.>.] (write_is_palindrome, .) -> (HALT, y, right)$
[...y<.>] Final state reached !$
Interpreter closing...$'
}

@test "'cabac' is palindrome" {
    run bash -c './_build/default/bin/turing our-machines/palindrome.json "cabac" | cat -e'
    assert_output 'Interpreter starting...$
[<c>abac] (pick_character, c) -> (go_to_end_and_find_c, ., right)$
[.<a>bac] (go_to_end_and_find_c, a) -> (go_to_end_and_find_c, a, right)$
[.a<b>ac] (go_to_end_and_find_c, b) -> (go_to_end_and_find_c, b, right)$
[.ab<a>c] (go_to_end_and_find_c, a) -> (go_to_end_and_find_c, a, right)$
[.aba<c>] (go_to_end_and_find_c, c) -> (go_to_end_and_find_c, c, right)$
[.abac<.>] (go_to_end_and_find_c, .) -> (is_c, ., left)$
[.aba<c>.] (is_c, c) -> (go_to_beginning, ., left)$
[.ab<a>..] (go_to_beginning, a) -> (go_to_beginning, a, left)$
[.a<b>a..] (go_to_beginning, b) -> (go_to_beginning, b, left)$
[.<a>ba..] (go_to_beginning, a) -> (go_to_beginning, a, left)$
[<.>aba..] (go_to_beginning, .) -> (pick_character, ., right)$
[.<a>ba..] (pick_character, a) -> (go_to_end_and_find_a, ., right)$
[..<b>a..] (go_to_end_and_find_a, b) -> (go_to_end_and_find_a, b, right)$
[..b<a>..] (go_to_end_and_find_a, a) -> (go_to_end_and_find_a, a, right)$
[..ba<.>.] (go_to_end_and_find_a, .) -> (is_a, ., left)$
[..b<a>..] (is_a, a) -> (go_to_beginning, ., left)$
[..<b>...] (go_to_beginning, b) -> (go_to_beginning, b, left)$
[.<.>b...] (go_to_beginning, .) -> (pick_character, ., right)$
[..<b>...] (pick_character, b) -> (go_to_end_and_find_b, ., right)$
[...<.>..] (go_to_end_and_find_b, .) -> (is_b, ., left)$
[..<.>...] (is_b, .) -> (write_is_palindrome, ., right)$
[...<.>..] (write_is_palindrome, .) -> (HALT, y, right)$
[...y<.>.] Final state reached !$
Interpreter closing...$'
}

@test "'ab' is not palindrome" {
    run bash -c './_build/default/bin/turing our-machines/palindrome.json "ab" | cat -e'
    assert_output 'Interpreter starting...$
[<a>b] (pick_character, a) -> (go_to_end_and_find_a, ., right)$
[.<b>] (go_to_end_and_find_a, b) -> (go_to_end_and_find_a, b, right)$
[.b<.>] (go_to_end_and_find_a, .) -> (is_a, ., left)$
[.<b>.] (is_a, b) -> (write_is_not_palindrome, b, right)$
[.b<.>] (write_is_not_palindrome, .) -> (HALT, n, right)$
[.bn<.>] Final state reached !$
Interpreter closing...$'
}

@test "'ac' is not palindrome" {
    run bash -c './_build/default/bin/turing our-machines/palindrome.json "ac" | cat -e'
    assert_output 'Interpreter starting...$
[<a>c] (pick_character, a) -> (go_to_end_and_find_a, ., right)$
[.<c>] (go_to_end_and_find_a, c) -> (go_to_end_and_find_a, c, right)$
[.c<.>] (go_to_end_and_find_a, .) -> (is_a, ., left)$
[.<c>.] (is_a, c) -> (write_is_not_palindrome, c, right)$
[.c<.>] (write_is_not_palindrome, .) -> (HALT, n, right)$
[.cn<.>] Final state reached !$
Interpreter closing...$'
}

@test "'ba' is not palindrome" {
    run bash -c './_build/default/bin/turing our-machines/palindrome.json "ba" | cat -e'
    assert_output 'Interpreter starting...$
[<b>a] (pick_character, b) -> (go_to_end_and_find_b, ., right)$
[.<a>] (go_to_end_and_find_b, a) -> (go_to_end_and_find_b, a, right)$
[.a<.>] (go_to_end_and_find_b, .) -> (is_b, ., left)$
[.<a>.] (is_b, a) -> (write_is_not_palindrome, a, right)$
[.a<.>] (write_is_not_palindrome, .) -> (HALT, n, right)$
[.an<.>] Final state reached !$
Interpreter closing...$'
}

@test "'bc' is not palindrome" {
    run bash -c './_build/default/bin/turing our-machines/palindrome.json "bc" | cat -e'
    assert_output 'Interpreter starting...$
[<b>c] (pick_character, b) -> (go_to_end_and_find_b, ., right)$
[.<c>] (go_to_end_and_find_b, c) -> (go_to_end_and_find_b, c, right)$
[.c<.>] (go_to_end_and_find_b, .) -> (is_b, ., left)$
[.<c>.] (is_b, c) -> (write_is_not_palindrome, c, right)$
[.c<.>] (write_is_not_palindrome, .) -> (HALT, n, right)$
[.cn<.>] Final state reached !$
Interpreter closing...$'
}

@test "'ca' is not palindrome" {
    run bash -c './_build/default/bin/turing our-machines/palindrome.json "ca" | cat -e'
    assert_output 'Interpreter starting...$
[<c>a] (pick_character, c) -> (go_to_end_and_find_c, ., right)$
[.<a>] (go_to_end_and_find_c, a) -> (go_to_end_and_find_c, a, right)$
[.a<.>] (go_to_end_and_find_c, .) -> (is_c, ., left)$
[.<a>.] (is_c, a) -> (write_is_not_palindrome, a, right)$
[.a<.>] (write_is_not_palindrome, .) -> (HALT, n, right)$
[.an<.>] Final state reached !$
Interpreter closing...$'
}

@test "'cb' is not palindrome" {
    run bash -c './_build/default/bin/turing our-machines/palindrome.json "cb" | cat -e'
    assert_output 'Interpreter starting...$
[<c>b] (pick_character, c) -> (go_to_end_and_find_c, ., right)$
[.<b>] (go_to_end_and_find_c, b) -> (go_to_end_and_find_c, b, right)$
[.b<.>] (go_to_end_and_find_c, .) -> (is_c, ., left)$
[.<b>.] (is_c, b) -> (write_is_not_palindrome, b, right)$
[.b<.>] (write_is_not_palindrome, .) -> (HALT, n, right)$
[.bn<.>] Final state reached !$
Interpreter closing...$'
}

@test "'acab' is not palindrome" {
    run bash -c './_build/default/bin/turing our-machines/palindrome.json "acab" | cat -e'
    assert_output 'Interpreter starting...$
[<a>cab] (pick_character, a) -> (go_to_end_and_find_a, ., right)$
[.<c>ab] (go_to_end_and_find_a, c) -> (go_to_end_and_find_a, c, right)$
[.c<a>b] (go_to_end_and_find_a, a) -> (go_to_end_and_find_a, a, right)$
[.ca<b>] (go_to_end_and_find_a, b) -> (go_to_end_and_find_a, b, right)$
[.cab<.>] (go_to_end_and_find_a, .) -> (is_a, ., left)$
[.ca<b>.] (is_a, b) -> (write_is_not_palindrome, b, right)$
[.cab<.>] (write_is_not_palindrome, .) -> (HALT, n, right)$
[.cabn<.>] Final state reached !$
Interpreter closing...$'
}
