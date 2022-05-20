#!/usr/bin/env bats

setup() {
  load test_helper
  fixtures bats
}

@test "no arguments prints message and usage instructions" {
  run bats
  [ $status -eq 1 ]
  [ "${lines[0]}" == 'Error: Must specify at least one <test>' ]
  [ "${lines[1]%% *}" == 'Usage:' ]
}

@test "invalid option prints message and usage instructions" {
  run bats --invalid-option
  [ $status -eq 1 ]
  [ "${lines[0]}" == "Error: Bad command line option '--invalid-option'" ]
  [ "${lines[1]%% *}" == 'Usage:' ]
}

@test "-v and --version print version number" {
  run bats -v
  [ $status -eq 0 ]
  [ "$(expr "$output" : "Bats [0-9][0-9.]*")" -ne 0 ]
}

@test "-h and --help print help" {
  run bats -h
  [ $status -eq 0 ]
  [ "${#lines[@]}" -gt 3 ]
}

@test "invalid filename prints an error" {
  run bats nonexistent
  [ $status -eq 1 ]
  [ "$(expr "$output" : ".*does not exist")" -ne 0 ]
}

@test "empty test file runs zero tests" {
  run bats "$FIXTURE_ROOT/empty.bats"
  [ $status -eq 0 ]
  [ "$output" = "1..0" ]
}

@test "one passing test" {
  run bats "$FIXTURE_ROOT/passing.bats"
  [ $status -eq 0 ]
  [ "${lines[0]}" = "1..1" ]
  [ "${lines[1]}" = "ok 1 a passing test" ]
}

@test "summary passing tests" {
  run filter_control_sequences bats -p "$FIXTURE_ROOT/passing.bats"
  echo "$output"
  [ $status -eq 0 ]
  [ "${lines[2]}" = "1 test, 0 failures" ]
}

@test "summary passing and skipping tests" {
  run filter_control_sequences bats -p "$FIXTURE_ROOT/passing_and_skipping.bats"
  [ $status -eq 0 ]
  [ "${lines[4]}" = "3 tests, 0 failures, 2 skipped" ]
}

@test "tap passing and skipping tests" {
  run filter_control_sequences bats --formatter tap "$FIXTURE_ROOT/passing_and_skipping.bats"
  [ $status -eq 0 ]
  [ "${lines[0]}" = "1..3" ]
  [ "${lines[1]}" = "ok 1 a passing test" ]
  [ "${lines[2]}" = "ok 2 a skipped test with no reason # skip" ]
  [ "${lines[3]}" = "ok 3 a skipped test with a reason # skip for a really good reason" ]
}

@test "summary passing and failing tests" {
  run filter_control_sequences bats -p "$FIXTURE_ROOT/failing_and_passing.bats"
  [ $status -eq 0 ]
  [ "${lines[5]}" = "2 tests, 1 failure" ]
}

@test "summary passing, failing and skipping tests" {
  run filter_control_sequences bats -p "$FIXTURE_ROOT/passing_failing_and_skipping.bats"
  [ $status -eq 0 ]
  [ "${lines[6]}" = "3 tests, 1 failure, 1 skipped" ]
}

@test "tap passing, failing and skipping tests" {
  run filter_control_sequences bats --formatter tap "$FIXTURE_ROOT/passing_failing_and_skipping.bats"
  [ $status -eq 0 ]
  [ "${lines[0]}" = "1..3" ]
  [ "${lines[1]}" = "ok 1 a passing test" ]
  [ "${lines[2]}" = "ok 2 a skipping test # skip" ]
  [ "${lines[3]}" = "not ok 3 a failing test" ]
}

@test "BATS_CWD is correctly set to PWD as validated by bats_trim_filename" {
  local trimmed
  bats_trim_filename "$PWD/foo/bar" 'trimmed'
  printf 'ACTUAL: %s\n' "$trimmed" >&2
  [ "$trimmed" = 'foo/bar' ]
}

@test "one failing test" {
  run bats "$FIXTURE_ROOT/failing.bats"
  [ $status -eq 1 ]
  [ "${lines[0]}" = '1..1' ]
  [ "${lines[1]}" = 'not ok 1 a failing test' ]
  [ "${lines[2]}" = "# (in test file $RELATIVE_FIXTURE_ROOT/failing.bats, line 4)" ]
  [ "${lines[3]}" = "#   \`eval \"( exit \${STATUS:-1} )\"' failed" ]
}

@test "one failing and one passing test" {
  run bats "$FIXTURE_ROOT/failing_and_passing.bats"
  [ $status -eq 1 ]
  [ "${lines[0]}" = '1..2' ]
  [ "${lines[1]}" = 'not ok 1 a failing test' ]
  [ "${lines[2]}" = "# (in test file $RELATIVE_FIXTURE_ROOT/failing_and_passing.bats, line 2)" ]
  [ "${lines[3]}" = "#   \`false' failed" ]
  [ "${lines[4]}" = 'ok 2 a passing test' ]
}

@test "failing test with significant status" {
  STATUS=2 run bats "$FIXTURE_ROOT/failing.bats"
  [ $status -eq 1 ]
  [ "${lines[3]}" = "#   \`eval \"( exit \${STATUS:-1} )\"' failed with status 2" ]
}

@test "failing helper function logs the test case's line number" {
  run bats "$FIXTURE_ROOT/failing_helper.bats"
  [ $status -eq 1 ]
  [ "${lines[1]}" = 'not ok 1 failing helper function' ]
  [ "${lines[2]}" = "# (from function \`failing_helper' in file $RELATIVE_FIXTURE_ROOT/test_helper.bash, line 6," ]
  [ "${lines[3]}" = "#  in test file $RELATIVE_FIXTURE_ROOT/failing_helper.bats, line 5)" ]
  [ "${lines[4]}" = "#   \`failing_helper' failed" ]
}

@test "failing bash condition logs correct line number" {
  run bats "$FIXTURE_ROOT/failing_with_bash_cond.bats"
  [ "$status" -eq 1 ]
  [ "${#lines[@]}" -eq 4 ]
  [ "${lines[1]}" = 'not ok 1 a failing test' ]
  [ "${lines[2]}" = "# (in test file $RELATIVE_FIXTURE_ROOT/failing_with_bash_cond.bats, line 4)" ]
  [ "${lines[3]}" = "#   \`[[ 1 == 2 ]]' failed" ]
}

@test "failing bash expression logs correct line number" {
  run bats "$FIXTURE_ROOT/failing_with_bash_expression.bats"
  [ "$status" -eq 1 ]
  [ "${#lines[@]}" -eq 4 ]
  [ "${lines[1]}" = 'not ok 1 a failing test' ]
  [ "${lines[2]}" = "# (in test file $RELATIVE_FIXTURE_ROOT/failing_with_bash_expression.bats, line 3)" ]
  [ "${lines[3]}" = "#   \`(( 1 == 2 ))' failed" ]
}

@test "failing negated command logs correct line number" {
  run bats "$FIXTURE_ROOT/failing_with_negated_command.bats"
  [ "$status" -eq 1 ]
  [ "${#lines[@]}" -eq 4 ]
  [ "${lines[1]}" = 'not ok 1 a failing test' ]
  [ "${lines[2]}" = "# (in test file $RELATIVE_FIXTURE_ROOT/failing_with_negated_command.bats, line 3)" ]
  [ "${lines[3]}" = "#   \`! true' failed" ]
}

@test "test environments are isolated" {
  run bats "$FIXTURE_ROOT/environment.bats"
  [ $status -eq 0 ]
}

@test "setup is run once before each test" {
  unset BATS_NUMBER_OF_PARALLEL_JOBS BATS_NO_PARALLELIZE_ACROSS_FILES
  # shellcheck disable=SC2031,SC2030
  export BATS_TEST_SUITE_TMPDIR="${BATS_TEST_TMPDIR}"
  run bats "$FIXTURE_ROOT/setup.bats"
  [ $status -eq 0 ]
  run cat "$BATS_TEST_SUITE_TMPDIR/setup.log"
  [ ${#lines[@]} -eq 3 ]
}

@test "teardown is run once after each test, even if it fails" {
  unset BATS_NUMBER_OF_PARALLEL_JOBS BATS_NO_PARALLELIZE_ACROSS_FILES
  # shellcheck disable=SC2031,SC2030
  export BATS_TEST_SUITE_TMPDIR="${BATS_TEST_TMPDIR}"
  run bats "$FIXTURE_ROOT/teardown.bats"
  [ $status -eq 1 ]
  run cat "$BATS_TEST_SUITE_TMPDIR/teardown.log"
  [ ${#lines[@]} -eq 3 ]
}

@test "setup failure" {
  run bats "$FIXTURE_ROOT/failing_setup.bats"
  [ $status -eq 1 ]
  [ "${lines[1]}" = 'not ok 1 truth' ]
  [ "${lines[2]}" = "# (from function \`setup' in test file $RELATIVE_FIXTURE_ROOT/failing_setup.bats, line 2)" ]
  [ "${lines[3]}" = "#   \`false' failed" ]
}

@test "passing test with teardown failure" {
  PASS=1 run bats "$FIXTURE_ROOT/failing_teardown.bats"
  [ $status -eq 1 ]
  echo "$output"
  [ "${lines[1]}" = 'not ok 1 truth' ]
  [ "${lines[2]}" = "# (from function \`teardown' in test file $RELATIVE_FIXTURE_ROOT/failing_teardown.bats, line 2)" ]
  [ "${lines[3]}" = "#   \`eval \"( exit \${STATUS:-1} )\"' failed" ]
}

@test "failing test with teardown failure" {
  PASS=0 run bats "$FIXTURE_ROOT/failing_teardown.bats"
  [ $status -eq 1 ]
  [ "${lines[1]}" =  'not ok 1 truth' ]
  [ "${lines[2]}" =  "# (in test file $RELATIVE_FIXTURE_ROOT/failing_teardown.bats, line 6)" ]
  [ "${lines[3]}" = $'#   `[ "$PASS" = 1 ]\' failed' ]
}

@test "teardown failure with significant status" {
  PASS=1 STATUS=2 run bats "$FIXTURE_ROOT/failing_teardown.bats"
  [ $status -eq 1 ]
  [ "${lines[3]}" = "#   \`eval \"( exit \${STATUS:-1} )\"' failed with status 2" ]
}

@test "failing test file outside of BATS_CWD" {
  cd "${BATS_TEST_TMPDIR}"
  run bats "$FIXTURE_ROOT/failing.bats"
  [ $status -eq 1 ]
  [ "${lines[2]}" = "# (in test file $FIXTURE_ROOT/failing.bats, line 4)" ]
}

@test "output is discarded for passing tests and printed for failing tests" {
  run bats "$FIXTURE_ROOT/output.bats"
  [ $status -eq 1 ]
  [ "${lines[6]}"  = '# failure stdout 1' ]
  [ "${lines[7]}"  = '# failure stdout 2' ]
  [ "${lines[11]}" = '# failure stderr' ]
}

@test "-c prints the number of tests" {
  run bats -c "$FIXTURE_ROOT/empty.bats"
  [ $status -eq 0 ]
  [ "$output" = 0 ]

  run bats -c "$FIXTURE_ROOT/output.bats"
  [ $status -eq 0 ]
  [ "$output" = 4 ]
}

@test "dash-e is not mangled on beginning of line" {
  run bats "$FIXTURE_ROOT/intact.bats"
  [ $status -eq 0 ]
  [ "${lines[1]}" = "ok 1 dash-e on beginning of line" ]
}

@test "dos line endings are stripped before testing" {
  run bats "$FIXTURE_ROOT/dos_line_no_shellcheck.bats"
  [ $status -eq 0 ]
}

@test "test file without trailing newline" {
  run bats "$FIXTURE_ROOT/without_trailing_newline.bats"
  [ $status -eq 0 ]
  [ "${lines[1]}" = "ok 1 truth" ]
}

@test "skipped tests" {
  run bats "$FIXTURE_ROOT/skipped.bats"
  [ $status -eq 0 ]
  [ "${lines[1]}" = "ok 1 a skipped test # skip" ]
  [ "${lines[2]}" = "ok 2 a skipped test with a reason # skip a reason" ]
}

@test "skipped test with parens (pretty formatter)" {
  run bats --pretty "$FIXTURE_ROOT/skipped_with_parens.bats"
  [ $status -eq 0 ]

  # Some systems (Alpine, for example) seem to emit an extra whitespace into
  # entries in the 'lines' array when a carriage return is present from the
  # pretty formatter.  This is why a '+' is used after the 'skipped' note.
  [[ "${lines[*]}" =~ "- a skipped test with parentheses in the reason (skipped: "+"a reason (with parentheses))" ]]
}

@test "extended syntax" {
  emulate_bats_env
  run bats-exec-suite -x "$FIXTURE_ROOT/failing_and_passing.bats"
  echo "$output"
  [ $status -eq 1 ]
  [ "${lines[1]}" = "suite $FIXTURE_ROOT/failing_and_passing.bats" ]
  [ "${lines[2]}" = 'begin 1 a failing test' ]
  [ "${lines[3]}" = 'not ok 1 a failing test' ]
  [ "${lines[6]}" = 'begin 2 a passing test' ]
  [ "${lines[7]}" = 'ok 2 a passing test' ]
}

@test "timing syntax" {
  run bats -T "$FIXTURE_ROOT/failing_and_passing.bats"
  echo "$output"
  [ $status -eq 1 ]
  regex='not ok 1 a failing test in [0-9]+ms'
  [[ "${lines[1]}" =~ $regex ]]
  regex='ok 2 a passing test in [0-9]+ms'
  [[ "${lines[4]}" =~ $regex ]]
}

@test "extended timing syntax" {
  emulate_bats_env
  run bats-exec-suite -x -T "$FIXTURE_ROOT/failing_and_passing.bats"
  echo "$output"
  [ $status -eq 1 ]
  regex="not ok 1 a failing test in [0-9]+ms"
  [ "${lines[2]}" = 'begin 1 a failing test' ]
  [[ "${lines[3]}" =~ $regex ]]
  [ "${lines[6]}" = 'begin 2 a passing test' ]
  regex="ok 2 a passing test in [0-9]+ms"
  [[ "${lines[7]}" =~ $regex ]]
}

@test "time is greater than 0ms for long test" {
  emulate_bats_env
  run bats-exec-suite -x -T "$FIXTURE_ROOT/run_long_command.bats"
  echo "$output"
  [ $status -eq 0 ]
  regex="ok 1 run long command in [1-9][0-9]*ms"
  [[ "${lines[3]}" =~ $regex ]]
}

@test "pretty and tap formats" {
  run bats --formatter tap "$FIXTURE_ROOT/passing.bats"
  tap_output="$output"
  [ $status -eq 0 ]

  run bats --pretty "$FIXTURE_ROOT/passing.bats"
  pretty_output="$output"
  [ $status -eq 0 ]

  [ "$tap_output" != "$pretty_output" ]
}

@test "pretty formatter bails on invalid tap" {
  run bats-format-pretty < <(printf "This isn't TAP!\nGood day to you\n")
  [ $status -eq 0 ]
  [ "${lines[0]}" = "This isn't TAP!" ]
  [ "${lines[1]}" = "Good day to you" ]
}

@test "single-line tests" {
  run bats "$FIXTURE_ROOT/single_line_no_shellcheck.bats"
  [ $status -eq 1 ]
  [ "${lines[1]}" =  'ok 1 empty' ]
  [ "${lines[2]}" =  'ok 2 passing' ]
  [ "${lines[3]}" =  'ok 3 input redirection' ]
  [ "${lines[4]}" =  'not ok 4 failing' ]
  [ "${lines[5]}" =  "# (in test file $RELATIVE_FIXTURE_ROOT/single_line_no_shellcheck.bats, line 9)" ]
  [ "${lines[6]}" = $'#   `@test "failing" { false; }\' failed' ]
}

@test "testing IFS not modified by run" {
  run bats "$FIXTURE_ROOT/loop_keep_IFS.bats"
  [ $status -eq 0 ]
  [ "${lines[1]}" = "ok 1 loop_func" ]
}

@test "expand variables in test name" {
  SUITE='test/suite' run bats "$FIXTURE_ROOT/expand_var_in_test_name.bats"
  [ $status -eq 0 ]
  [ "${lines[1]}" = "ok 1 test/suite: test with variable in name" ]
}

@test "handle quoted and unquoted test names" {
  run bats "$FIXTURE_ROOT/quoted_and_unquoted_test_names_no_shellcheck.bats"
  [ $status -eq 0 ]
  [ "${lines[1]}" = "ok 1 single-quoted name" ]
  [ "${lines[2]}" = "ok 2 double-quoted name" ]
  [ "${lines[3]}" = "ok 3 unquoted name" ]
}

@test 'ensure compatibility with unofficial Bash strict mode' {
  local expected='ok 1 unofficial Bash strict mode conditions met'

  if [[ -n "$BATS_NUMBER_OF_PARALLEL_JOBS" ]]; then
    if [[ -z "$BATS_NO_PARALLELIZE_ACROSS_FILES" ]]; then
      type -p parallel &>/dev/null || skip "Don't check file parallelized without GNU parallel"
    fi
    (type -p flock &>/dev/null || type -p shlock &>/dev/null) || skip "Don't check parallelized without flock/shlock "
  fi

  # PATH required for windows
  # HOME required to avoid error from GNU Parallel
  # Run Bats under SHELLOPTS=nounset (recursive `set -u`) to catch 
  # as many unset variable accesses as possible.
  run env - \
          "PATH=$PATH" \
          "HOME=$HOME" \
          "BATS_NO_PARALLELIZE_ACROSS_FILES=$BATS_NO_PARALLELIZE_ACROSS_FILES" \
          "BATS_NUMBER_OF_PARALLEL_JOBS=$BATS_NUMBER_OF_PARALLEL_JOBS" \
          SHELLOPTS=nounset \
      "${BATS_ROOT}/bin/bats" "$FIXTURE_ROOT/unofficial_bash_strict_mode.bats"
  if [[ "$status" -ne 0 || "${lines[1]}" != "$expected" ]]; then
    cat <<END_OF_ERR_MSG

This test failed because the Bats internals are violating one of the
constraints imposed by:

--------
$(< "$FIXTURE_ROOT/unofficial_bash_strict_mode.bash")
--------

See:
- https://github.com/sstephenson/bats/issues/171
- http://redsymbol.net/articles/unofficial-bash-strict-mode/

If there is no error output from the test fixture, run the following to
debug the problem:

  $ SHELLOPTS=nounset bats $RELATIVE_FIXTURE_ROOT/unofficial_bash_strict_mode.bats

If there's no error output even with this command, make sure you're using the
latest version of Bash, as versions before 4.1-alpha may not produce any error
output for unset variable accesses.

If there's no output even when running the latest Bash, the problem may reside
in the DEBUG trap handler. A particularly sneaky issue is that in Bash before
4.1-alpha, an expansion of an empty array, e.g. "\${FOO[@]}", is considered
an unset variable access. The solution is to add a size check before the
expansion, e.g. [[ "\${#FOO[@]}" -ne 0 ]].

END_OF_ERR_MSG
    emit_debug_output && return 1
  fi
}

@test "parse @test lines with various whitespace combinations" {
  run bats "$FIXTURE_ROOT/whitespace_no_shellcheck.bats"
  [ $status -eq 0 ]
  [ "${lines[1]}" = 'ok 1 no extra whitespace' ]
  [ "${lines[2]}" = 'ok 2 tab at beginning of line' ]
  [ "${lines[3]}" = 'ok 3 tab before description' ]
  [ "${lines[4]}" = 'ok 4 tab before opening brace' ]
  [ "${lines[5]}" = 'ok 5 tabs at beginning of line and before description' ]
  [ "${lines[6]}" = 'ok 6 tabs at beginning, before description, before brace' ]
  [ "${lines[7]}" = 'ok 7 extra whitespace around single-line test' ]
  [ "${lines[8]}" = 'ok 8 no extra whitespace around single-line test' ]
  [ "${lines[9]}" = 'ok 9 parse unquoted name between extra whitespace' ]
  [ "${lines[10]}" = 'ok 10 {' ]  # unquoted single brace is a valid description
  [ "${lines[11]}" = 'ok 11 ' ]   # empty name from single quote
}

@test "duplicate tests error and generate a warning on stderr" {
  run bats --tap "$FIXTURE_ROOT/duplicate-tests_no_shellcheck.bats"
  [ $status -eq 1 ]

  local expected='Error: Duplicate test name(s) in file '
  expected+="\"${FIXTURE_ROOT}/duplicate-tests_no_shellcheck.bats\": test_gizmo_test"

  printf 'expected: "%s"\n' "$expected" >&2
  printf 'actual:   "%s"\n' "${lines[0]}" >&2
  [ "${lines[0]}" = "$expected" ]

  printf 'num lines: %d\n' "${#lines[*]}" >&2
  [ "${#lines[*]}" = "1" ]
}

@test "sourcing a nonexistent file in setup produces error output" {
  run bats "$FIXTURE_ROOT/source_nonexistent_file_in_setup.bats"
  [ $status -eq 1 ]
  [ "${lines[1]}" = 'not ok 1 sourcing nonexistent file fails in setup' ]
  [ "${lines[2]}" = "# (from function \`setup' in test file $RELATIVE_FIXTURE_ROOT/source_nonexistent_file_in_setup.bats, line 3)" ]
  [ "${lines[3]}" = "#   \`source \"nonexistent file\"' failed" ]
}

@test "referencing unset parameter in setup produces error output" {
  run bats "$FIXTURE_ROOT/reference_unset_parameter_in_setup.bats"
  [ $status -eq 1 ]
  [ "${lines[1]}" = 'not ok 1 referencing unset parameter fails in setup' ]
  [ "${lines[2]}" = "# (from function \`setup' in test file $RELATIVE_FIXTURE_ROOT/reference_unset_parameter_in_setup.bats, line 4)" ]
  [ "${lines[3]}" = "#   \`echo \"\$unset_parameter\"' failed" ]
}

@test "sourcing a nonexistent file in test produces error output" {
  run bats "$FIXTURE_ROOT/source_nonexistent_file.bats"
  [ $status -eq 1 ]
  [ "${lines[1]}" = 'not ok 1 sourcing nonexistent file fails' ]
  [ "${lines[2]}" = "# (in test file $RELATIVE_FIXTURE_ROOT/source_nonexistent_file.bats, line 3)" ]
  [ "${lines[3]}" = "#   \`source \"nonexistent file\"' failed" ]
}

@test "referencing unset parameter in test produces error output" {
  run bats "$FIXTURE_ROOT/reference_unset_parameter.bats"
  [ $status -eq 1 ]
  [ "${lines[1]}" = 'not ok 1 referencing unset parameter fails' ]
  [ "${lines[2]}" = "# (in test file $RELATIVE_FIXTURE_ROOT/reference_unset_parameter.bats, line 4)" ]
  [ "${lines[3]}" = "#   \`echo \"\$unset_parameter\"' failed" ]
}

@test "sourcing a nonexistent file in teardown produces error output" {
  run bats "$FIXTURE_ROOT/source_nonexistent_file_in_teardown.bats"
  [ $status -eq 1 ]
  [ "${lines[1]}" = 'not ok 1 sourcing nonexistent file fails in teardown' ]
  [ "${lines[2]}" = "# (from function \`teardown' in test file $RELATIVE_FIXTURE_ROOT/source_nonexistent_file_in_teardown.bats, line 3)" ]
  [ "${lines[3]}" = "#   \`source \"nonexistent file\"' failed" ]
}

@test "referencing unset parameter in teardown produces error output" {
  run bats "$FIXTURE_ROOT/reference_unset_parameter_in_teardown.bats"
  [ $status -eq 1 ]
  [ "${lines[1]}" = 'not ok 1 referencing unset parameter fails in teardown' ]
  [ "${lines[2]}" = "# (from function \`teardown' in test file $RELATIVE_FIXTURE_ROOT/reference_unset_parameter_in_teardown.bats, line 4)" ]
  [ "${lines[3]}" = "#   \`echo \"\$unset_parameter\"' failed" ]
}

@test "execute exported function without breaking failing test output" {
  exported_function() { return 0; }
  export -f exported_function
  run bats "$FIXTURE_ROOT/exported_function.bats"
  [ $status -eq 1 ]
  [ "${lines[0]}" = "1..1" ]
  [ "${lines[1]}" = "not ok 1 failing test" ]
  [ "${lines[2]}" = "# (in test file $RELATIVE_FIXTURE_ROOT/exported_function.bats, line 7)" ]
  [ "${lines[3]}" = "#   \`false' failed" ]
  [ "${lines[4]}" = "# a='exported_function'" ]
}

@test "output printed even when no final newline" {
  run bats "$FIXTURE_ROOT/no-final-newline.bats"
  printf 'num lines: %d\n' "${#lines[@]}" >&2
  printf 'LINE: %s\n' "${lines[@]}" >&2
  [ "$status" -eq 1 ]
  [ "${#lines[@]}" -eq 11 ]
  [ "${lines[1]}" = 'not ok 1 error in test' ]
  [ "${lines[2]}" = "# (in test file $RELATIVE_FIXTURE_ROOT/no-final-newline.bats, line 3)" ]
  [ "${lines[3]}" = "#   \`false' failed" ]
  [ "${lines[4]}" = '# foo' ]
  [ "${lines[5]}" = '# bar' ]
  [ "${lines[6]}" = 'not ok 2 test function returns nonzero' ]
  [ "${lines[7]}" = "# (in test file $RELATIVE_FIXTURE_ROOT/no-final-newline.bats, line 8)" ]
  [ "${lines[8]}" = "#   \`return 1' failed" ]
  [ "${lines[9]}" = '# foo' ]
  [ "${lines[10]}" = '# bar' ]
}

@test "run tests which consume stdin (see #197)" {
  run bats "$FIXTURE_ROOT/read_from_stdin.bats"
  [ "$status" -eq 0 ]
  [[ "${lines[0]}" == "1..3" ]]
  [[ "${lines[1]}" == "ok 1 test 1" ]]
  [[ "${lines[2]}" == "ok 2 test 2 with	TAB in name" ]]
  [[ "${lines[3]}" == "ok 3 test 3" ]]
}

@test "report correct line on unset variables" {
  LANG=C run bats "$FIXTURE_ROOT/unbound_variable.bats"
  [ "$status" -eq 1 ]
  [ "${#lines[@]}" -eq 9 ]
  [ "${lines[1]}" = 'not ok 1 access unbound variable' ]
  [ "${lines[2]}" = "# (in test file $RELATIVE_FIXTURE_ROOT/unbound_variable.bats, line 9)" ]
  [ "${lines[3]}" = "#   \`foo=\$unset_variable' failed" ]
  # shellcheck disable=SC2076
  [[ "${lines[4]}" =~ ".bats: line 9:" ]]
  [ "${lines[5]}" = 'not ok 2 access second unbound variable' ]
  [ "${lines[6]}" = "# (in test file $RELATIVE_FIXTURE_ROOT/unbound_variable.bats, line 15)" ]
  [ "${lines[7]}" = "#   \`foo=\$second_unset_variable' failed" ]
  # shellcheck disable=SC2076
  [[ "${lines[8]}" =~ ".bats: line 15:" ]]
}

@test "report correct line on external function calls" {
  run bats "$FIXTURE_ROOT/external_function_calls.bats"
  [ "$status" -eq 1 ]

  expectedNumberOfTests=12
  linesPerTest=5

  outputOffset=1
  currentErrorLine=9

  for t in $(seq $expectedNumberOfTests); do
    # shellcheck disable=SC2076
    [[ "${lines[$outputOffset]}" =~ "not ok $t " ]]

    [[ "${lines[$outputOffset]}" =~ stackdepth=([0-9]+) ]]
    stackdepth="${BASH_REMATCH[1]}"
    case "${stackdepth}" in
      1)
        [ "${lines[$((outputOffset + 1))]}" = "# (in test file $RELATIVE_FIXTURE_ROOT/external_function_calls.bats, line $currentErrorLine)" ]
        outputOffset=$((outputOffset + 3))
        ;;
      2)
        [[ "${lines[$((outputOffset + 1))]}" =~ ^'# (from function `'.*\'' in file '.*'/test_helper.bash, line '[0-9]+,$ ]]
        [ "${lines[$((outputOffset + 2))]}" = "#  in test file $RELATIVE_FIXTURE_ROOT/external_function_calls.bats, line $currentErrorLine)" ]
        outputOffset=$((outputOffset + 4))
        ;;
      *)
        printf 'error: stackdepth=%s not implemented\n' "${stackdepth}" >&2
        return 1
    esac
    currentErrorLine=$((currentErrorLine + linesPerTest))
  done
}

@test "test count validator catches mismatch and returns non zero" {
  # shellcheck source=lib/bats-core/validator.bash
  source "$BATS_ROOT/lib/bats-core/validator.bash"
  export -f bats_test_count_validator
  run bash -c "echo $'1..1\n' | bats_test_count_validator"
  [[ $status -ne 0 ]]

  run bash -c "echo $'1..1\nok 1\nok 2' | bats_test_count_validator"
  [[ $status -ne 0 ]]

  run bash -c "echo $'1..1\nok 1' | bats_test_count_validator"
  [[ $status -eq 0 ]]
}

@test "running the same file twice runs its tests twice without errors" {
  run bats "$FIXTURE_ROOT/passing.bats" "$FIXTURE_ROOT/passing.bats"
  echo "$output"
  [[ $status -eq 0 ]]
  [[ "${lines[0]}" == "1..2" ]] # got 2x1 tests
}

@test "Don't use unbound variables inside bats (issue #340)" {
  run bats "$FIXTURE_ROOT/set_-eu_in_setup_and_teardown.bats"
  echo "$output"
  [[ "${lines[0]}" == "1..4" ]]
  [[ "${lines[1]}" == "ok 1 skipped test # skip" ]]
  [[ "${lines[2]}" == "ok 2 skipped test with reason # skip reason" ]]
  [[ "${lines[3]}" == "ok 3 passing test" ]]
  [[ "${lines[4]}" == "not ok 4 failing test" ]]
  [[ "${lines[5]}" == "# (in test file $RELATIVE_FIXTURE_ROOT/set_-eu_in_setup_and_teardown.bats, line 22)" ]]
  [[ "${lines[6]}" == "#   \`false' failed" ]]
  [[ "${#lines[@]}" -eq 7 ]]
}

@test "filenames with tab can be used" {
  [[ "$OSTYPE" == "linux"* ]] || skip "FS cannot deal with tabs in filenames"

  cp "${FIXTURE_ROOT}/tab in filename.bats" "${BATS_TEST_TMPDIR}/tab"$'\t'"in filename.bats"
  bats "${BATS_TEST_TMPDIR}/tab"$'\t'"in filename.bats"
}

@test "each file is evaluated n+1 times" {
  # shellcheck disable=SC2031,SC2030
  export TEMPFILE="$BATS_TEST_TMPDIR/$BATS_TEST_NAME.log"
  run bats "$FIXTURE_ROOT/evaluation_count/"

  cat "$TEMPFILE"

  run grep "file1" "$TEMPFILE"
  [[ ${#lines[@]} -eq 2 ]]

  run grep "file2" "$TEMPFILE"
  [[ ${#lines[@]} -eq 3 ]]
}

@test "Don't hang on CTRL-C (issue #353)" {
  if [[ "$BATS_NUMBER_OF_PARALLEL_JOBS" -gt 1 ]]; then
    skip "Aborts don't work in parallel mode"
  fi
  load 'concurrent-coordination'
  # shellcheck disable=SC2031,SC2030
  export SINGLE_USE_LATCH_DIR="${BATS_TEST_TMPDIR}"

  # guarantee that background processes get their own process group -> pid=pgid
  set -m
  bats "$FIXTURE_ROOT/hang_in_test.bats" & # don't block execution, or we cannot send signals
  SUBPROCESS_PID=$!

  single-use-latch::wait hang_in_test 1

  # emulate CTRL-C by sending SIGINT to the whole process group
  kill -SIGINT -- -$SUBPROCESS_PID

  sleep 1 # wait for the signal to be acted upon

  # when the process is gone, we cannot deliver a signal anymore, getting non-zero from kill
  run kill -0 -- -$SUBPROCESS_PID
  [[ $status -ne 0 ]] \
    || (kill -9 -- -$SUBPROCESS_PID; false)
    #   ^ kill the process for good when SIGINT failed,
    #     to avoid waiting endlessly for stuck children to finish
}

@test "test comment style" {
  run bats "$FIXTURE_ROOT/comment_style.bats"
  [ $status -eq 0 ]
  [ "${lines[0]}" = '1..6' ]
  [ "${lines[1]}" = 'ok 1 should_be_found' ]
  [ "${lines[2]}" = 'ok 2 should_be_found_with_trailing_whitespace' ]
  [ "${lines[3]}" = 'ok 3 should_be_found_with_parens' ]
  [ "${lines[4]}" = 'ok 4 should_be_found_with_parens_and_whitespace' ]
  [ "${lines[5]}" = 'ok 5 should_be_found_with_function_and_parens' ]
  [ "${lines[6]}" = 'ok 6 should_be_found_with_function_parens_and_whitespace' ]
}

@test "test works even if PATH is reset" {
  run bats "$FIXTURE_ROOT/update_path_env.bats"
  [ "$status" -eq 1 ]
  [ "${lines[4]}" = "# /usr/local/bin:/usr/bin:/bin" ]
}

@test "Test nounset does not trip up bats' internals (see #385)" {
  # don't export nounset within this file or we might trip up the testsuite itself,
  # getting bad diagnostics
  run bash -c "set -o nounset; export SHELLOPTS; bats --tap '$FIXTURE_ROOT/passing.bats'"
  echo "$output"
  [ "${lines[0]}" = "1..1" ]
  [ "${lines[1]}" = "ok 1 a passing test" ]
  [ ${#lines[@]} = 2 ]
}

@test "run tmpdir is cleaned up by default" {
  TEST_TMPDIR="${BATS_TEST_TMPDIR}/$BATS_TEST_NAME"
  bats --tempdir "$TEST_TMPDIR" "$FIXTURE_ROOT/passing.bats"

  [ ! -d "$TEST_TMPDIR" ]
}

@test "run tmpdir is not cleanup up with --no-cleanup-tempdir" {
  TEST_TMPDIR="${BATS_TEST_TMPDIR}/$BATS_TEST_NAME"
  bats --tempdir "$TEST_TMPDIR" --no-tempdir-cleanup "$FIXTURE_ROOT/passing.bats"

  [ -d "$TEST_TMPDIR" ]

  # should also find preprocessed files!
  [ "$(find "$TEST_TMPDIR" -name '*.src' | wc -l)" -eq 1 ]
}

@test "All formatters (except cat) implement the callback interface" {
  cd "$BATS_ROOT/libexec/bats-core/"
  for formatter in bats-format-*; do
    # the cat formatter is not expected to implement this interface
    if [[ "$formatter" == *"bats-format-cat" ]]; then
      continue
    fi
    tested_at_least_one_formatter=1
    echo "Formatter: ${formatter}"
    # the replay should be possible without errors
    bash -u "$formatter" >/dev/null <<EOF
1..1
suite "$BATS_FIXTURE_ROOT/failing.bats"
# output from setup_file
begin 1 test_a_failing_test
# fd3 output from test
not ok 1 a failing test
# (in test file test/fixtures/bats/failing.bats, line 4)
#   \`eval "( exit ${STATUS:-1} )"' failed
begin 2 test_a_successful_test
ok 2 a succesful test
unknown line
EOF
  done

  [[ -n "$tested_at_least_one_formatter" ]]
}

@test "run should exit if tmpdir exist" {
  local dir
  dir=$(mktemp -d "${BATS_RUN_TMPDIR}/BATS_RUN_TMPDIR_TEST.XXXXXX")
  run bats --tempdir "${dir}" "$FIXTURE_ROOT/passing.bats"
  [ "$status" -eq 1 ]
  [ "${lines[0]}" == "Error: BATS_RUN_TMPDIR (${dir}) already exists" ]
  [ "${lines[1]}" == "Reusing old run directories can lead to unexpected results ... aborting!" ]
}

@test "run should exit if TMPDIR can't be created" {
  local dir
  dir=$(mktemp "${BATS_RUN_TMPDIR}/BATS_RUN_TMPDIR_TEST.XXXXXX")
  run bats --tempdir "${dir}" "$FIXTURE_ROOT/passing.bats"
  [ "$status" -eq 1 ]
  [ "${lines[1]}" == "Error: Failed to create BATS_RUN_TMPDIR (${dir})" ]
}

@test "Fail if BATS_TMPDIR does not exist or is not writable" {
  # shellcheck disable=SC2031,SC2030
  export TMPDIR
  TMPDIR=$(mktemp -u "${BATS_RUN_TMPDIR}/donotexist.XXXXXX")
  run bats "$FIXTURE_ROOT/BATS_TMPDIR.bats"
  echo "$output"
  [ "$status" -eq 1 ]
  [ "${lines[0]}" = "Error: BATS_TMPDIR (${TMPDIR}) does not exist or is not a directory" ]
}

@test "Setting BATS_TMPDIR is ignored" {
  unset TMPDIR # ensure we don't have a predefined value
  expected="/tmp" run bats "$FIXTURE_ROOT/BATS_TMPDIR.bats"
  echo "$output"
  [ "$status" -eq 0 ]
  BATS_TMPDIR="${BATS_RUN_TMPDIR}" expected="/tmp" run bats "$FIXTURE_ROOT/BATS_TMPDIR.bats"
  [ "$status" -eq 0 ]
}

@test "Parallel mode works on MacOS with over subscription (issue #433)" {
  type -p parallel &>/dev/null || skip "--jobs requires GNU parallel"
  (type -p flock &>/dev/null || type -p shlock &>/dev/null) || skip "--jobs requires flock/shlock"
  run bats -j 2 "$FIXTURE_ROOT/issue-433"

  [ "$status" -eq 0 ]
  [[ "$output" != *"No such file or directory"* ]] || exit 1 # ensure failures are detected with old bash
}

@test "Failure in free code (see #399)" {
  run bats --tap "$FIXTURE_ROOT/failure_in_free_code.bats"
  echo "$output"
  [ "$status" -ne 0 ]
  [ "${lines[0]}" == 1..1 ]
  [ "${lines[1]}" == 'not ok 1 setup_file failed' ]
  [ "${lines[2]}" == "# (from function \`helper' in file $RELATIVE_FIXTURE_ROOT/failure_in_free_code.bats, line 4," ]
  [ "${lines[3]}" == "#  in test file $RELATIVE_FIXTURE_ROOT/failure_in_free_code.bats, line 7)" ]
  [ "${lines[4]}" == "#   \`helper' failed" ]
}

@test "CTRL-C aborts and fails the current test" {
  if [[ "$BATS_NUMBER_OF_PARALLEL_JOBS" -gt 1 ]]; then
    skip "Aborts don't work in parallel mode"
  fi
  
  # shellcheck disable=SC2031,SC2030
  export TEMPFILE="$BATS_TEST_TMPDIR/$BATS_TEST_NAME.log"

  # guarantee that background processes get their own process group -> pid=pgid
  set -m
  
  load 'concurrent-coordination'
  # shellcheck disable=SC2031,SC2030
  export SINGLE_USE_LATCH_DIR="${BATS_SUITE_TMPDIR}"
  # we cannot use run for a background task, so we have to store the output for later
  bats "$FIXTURE_ROOT/hang_in_test.bats" --tap  >"$TEMPFILE" 2>&1 & # don't block execution, or we cannot send signals

  SUBPROCESS_PID=$!

  single-use-latch::wait hang_in_test 1 10 || (cat "$TEMPFILE"; false) # still forward output on timeout

  # emulate CTRL-C by sending SIGINT to the whole process group
  kill -SIGINT -- -$SUBPROCESS_PID || (cat "$TEMPFILE"; false)

  # the test suite must be marked as failed!
  wait $SUBPROCESS_PID && return 1

  run cat "$TEMPFILE"
  echo "$output"

  [ "${lines[1]}" == "not ok 1 test" ]
  # due to scheduling the exact line will vary but we should exit with 130
  [[ "${lines[3]}" == *"failed with status 130" ]] || false
  [ "${lines[4]}" == "# Received SIGINT, aborting ..." ]
}

@test "CTRL-C aborts and fails the current run" {
  if [[ "$BATS_NUMBER_OF_PARALLEL_JOBS" -gt 1 ]]; then
    skip "Aborts don't work in parallel mode"
  fi

  # shellcheck disable=SC2031,2030
  export TEMPFILE="$BATS_TEST_TMPDIR/$BATS_TEST_NAME.log"

  # guarantee that background processes get their own process group -> pid=pgid
  set -m
  
  load 'concurrent-coordination'
  # shellcheck disable=SC2031,SC2030
  export SINGLE_USE_LATCH_DIR="${BATS_SUITE_TMPDIR}"
  # we cannot use run for a background task, so we have to store the output for later
  bats "$FIXTURE_ROOT/hang_in_run.bats" --tap  >"$TEMPFILE" 2>&1 & # don't block execution, or we cannot send signals

  SUBPROCESS_PID=$!
  
  single-use-latch::wait hang_in_run 1 10

  # emulate CTRL-C by sending SIGINT to the whole process group
  kill -SIGINT -- -$SUBPROCESS_PID || (cat "$TEMPFILE"; false)

  # the test suite must be marked as failed!
  wait $SUBPROCESS_PID && return 1

  run cat "$TEMPFILE"
  
  [ "${lines[1]}" == "not ok 1 test" ]
  # due to scheduling the exact line will vary but we should exit with 130
  [[ "${lines[3]}" == *"failed with status 130" ]] || false
  [ "${lines[4]}" == "# Received SIGINT, aborting ..." ]
}

@test "CTRL-C aborts and fails after run" {
  if [[ "$BATS_NUMBER_OF_PARALLEL_JOBS" -gt 1 ]]; then
    skip "Aborts don't work in parallel mode"
  fi

  # shellcheck disable=SC2031,2030
  export TEMPFILE="$BATS_TEST_TMPDIR/$BATS_TEST_NAME.log"

  # guarantee that background processes get their own process group -> pid=pgid
  set -m
  
  load 'concurrent-coordination'
  # shellcheck disable=SC2031,SC2030
  export SINGLE_USE_LATCH_DIR="${BATS_SUITE_TMPDIR}"
  # we cannot use run for a background task, so we have to store the output for later
  bats "$FIXTURE_ROOT/hang_after_run.bats" --tap  >"$TEMPFILE" 2>&1 & # don't block execution, or we cannot send signals

  SUBPROCESS_PID=$!
  
  single-use-latch::wait hang_after_run 1 10

  # emulate CTRL-C by sending SIGINT to the whole process group
  kill -SIGINT -- -$SUBPROCESS_PID || (cat "$TEMPFILE"; false)

  # the test suite must be marked as failed!
  wait $SUBPROCESS_PID && return 1

  run cat "$TEMPFILE"
  
  [ "${lines[1]}" == "not ok 1 test" ]
  # due to scheduling the exact line will vary but we should exit with 130
  [[ "${lines[3]}" == *"failed with status 130" ]] || false
  [ "${lines[4]}" == "# Received SIGINT, aborting ..." ]
}

@test "CTRL-C aborts and fails the current teardown" {
  if [[ "$BATS_NUMBER_OF_PARALLEL_JOBS" -gt 1 ]]; then
    skip "Aborts don't work in parallel mode"
  fi

  # shellcheck disable=SC2031,SC2030
  export TEMPFILE="$BATS_TEST_TMPDIR/$BATS_TEST_NAME.log"

  # guarantee that background processes get their own process group -> pid=pgid
  set -m
  
  load 'concurrent-coordination'
  # shellcheck disable=SC2031,SC2030
  export SINGLE_USE_LATCH_DIR="${BATS_SUITE_TMPDIR}"
  # we cannot use run for a background task, so we have to store the output for later
  bats "$FIXTURE_ROOT/hang_in_teardown.bats" --tap  >"$TEMPFILE" 2>&1 & # don't block execution, or we cannot send signals

  SUBPROCESS_PID=$!
  
  single-use-latch::wait hang_in_teardown 1 10

  # emulate CTRL-C by sending SIGINT to the whole process group
  kill -SIGINT -- -$SUBPROCESS_PID || (cat "$TEMPFILE"; false)

  # the test suite must be marked as failed!
  wait $SUBPROCESS_PID && return 1

  run cat "$TEMPFILE"
  echo "$output"

  [ "${lines[1]}" == "not ok 1 empty" ]
  # due to scheduling the exact line will vary but we should exit with 130
  [[ "${lines[3]}" == *"failed with status 130" ]] || false
  [ "${lines[4]}" == "# Received SIGINT, aborting ..." ]
}

@test "CTRL-C aborts and fails the current setup_file" {
  if [[ "$BATS_NUMBER_OF_PARALLEL_JOBS" -gt 1 ]]; then
    skip "Aborts don't work in parallel mode"
  fi

  # shellcheck disable=SC2031,SC2030
  export TEMPFILE="$BATS_TEST_TMPDIR/$BATS_TEST_NAME.log"

  # guarantee that background processes get their own process group -> pid=pgid
  set -m
  
  load 'concurrent-coordination'
  # shellcheck disable=SC2031,SC2030
  export SINGLE_USE_LATCH_DIR="${BATS_SUITE_TMPDIR}"
  # we cannot use run for a background task, so we have to store the output for later
  bats "$FIXTURE_ROOT/hang_in_setup_file.bats" --tap  >"$TEMPFILE" 2>&1 & # don't block execution, or we cannot send signals

  SUBPROCESS_PID=$!
  
  single-use-latch::wait hang_in_setup_file 1 10

  # emulate CTRL-C by sending SIGINT to the whole process group
  kill -SIGINT -- -$SUBPROCESS_PID || (cat "$TEMPFILE"; false)

  # the test suite must be marked as failed!
  wait $SUBPROCESS_PID && return 1

  run cat "$TEMPFILE"
  echo "$output"

  [ "${lines[1]}" == "not ok 1 setup_file failed" ]
  # due to scheduling the exact line will vary but we should exit with 130
  [[ "${lines[3]}" == *"failed with status 130" ]] || false
  [ "${lines[4]}" == "# Received SIGINT, aborting ..." ]
}

@test "CTRL-C aborts and fails the current teardown_file" {
  if [[ "$BATS_NUMBER_OF_PARALLEL_JOBS" -gt 1 ]]; then
    skip "Aborts don't work in parallel mode"
  fi
  # shellcheck disable=SC2031
  export TEMPFILE="${BATS_TEST_TMPDIR}/$BATS_TEST_NAME.log"

  # guarantee that background processes get their own process group -> pid=pgid
  set -m
  
  load 'concurrent-coordination'
  # shellcheck disable=SC2031
  export SINGLE_USE_LATCH_DIR="${BATS_SUITE_TMPDIR}"
  # we cannot use run for a background task, so we have to store the output for later
  bats "$FIXTURE_ROOT/hang_in_teardown_file.bats" --tap  >"$TEMPFILE" 2>&1 & # don't block execution, or we cannot send signals

  SUBPROCESS_PID=$!
  
  single-use-latch::wait hang_in_teardown_file 1 10

  # emulate CTRL-C by sending SIGINT to the whole process group
  kill -SIGINT -- -$SUBPROCESS_PID || (cat "$TEMPFILE"; false)

  # the test suite must be marked as failed!
  wait $SUBPROCESS_PID && return 1

  run cat "$TEMPFILE"
  echo "$output"

  [ "${lines[0]}" == "1..1" ]
  [ "${lines[1]}" == "ok 1 empty" ]
  [ "${lines[2]}" == "not ok 2 teardown_file failed" ]
  # due to scheduling the exact line will vary but we should exit with 130
  [[ "${lines[4]}" == *"failed with status 130" ]] || false
  [ "${lines[5]}" == "# Received SIGINT, aborting ..." ]
  [ "${lines[6]}" == "# bats warning: Executed 2 instead of expected 1 tests" ]
}


@test "single star in output is not treated as a glob" {
  star(){ echo '*'; }
  
  run star
  [ "${lines[0]}" = '*' ]
}

@test "multiple stars in output are not treated as a glob" {
  stars(){ echo '**'; }
  
  run stars
  [ "${lines[0]}" = '**' ]
}

@test "ensure all folders are shellchecked" {
  if [[ ! -f "$BATS_ROOT/shellcheck.sh" ]]; then
    skip "\$BATS_ROOT/shellcheck.sh is required for this test"
  fi
  cd "$BATS_ROOT"
  run "./shellcheck.sh" --list
  echo "$output"

  grep bin/bats <<< "$output"
  grep contrib/ <<< "$output"
  grep docker/ <<< "$output"
  grep lib/bats-core/ <<< "$output"
  grep libexec/bats-core/ <<< "$output"
  grep test/fixtures <<< "$output"
  grep install.sh <<< "$output"
}

@test "BATS_RUN_COMMAND: test content of variable" {
  run bats -v
  [[ "${BATS_RUN_COMMAND}" == "bats -v" ]]
  run bats "${BATS_TEST_DESCRIPTION}"
  echo "$BATS_RUN_COMMAND"
  [[ "$BATS_RUN_COMMAND" == "bats BATS_RUN_COMMAND: test content of variable" ]]
}

@test "pretty formatter summary is colorized red on failure" {
  bats_require_minimum_version 1.5.0
  run -1 bats --pretty "$FIXTURE_ROOT/failing.bats"
  
  [ "${lines[4]}" == $'\033[0m\033[31;1m' ] # TODO: avoid checking for the leading reset too
  [ "${lines[5]}" == '1 test, 1 failure' ]
  [ "${lines[6]}" == $'\033[0m' ]
}

@test "pretty formatter summary is colorized green on success" {
  bats_require_minimum_version 1.5.0
  run -0 bats --pretty "$FIXTURE_ROOT/passing.bats"

  [ "${lines[2]}" == $'\033[0m\033[32;1m' ] # TODO: avoid checking for the leading reset too
  [ "${lines[3]}" == '1 test, 0 failures' ]
  [ "${lines[4]}" == $'\033[0m' ]
}

@test "--print-output-on-failure works as expected" {
  run bats --print-output-on-failure --show-output-of-passing-tests "$FIXTURE_ROOT/print_output_on_failure.bats"
  [ "${lines[0]}" == '1..3' ]
  [ "${lines[1]}" == 'ok 1 no failure prints no output' ]
  # ^ no output despite --show-output-of-passing-tests, because there is no failure
  [ "${lines[2]}" == 'not ok 2 failure prints output' ]
  [ "${lines[3]}" == "# (in test file $RELATIVE_FIXTURE_ROOT/print_output_on_failure.bats, line 6)" ]
  [ "${lines[4]}" == "#   \`run -1 echo \"fail hard\"' failed, expected exit code 1, got 0" ]
  [ "${lines[5]}" == '# Last output:' ]
  [ "${lines[6]}" == '# fail hard' ]
  [ "${lines[7]}" == 'not ok 3 empty output on failure' ]
  [ "${lines[8]}" == "# (in test file $RELATIVE_FIXTURE_ROOT/print_output_on_failure.bats, line 10)" ]
  [ "${lines[9]}" == "#   \`false' failed" ]
  [ ${#lines[@]} -eq 10 ]
}

@test "--show-output-of-passing-tests works as expected" {
  bats_require_minimum_version 1.5.0
  run -0 bats --show-output-of-passing-tests "$FIXTURE_ROOT/show-output-of-passing-tests.bats"
  [ "${lines[0]}" == '1..1' ]
  [ "${lines[1]}" == 'ok 1 test' ]
  [ "${lines[2]}" == '# output' ]
  [ ${#lines[@]} -eq 3 ]
}

@test "--verbose-run prints output" {
  bats_require_minimum_version 1.5.0
  run -1 bats --verbose-run "$FIXTURE_ROOT/verbose-run.bats"
  [ "${lines[0]}" == '1..1' ]
  [ "${lines[1]}" == 'not ok 1 test' ]
  [ "${lines[2]}" == "# (in test file $RELATIVE_FIXTURE_ROOT/verbose-run.bats, line 3)" ]
  [ "${lines[3]}" == "#   \`run ! echo test' failed, expected nonzero exit code!" ]
  [ "${lines[4]}" == '# test' ]
  [ ${#lines[@]} -eq 5 ]
}

@test "BATS_VERBOSE_RUN=1 also prints output" {
  bats_require_minimum_version 1.5.0
  run -1 env BATS_VERBOSE_RUN=1 bats "$FIXTURE_ROOT/verbose-run.bats"
  [ "${lines[0]}" == '1..1' ]
  [ "${lines[1]}" == 'not ok 1 test' ]
  [ "${lines[2]}" == "# (in test file $RELATIVE_FIXTURE_ROOT/verbose-run.bats, line 3)" ]
  [ "${lines[3]}" == "#   \`run ! echo test' failed, expected nonzero exit code!" ]
  [ "${lines[4]}" == '# test' ]
  [ ${#lines[@]} -eq 5 ]
}

@test "--gather-test-outputs-in gathers outputs of all tests (even succeeding!)" {
  local OUTPUT_DIR="$BATS_TEST_TMPDIR/logs"
  run bats --verbose-run --gather-test-outputs-in "$OUTPUT_DIR" "$FIXTURE_ROOT/print_output_on_failure.bats"

  [ -d "$OUTPUT_DIR" ] # will be generated!

  # even outputs of successful tests are generated
  OUTPUT=$(<"$OUTPUT_DIR/1-no failure prints no output.log") # own line to trigger failure if file does not exist
  [ "$OUTPUT" ==  "success" ]
  
  OUTPUT=$(<"$OUTPUT_DIR/2-failure prints output.log")
  [ "$OUTPUT" == "fail hard" ]

  # even empty outputs are generated
  OUTPUT=$(<"$OUTPUT_DIR/3-empty output on failure.log")
  [ "$OUTPUT" == "" ]

  [ "$(find "$OUTPUT_DIR" -type f | wc -l)" -eq 3 ]
}

@test "Tell about missing flock and shlock" {
  if ! command -v parallel; then
    skip "this test requires GNU parallel to be installed"
  fi
  if command -v flock; then
    skip "this test requires flock not to be installed"
  fi
  if command -v shlock; then
    skip "this test requires flock not to be installed"
  fi

  bats_require_minimum_version 1.5.0
  run ! bats --jobs 2 "$FIXTURE_ROOT/parallel.bats"
  [ "${lines[0]}" == "ERROR: flock/shlock is required for parallelization within files!" ]
  [ "${#lines[@]}" -eq 1 ]
}

@test "Test with a name that is waaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaay too long" {
  skip "This test should only check if the long name chokes bats' internals during execution"
}

@test "BATS_CODE_QUOTE_STYLE works with any two characters (even unicode)" {
  bats_require_minimum_version 1.5.0
  BATS_CODE_QUOTE_STYLE='``' run -1 bats --tap "${FIXTURE_ROOT}/failing.bats"
  # shellcheck disable=SC2016
  [ "${lines[3]}" == '#   `eval "( exit ${STATUS:-1} )"` failed' ]

  
  export BATS_CODE_QUOTE_STYLE='😁😂'
  if [[ ${#BATS_CODE_QUOTE_STYLE} -ne 2 ]]; then
    # for example, this happens on windows!
    skip 'Unicode chars are not counted as one char in this system'
  fi

  bats_require_minimum_version 1.5.0
  run -1 bats --tap "${FIXTURE_ROOT}/failing.bats"
  # shellcheck disable=SC2016
  [ "${lines[3]}" == '#   😁eval "( exit ${STATUS:-1} )"😂 failed' ]
}

@test "BATS_CODE_QUOTE_STYLE=custom requires BATS_CODE_QUOTE_BEGIN/END" {
  # unset because they are set in the surrounding scope
  unset BATS_BEGIN_CODE_QUOTE BATS_END_CODE_QUOTE

  bats_require_minimum_version 1.5.0

  BATS_CODE_QUOTE_STYLE=custom run -1 bats --tap "${FIXTURE_ROOT}/passing.bats"
  [ "${lines[0]}" == 'ERROR: BATS_CODE_QUOTE_STYLE=custom requires BATS_BEGIN_CODE_QUOTE and BATS_END_CODE_QUOTE to be set' ]

  # shellcheck disable=SC2016
  BATS_CODE_QUOTE_STYLE=custom \
  BATS_BEGIN_CODE_QUOTE='$(' \
  BATS_END_CODE_QUOTE=')' \
    run -1 bats --tap "${FIXTURE_ROOT}/failing.bats"
  # shellcheck disable=SC2016
  [ "${lines[3]}" == '#   $(eval "( exit ${STATUS:-1} )") failed' ]
}

@test "Warn about invalid BATS_CODE_QUOTE_STYLE" {
  bats_require_minimum_version 1.5.0
  BATS_CODE_QUOTE_STYLE='' run -1 bats --tap "${FIXTURE_ROOT}/passing.bats"
  [ "${lines[0]}" == 'ERROR: Unknown BATS_CODE_QUOTE_STYLE: ' ]

  BATS_CODE_QUOTE_STYLE='1' run -1 bats --tap "${FIXTURE_ROOT}/passing.bats"
  [ "${lines[0]}" == 'ERROR: Unknown BATS_CODE_QUOTE_STYLE: 1' ]

  BATS_CODE_QUOTE_STYLE='three' run -1 bats --tap "${FIXTURE_ROOT}/passing.bats"
  [ "${lines[0]}" == 'ERROR: Unknown BATS_CODE_QUOTE_STYLE: three' ]
}

@test "Debug trap must only override variables that are prefixed with bats_ (issue #519)" {
  # use declare -p to gather variables in pristine bash and bats @test environment
  # then compare which ones are introduced in @test compared to bash

  # make declare's output more readable and suitable for `comm`
  if [[ "${BASH_VERSINFO[0]}" -eq 3 ]]; then
    normalize_variable_list() {
      # `declare -p`: VAR_NAME="VALUE"
      # will also contain function definitions!
      while read -r line; do
        # Skip variable assignments in function definitions!
        # (They will be indented.)
        declare_regex='^declare -[^[:space:]]+ ([^=]+)='
        plain_regex='^([^=[:space]]+)='
        if [[ $line =~ $declare_regex ]]; then
          printf "%s\n" "${BASH_REMATCH[1]}"
        elif [[ $line =~ $plain_regex ]]; then
          printf "%s\n" "${BASH_REMATCH[1]}"
        fi
      done | sort
    }
  else
    normalize_variable_list() {
      # `declare -p`: declare -X VAR_NAME="VALUE"
      while IFS=' =' read -r _declare _ variable _; do
          if [[ "$_declare" == declare ]]; then # skip multiline variables' values
            printf "%s\n" "$variable"
          fi
      done | sort
    }
  fi

  # get the bash baseline
  # add variables that should be ignored like PIPESTATUS here
  BASH_DECLARED_VARIABLES=$(env -i PIPESTATUS= "$BASH" -c "declare -p")
  local BATS_DECLARED_VARIABLES_FILE="${BATS_TEST_TMPDIR}/variables.log"
  bats_require_minimum_version 1.5.0
  # now capture bats @test environment
  run -0 env -i PATH="$PATH" BATS_DECLARED_VARIABLES_FILE="$BATS_DECLARED_VARIABLES_FILE"  bash "${BATS_ROOT}/bin/bats" "${FIXTURE_ROOT}/issue-519.bats"
  # use function to allow failing via !, run is a bit unwiedly with the pipe and subshells
  check_no_new_variables() {
    # -23 -> only look at additions on the bats list
    ! comm -23 <(normalize_variable_list <"$BATS_DECLARED_VARIABLES_FILE") \
               <(normalize_variable_list <<< "$BASH_DECLARED_VARIABLES" ) \
               | grep -v '^BATS_' # variables that are prefixed with BATS_ don't count
  }
  check_no_new_variables
}

@test "Don't wait for disowned background jobs to finish because of open FDs (#205)" {
    SECONDS=0
    export LOG_FILE="$BATS_TEST_TMPDIR/fds.log"
    bats_require_minimum_version 1.5.0
    run -0 bats --show-output-of-passing-tests --tap "${FIXTURE_ROOT}/issue-205.bats"
    echo "Whole suite took: $SECONDS seconds"
    FDS_LOG=$(<"$LOG_FILE")
    echo "$FDS_LOG"
    [ $SECONDS -lt 10 ]
    [[ $FDS_LOG == *'otherfunc fds after: (0 1 2)'* ]] || false
    [[ $FDS_LOG == *'setup_file fds after: (0 1 2)'* ]] || false
}

@test "Allow for prefixing tests' names with BATS_TEST_NAME_PREFIX" {
  BATS_TEST_NAME_PREFIX='PREFIX: ' run bats "${FIXTURE_ROOT}/passing.bats"
  [ "${lines[1]}" == "ok 1 PREFIX: a passing test" ]
}

@test "Setting status in teardown* does not override exit code (see issue #575)" {
  bats_require_minimum_version 1.5.0
  TEARDOWN_RETURN_CODE=0 TEST_RETURN_CODE=0 STATUS=0 run -0 bats "$FIXTURE_ROOT/teardown_override_status.bats"
  TEARDOWN_RETURN_CODE=1 TEST_RETURN_CODE=0 STATUS=0 run -1 bats "$FIXTURE_ROOT/teardown_override_status.bats"
  TEARDOWN_RETURN_CODE=0 TEST_RETURN_CODE=1 STATUS=0 run -1 bats "$FIXTURE_ROOT/teardown_override_status.bats"
  TEARDOWN_RETURN_CODE=1 TEST_RETURN_CODE=1 STATUS=0 run -1 bats "$FIXTURE_ROOT/teardown_override_status.bats"
  TEARDOWN_RETURN_CODE=0 TEST_RETURN_CODE=0 STATUS=1 run -0 bats "$FIXTURE_ROOT/teardown_override_status.bats"
  TEARDOWN_RETURN_CODE=1 TEST_RETURN_CODE=0 STATUS=1 run -1 bats "$FIXTURE_ROOT/teardown_override_status.bats"

  TEARDOWN_RETURN_CODE=0 TEST_RETURN_CODE=0 STATUS=0 run -0 bats "$FIXTURE_ROOT/teardown_file_override_status.bats"
  TEARDOWN_RETURN_CODE=1 TEST_RETURN_CODE=0 STATUS=0 run -1 bats "$FIXTURE_ROOT/teardown_file_override_status.bats"
  TEARDOWN_RETURN_CODE=0 TEST_RETURN_CODE=1 STATUS=0 run -1 bats "$FIXTURE_ROOT/teardown_file_override_status.bats"
  TEARDOWN_RETURN_CODE=1 TEST_RETURN_CODE=1 STATUS=0 run -1 bats "$FIXTURE_ROOT/teardown_file_override_status.bats"
  TEARDOWN_RETURN_CODE=0 TEST_RETURN_CODE=0 STATUS=1 run -0 bats "$FIXTURE_ROOT/teardown_file_override_status.bats"
  TEARDOWN_RETURN_CODE=1 TEST_RETURN_CODE=0 STATUS=1 run -1 bats "$FIXTURE_ROOT/teardown_file_override_status.bats"

  TEARDOWN_RETURN_CODE=0 TEST_RETURN_CODE=0 STATUS=0 run -0 bats "$FIXTURE_ROOT/teardown_suite_override_status/"
  TEARDOWN_RETURN_CODE=1 TEST_RETURN_CODE=0 STATUS=0 run -1 bats "$FIXTURE_ROOT/teardown_suite_override_status/"
  TEARDOWN_RETURN_CODE=0 TEST_RETURN_CODE=1 STATUS=0 run -1 bats "$FIXTURE_ROOT/teardown_suite_override_status/"
  TEARDOWN_RETURN_CODE=1 TEST_RETURN_CODE=1 STATUS=0 run -1 bats "$FIXTURE_ROOT/teardown_suite_override_status/"
  TEARDOWN_RETURN_CODE=0 TEST_RETURN_CODE=0 STATUS=1 run -0 bats "$FIXTURE_ROOT/teardown_suite_override_status/"
  TEARDOWN_RETURN_CODE=1 TEST_RETURN_CODE=0 STATUS=1 run -1 bats "$FIXTURE_ROOT/teardown_suite_override_status/"
}