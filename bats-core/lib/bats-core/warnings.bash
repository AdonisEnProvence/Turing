# shellcheck source=lib/bats-core/tracing.bash
source "$BATS_ROOT/lib/bats-core/tracing.bash"

BATS_WARNING_SHORT_DESCS=(
  # to start with 1
  'PADDING'
  # see issue #578 for context
  "\`run\`'s command \`%s\` exited with code 127, indicating 'Command not found'. Use run's return code checks, e.g. \`run -127\`, to fix this message."
  "%s requires at least BATS_VERSION=%s. Use \`bats_require_minimum_version %s\` to fix this message."
)

# generate a warning report for the parent call's call site
bats_generate_warning() { # <warning number> [<printf args for warning string>...]
  local warning_number="$1" padding="00"
  shift
  if [[ $warning_number =~ [0-9]+ ]] && ((warning_number < ${#BATS_WARNING_SHORT_DESCS[@]} )); then
    {
        printf "BW%s: ${BATS_WARNING_SHORT_DESCS[$warning_number]}\n" "${padding:${#warning_number}}${warning_number}" "$@"
        bats_capture_stack_trace
        BATS_STACK_TRACE_PREFIX='      ' bats_print_stack_trace "${BATS_DEBUG_LAST_STACK_TRACE[@]}"
    } >> "$BATS_WARNING_FILE" 2>&3
  else
    printf "Invalid Bats warning number '%s'. It must be an integer between 1 and %d." "$warning_number" "$((${#BATS_WARNING_SHORT_DESCS[@]} - 1))" >&2
    exit 1
  fi
}

# generate a warning if the BATS_GUARANTEED_MINIMUM_VERSION is not high enough
bats_warn_minimum_guaranteed_version() { # <feature> <minimum required version>
  if bats_version_lt "$BATS_GUARANTEED_MINIMUM_VERSION" "$2"; then
    bats_generate_warning 2 "$1" "$2" "$2"
  fi
}