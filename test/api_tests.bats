setup() {
    load 'test_helper/bats-support/load'
    load 'test_helper/bats-assert/load'
    # ... the remaining setup is unchanged
    source "start_server"

    # get the containing directory of this file
    # use $BATS_TEST_FILENAME instead of ${BASH_SOURCE[0]} or $0,
    # as those will point to the bats executable's location or the preprocessed file respectively
    DIR="$( cd "$( dirname "$BATS_TEST_FILENAME" )" >/dev/null 2>&1 && pwd )"
    # make executables in src/ visible to PATH
    PATH="$DIR/../src:$PATH"
}

teardown() {
    source "kill_server"
}

@test "Fail on empty input" {
    run zsh -c 'curl --header "Content-Type: application/json" \
  --request POST \
  --data `{"machineConfig": {
    "name": "02n",
    "alphabet": [
        "0",
        ".",
        "y",
        "n"
    ],
    "blank": ".",
    "states": ["one", "two", "HALT"],
    "initial": "one",
    "finals": [
        "HALT"
    ],
    "transitions": {
        "one": [
            { "read": "0", "write": "0", "to_state": "two", "action": "RIGHT" },
            { "read": ".", "write": "y", "to_state": "HALT", "action": "LEFT" }
        ],
        "two": [
            { "read": "0", "write": "0", "to_state": "one", "action": "RIGHT" },
            { "read": ".", "write": "n", "to_state": "HALT", "action": "LEFT" }
        ]
    }
}, "input":"000"}` \
  http://localhost:8080/execute-machine'
    assert_output 'Salut'
}
