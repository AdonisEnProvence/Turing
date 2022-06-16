setup() {
    ../_build/default/bin/turing serve &

    # Run tests only after the server starts listening to requests.
    # See https://stackoverflow.com/a/21189440
    until $(curl --output /dev/null --silent --head http://localhost:8080); do
        sleep 5
    done

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

teardown() {
    kill -9 $(lsof -t -i:8080)
}

@test "Success on valid machine config and input" {
    run zsh -c 'curl --header "Content-Type: application/json" \
  --request POST \
  http://localhost:8080/execute-machine \
  --data @- << EOF 2> /dev/null
{"machineConfig": {
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
}, "input":"000"}
EOF'
    assert_output '{"blank":".","tapeHistory":[{"currentState":"one","indexOnTape":1,"status":"continue","tape":["0","0","0"]},{"currentState":"two","indexOnTape":2,"status":"continue","tape":["0","0","0"]},{"currentState":"one","indexOnTape":3,"status":"continue","tape":["0","0","0"]},{"currentState":"two","indexOnTape":4,"status":"continue","tape":["0","0","0","."]},{"currentState":"HALT","indexOnTape":3,"status":"final","tape":["0","0","0","n"]}]}'
}


@test "Failure on machine config parser" {
    run zsh -c 'curl --header "Content-Type: application/json" \
  --request POST \
  http://localhost:8080/execute-machine \
  --data @- << EOF 2> /dev/null
{"machineConfig": {
 "name": "02n",
 "alphabet": [
     "0",
     ".",
     "y",
     "n"
 ],
 "blank": ".",
 "states": ["one", "two", "HALT"],
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
}, "input":"000"}
EOF'
    assert_output 'Error occured during machine configuration parsing:

machine has no initial state; a machine must have a non-empty initial state'
}


@test "Failure on machine config validation" {
    run zsh -c 'curl --header "Content-Type: application/json" \
  --request POST \
  http://localhost:8080/execute-machine \
  --data @- << EOF 2> /dev/null
{"machineConfig": {
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
         { "read": "1", "write": "0", "to_state": "two", "action": "RIGHT" },
         { "read": ".", "write": "y", "to_state": "HALT", "action": "LEFT" }
     ],
     "two": [
         { "read": "0", "write": "0", "to_state": "one", "action": "RIGHT" },
         { "read": ".", "write": "n", "to_state": "HALT", "action": "LEFT" }
     ]
 }
}, "input":"000"}
EOF'
    assert_output 'Error occured during machine configuration validation:

machine transition 0 of "one" has not alphabet character read operation target (received: 1); machine transitions must be scoped to a listed state, must only contain unique read character per transition and a listed to_state target'
}

@test "Failure halting problem infinite machine" {
    run zsh -c 'curl --header "Content-Type: application/json" \
  --request POST \
  http://localhost:8080/execute-machine \
  --data @- << EOF 2> /dev/null
{"machineConfig": {
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
         { "read": ".", "write": "y", "to_state": "HALT", "action": "RIGHT" }
     ],
     "two": [
         { "read": "0", "write": "0", "to_state": "one", "action": "RIGHT" },
         { "read": ".", "write": "n", "to_state": "two", "action": "RIGHT" }
     ]
 }
}, "input":"000"}
EOF'
    assert_output 'Machine too long to be executed, autokill.'
}

@test "Failure Body is invalid" {
    run zsh -c 'curl --header "Content-Type: application/json" \
  --request POST \
  http://localhost:8080/execute-machine \
  --data @- << EOF 2> /dev/null
{"machineConfig": {
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
         { "read": ".", "write": "y", "to_state": "HALT", "action": "RIGHT" }
     ],
     "two": [
         { "read": "0", "write": "0", "to_state": "one", "action": "RIGHT" },
         { "read": ".", "write": "n", "to_state": "two", "action": "RIGHT" }
     ]
 }
}}
EOF'
    assert_output 'Body is invalid'
}

@test "Failure input is invalid" {
    run zsh -c 'curl --header "Content-Type: application/json" \
  --request POST \
  http://localhost:8080/execute-machine \
  --data @- << EOF 2> /dev/null
{"machineConfig": {
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
         { "read": ".", "write": "y", "to_state": "HALT", "action": "RIGHT" }
     ],
     "two": [
         { "read": "0", "write": "0", "to_state": "one", "action": "RIGHT" },
         { "read": ".", "write": "n", "to_state": "two", "action": "RIGHT" }
     ]
 }
}, "input":"0001"}
EOF'
    assert_output 'Character "1" is not in the alphabet'
}