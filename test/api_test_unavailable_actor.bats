setup() {
    WORKER_NUMBER=0 ./_build/default/bin/turing serve &

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

@test "Failure on unavailable actor" {
    run bash -c 'curl --header "Content-Type: application/json" \
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

    assert_output '{"reason":"Could not find available actor.\n"}'
}
