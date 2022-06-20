# turing

An escript

## Build

    $ rebar3 escriptize

## Run

    $ _build/default/bin/turing

## Test

    $ rebar3 eunit

### Generate code coverage reports

    $ rebar3 cover --verbose

You can visualize code coverage, and which lines of code are not tested, by opening `_build/test/cover/index.html`.

## Dockerfile

Follow those steps to launch a local Docker container with Erlang installed.

```sh
# Build an image defined in the `Dockerfile` of `.` directory and name it `turing`.

docker build -f Dockerfile.local -t turing .

# Run the image named `turing`, attach local directory to /turing directory in the container, thanks to volumes.
# If we change files on our computer, the changes will be reflected in the container.
# Run the container in detached mode so that we can access the container with a shell when we want.
docker run -d --name turing -v $PWD:/turing turing

# Launch a shell inside the container.
# We will write the same commands as if we were developing directly on our machine.
docker exec -it turing bash

# Remove container
docker rm -f turing
```

## Bats shell tests

To run shell tests you have to be inside the docker container
Run

```sh
./bats-core/bin/bats test/
```

## Universal Turing Machine

<< In computer science, a universal Turing machine (UTM) is a Turing machine that simulates an arbitrary Turing machine on arbitrary input. >>
[Wikipedia](https://en.wikipedia.org/wiki/Universal_Turing_machine)

### The input

An utm input will be composed as follows:
`Initial-state~State_1{[Read To_State Action Write][Read To_State Action Write]...}...&Input`  
Where `{}` are state definition closure and `[]` are state transitions closure
`~` the initial state separator and `&` the input beginning

Example:

```bash
./turing utm-config.json "S~S{[0S>_][1H<1]}&001"
```

#### Input translator script

This script is located [/universal_turing_machine_generator/index.js](/universal_turing_machine_generator/index.js)
To manually translate a classic machine config into an utm input can be quite fastidious.
The script then takes a classic machine config json file as only argument:

```bash
node universal_turing_machine_generator/machine_to_input_declaration.js our-machines/02n.json
# "one~one{[0two>0][_HALT<y]}two{[0one>0][_HALT<n]}&YOUR_INPUT"
```

_Important note:_ the script does neither parse nor validate the arg file. We recommend passing a working classic machine config.

### The machine config

#### Utm generator script

The script is located [here](/universal_turing_machine_generator/index.js).
The script takes a json file as only argument, this json file must contain the following data set:

```json
{
  "inputCharacters": ["0", "y", "n"],
  "states": ["E", "P"]
}
```

The script build states for each possible `inputCharacters` and `states` combination but also for skipping to next inside input transitions input nor states declaration etc.  
See [generate_unary_add_machine_config.json](/universal_turing_machine_generator/generate_unary_add_utm_machine_config.json)

Run example

```bash
node universal_turing_machine_generator/index.js universal_turing_machine_generator/generate_02n_utm_machine_config.json
# creates ./data.json
```

Finally, you can run an 02n utm machine as following:

```bash
./_build/default/bin/turing our-machines/02n_utm.json "E~E{[0P>0][_H<y]}P{[0E>0][_H<n]}&00"
Interpreter starting...
# ...
[E~E{[0P>0][_H<y]}P{[0E>0][_H<n]}&00<^>] (execute-transition-H_<_y, ^) -> (HALT, y, left)
[E~E{[0P>0][_H<y]}P{[0E>0][_H<n]}&0<0>y] Final state reached !
Interpreter closing...
```

#### Unauthorized characters:

- inputCharacters

Must not contain any `_`, that is the blank input alias used inside the input states and transitions definition. And the `.` being the default blank character.

_Important note:_ the script does neither parse nor validate the generator arg file as the process result will be parsed by the turing machine itself.
Just to say you can generate invalid machine config such as duplicated alphabet characters. It's just a tool.

- States

Every `states` names must be chars. Beware of `inputCharacters` and `states` conflicts.
The `H` state is used as default final state `"HALT"`
