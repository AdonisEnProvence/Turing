# Turing

## What is a Turing machine ?

> A Turing machine is a mathematical model of computation describing an abstract machine that manipulates symbols on a strip of tape according to a table of rules. Despite the model's simplicity, it is capable of implementing any computer algorithm. [Wikipedia](https://en.wikipedia.org/wiki/Turing_machine)

We then have a tape, where is injected the machine input.
A head that will read every tape element interpreting them base the defined machine configuration.

## The visualizer

GIF HERE

[Open the visualizer →](https://turing.adonisenprovence.com/)

The visualizer will request machine execution to our server, whom will send back the executed machine.
The client will interpret the resulting data and display it using human understandable animations.
From the client machine player you can find several controls:

- Choose execution speed
- Pause/Play
- Enter manual mode
- Reset

But also edit the input and machine configuration from the related text fields.
The client will notify the user about any machine nor input configuration error.

## The command line

You can execute Turing machine in local using the CLI.
Such as:

```
_build/default/bin/turing run our-machines/02n.json "000" --color
Interpreter starting...
[<0>00] (one, 0) -> (two, 0, right)
[0<0>0] (two, 0) -> (one, 0, right)
[00<0>] (one, 0) -> (two, 0, right)
[000<.>] (two, .) -> (HALT, n, left)
[00<0>n] Final state reached !
Interpreter closing...
```

where `02n.json` contains:

```json
{
  "name": "02n",
  "alphabet": ["0", ".", "y", "n"],
  "blank": ".",
  "states": ["one", "two", "HALT"],
  "initial": "one",
  "finals": ["HALT"],
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
}
```

The CLI is called using the command `RUN`.
For big machine you might need to enable color mode.
To do so you can use the following flag:

- `--color` or `-c` will enable color mode.

![Turing machine execution with color flag enabled ](docs/execute-machine-with-color.png)

## Universal Turing Machine

> In computer science, a universal Turing machine (UTM) is a Turing machine that simulates an arbitrary Turing machine on arbitrary input.
> [Wikipedia](https://en.wikipedia.org/wiki/Universal_Turing_machine)

To say that we have to implement a machine configuration that allow the input to describe its own transitions and states.

### The input

An utm input will be composed as follows:  
`Initial-state~State_1{[Read To_State Action Write][Read To_State Action Write]...}...&Input`  
Where `{}` are state definition closure and `[]` are state transitions closure
`~` the initial state separator and `&` the input beginning

Example:

```bash
./turing utm-config.json "S~S{[0S>_][1H<1]}&001"
```

### An Universal Turing Machine config

The UTM configuration must store by its definition the possible transitions that would be allowed inside the input.
For example, you can find the UTM that allows to run the above `02n.json` here:

[our-machines/02n_utm.json →](/our-machines/02n_utm.json)

Finally, you can run the `02n` UTM machine as following:

```bash
./_build/default/bin/turing our-machines/02n_utm.json "E~E{[0P>0][_H<y]}P{[0E>0][_H<n]}&00"
Interpreter starting...
# ...
[E~E{[0P>0][_H<y]}P{[0E>0][_H<n]}&00<^>] (execute-transition-H_<_y, ^) -> (HALT, y, left)
[E~E{[0P>0][_H<y]}P{[0E>0][_H<n]}&0<0>y] Final state reached !
Interpreter closing...
```

As building UTM configuration is a pain, we've implemented tools that allow the user to generate any kind of UTM he needs.  
The UTM generator script requires the following input to be building the `02n_utm.json`

```json
{
  "inputCharacters": ["0", "y", "n"],
  "states": ["E", "P"]
}
```

## Docs

You can find more details about running the project in local and the technical details in files below:

[See technical details →](docs/technical.md)

[See Setup →](docs/setup.md)
