# Technical details

## The stack

The cli and server are built using [erlang](https://www.erlang.org/).  
We're working on a [cowboy](https://ninenines.eu/) server.

Inside the client we're using:

- [vue](https://vuejs.org/)
- [xstate](https://xstate.js.org/docs/)
- [tailwindcss](https://tailwindcss.com/)
- [ace](https://ace.c9.io/)

## Universal Turing Machine tools

### Input translator script

This script is located [/universal_turing_machine_generator/index.js](/universal_turing_machine_generator/index.js)
To manually translate a classic machine config into an utm input can be quite fastidious.
The script then takes a classic machine config json file as only argument:

```bash
node universal_turing_machine_generator/machine_to_input_declaration.js our-machines/02n.json
# "one~one{[0two>0][_HALT<y]}two{[0one>0][_HALT<n]}&YOUR_INPUT"
```

_Important note:_ the script does neither parse nor validate the arg file. We recommend passing a working classic machine config.

### Utm generator script

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

```txt
./_build/default/bin/turing run our-machines/02n_utm.json "E~E{[0P>0][_H<y]}P{[0E>0][_H<n]}&00"
Interpreter starting...
[<E>~E{[0P>0][_H<y]}P{[0E>0][_H<n]}&00] (retrieve_initial_state, E) -> (go-to-input-start-for_E, E, right)
[E<~>E{[0P>0][_H<y]}P{[0E>0][_H<n]}&00] (go-to-input-start-for_E, ~) -> (go-to-input-start-for_E, ~, right)
...
[E~E{[0P>0][_H<y]}P{[0E>0][_H<n]}&00<^>] (execute-transition-H_<_y, ^) -> (HALT, y, left)
[E~E{[0P>0][_H<y]}P{[0E>0][_H<n]}&0<0>y] Final state reached !
Interpreter closing...
```

### Unauthorized characters:

- inputCharacters

Must not contain any `_`, that is the blank input alias used inside the input states and transitions definition. And the `.` being the default blank character.

_Important note:_ the script does neither parse nor validate the generator arg file as the process result will be parsed by the turing machine itself.
Just to say you can generate invalid machine config such as duplicated alphabet characters. It's just a tool.

- States

Every `states` names must be chars. Beware of `inputCharacters` and `states` conflicts.
The `H` state is used as default final state `"HALT"`
