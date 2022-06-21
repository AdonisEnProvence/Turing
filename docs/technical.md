# Technical details

## The stack

CLI and server are built using [Erlang](https://www.erlang.org/).

We're working on a [Cowboy](https://ninenines.eu/) server.

Inside the client we're using:

- [Vue.js](https://vuejs.org/)
- [XState](https://xstate.js.org/docs/)
- [Tailwind CSS](https://tailwindcss.com/)
- [Ace](https://ace.c9.io/)

## Universal Turing Machine tools

### Input translator script

Manually translating a classic machine config into an UTM input can be quite fastidious. We wrote a script to simplify the process.

[See script →](/universal_turing_machine_generator/machine_to_input_declaration.js)

The script takes a classic machine configuration as argument:

```bash
node universal_turing_machine_generator/machine_to_input_declaration.js our-machines/02n.json
# "one~one{[0two>0][_HALT<y]}two{[0one>0][_HALT<n]}&YOUR_INPUT"
```

_Important note:_ the script does neither parse nor validate the machine configuration. We recommend passing a working machine configuration.

### UTM generator script

Building an UTM configuration is really time-consuming and hardly maintainable. We also wrote a script to simplify the process.

[See script →](/universal_turing_machine_generator/index.js)

The script takes a json file as only argument, which must contain the following data set:

```json
{
  "inputCharacters": ["0", "y", "n"],
  "states": ["E", "P"]
}
```

The script builds states for each possible `inputCharacters` and `states` combination but also for skipping to next input transitions or states declaration etc.  

[See input example →](/universal_turing_machine_generator/generate_unary_add_utm_machine_config.json)

Run example:

```bash
node universal_turing_machine_generator/index.js universal_turing_machine_generator/generate_02n_utm_machine_config.json
# creates ./data.json
```

_Important note:_ the script does neither parse nor validate the machine configuration. We recommend passing a working machine configuration.

Finally, you can run an 02n UTM machine as following:

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

### Unauthorized characters

- inputCharacters

  Must not contain any `_` character as it's the blank input alias used inside the input states and transitions definition. `.` character is forbidden too as it's the default blank character.
- states

  Every `states` names must be strings made of a single character. Beware of `inputCharacters` and `states` conflicts.
  The `H` state is used as default final state `"HALT"`

_Important note:_ the script does neither parse nor validate the configuration file passed to generator as the process result will be parsed by the turing machine itself.
