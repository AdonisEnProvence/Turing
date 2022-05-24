-record(parsed_machine_config_transition, {
    read :: string(),
    to_state :: string(),
    write :: string(),
    action :: left | right
}).

-record(parsed_machine_config, {
    states :: list(string()),
    initial :: string(),
    transitions :: #{string() := list(#parsed_machine_config_transition{})}
}).
