-record(parsed_machine_config_transition, {
    read :: string(),
    to_state :: string(),
    write :: string(),
    action :: left | right
}).

-record(parsed_machine_config, {
    name :: string(),
    alphabet :: list(string()),
    blank :: string(),
    states :: list(string()),
    initial :: string(),
    finals :: list(string()),
    transitions :: #{string() := list(#parsed_machine_config_transition{})}
}).

-record(program_options, {
    print_head_with_color :: boolean()
}).
