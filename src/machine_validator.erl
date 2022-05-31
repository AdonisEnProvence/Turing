-module(machine_validator).

-ifdef(TEST).
-export([validate_machine_alphabet/1, validate_machine_states/1, validate_machine_blank/2]).
-else.
-export([]).
-endif.

% Reminder validation step is after parser step
% We then assume that the received data is already parsed

look_for_duplicated_in_list(List) ->
    ListLength = erlang:length(List),
    ListSet = sets:from_list(List),
    ListSetLength = sets:size(ListSet),
    ListContainsDuplicates = ListLength =/= ListSetLength,
    if
        ListContainsDuplicates =:= false ->
            ok;
        ListContainsDuplicates =:= true ->
            DuplicatedCharacterList = List -- sets:to_list(ListSet),
            {error, {duplicated_characters, DuplicatedCharacterList}}
    end.

is_alphabet_character(Character, Alphabet) ->
    CharacterIsInAlphabet = lists:member(Character, Alphabet),
    if
        CharacterIsInAlphabet =:= true -> ok;
        CharacterIsInAlphabet =:= false -> {error, {expected_alphabet_character, Character}}
    end.

% Alphabet validation
validate_machine_alphabet(Alphabet) ->
    look_for_duplicated_in_list(Alphabet).

% States validation
validate_machine_states(States) ->
    look_for_duplicated_in_list(States).

% Blank Validation
validate_machine_blank(Blank, Alphabet) ->
    is_alphabet_character(Blank, Alphabet).
