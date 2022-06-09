import fs from "fs";
import util from "util";

const STATE_DECLERATION_START = "{";
const STATE_DECLERATION_END = "}";
const TRANSITION_DECLERATION_START = "[";
const TRANSITION_DECLERATION_END = "]";
const INITIAL_STATE_DECLERATION_END = "~";
const INPUT_DECLERATION_START = "&";
const HALT_STATE = "H";

const FINAL_STATE = "HALT";
const ACTION_RIGHT = ">";
const ACTION_LEFT = "<";
const BLANK = ".";
const INPUT_BLANK_ALIAS = "_";
const CURRENT_READ_CHARACTER = "^";

// Transitions prefix
const GO_TO_INPUT_START_FOR_PREFIX = "go-to-input-start-for_";
const READ_TAPE_FOR_PREFIX = "read-tape-for_";
const SCANLEFT_TO_STATES_DECLERATION_FOR =
  "scanleft-to-states-decleration-for_"; // scanleft-to-states-decleration-for_S(0)
const FIND_STATE_PREFIX = "find-state_";
const FIND_STATE_TRANSITION_PREFIX = "find-state-transition-for_";
const SCANRIGHT_TO_NEXT_STATE_DEFINITION_PREFIX = `scanright-to-next-state-definition_`;

const STATIC_ALPHABET = [
  STATE_DECLERATION_START,
  STATE_DECLERATION_END,
  TRANSITION_DECLERATION_START,
  TRANSITION_DECLERATION_END,
  INPUT_BLANK_ALIAS,
  INITIAL_STATE_DECLERATION_END,
  INPUT_DECLERATION_START,
  HALT_STATE,
  ACTION_RIGHT,
  ACTION_LEFT,
  BLANK,
];

const generatorConfigFile = process.argv[2];
const rawdata = fs.readFileSync(generatorConfigFile);
const parsedConfig = JSON.parse(rawdata);
console.log(parsedConfig);

const configReadWrite = parsedConfig.writeRead;
const configStates = parsedConfig.states;

const StatesDuplicationReservedError = configStates.every(
  (state) => STATIC_ALPHABET.includes(state) === false
);
if (!StatesDuplicationReservedError) {
  console.error("StatesDuplicationReservedError");
  process.exit(1);
}

const readWriteDuplicationReservedError = configReadWrite.every(
  (character) => STATIC_ALPHABET.includes(character) === false
);
if (!readWriteDuplicationReservedError) {
  console.error("readWriteDuplicationReservedError");
  process.exit(1);
}

const dynamicAlphabet = [...configStates, ...configReadWrite];
const updatedAlphabet = [...STATIC_ALPHABET, ...dynamicAlphabet];
const customConfigInputFindableCharCollection = [BLANK, ...configReadWrite];
const customConfigStatesWithHalt = [HALT_STATE, ...configStates];
const customConfigStates = configStates;
console.log(updatedAlphabet);

const result = {
  name: "utm_machine",
  alphabet: updatedAlphabet,
  blank: BLANK,
  finals: [FINAL_STATE],
  states: [FINAL_STATE],
  transitions: {},
};
// First build "retrieve_initial_state",

const retrieveInitialStateName = "retrieve-initial-state";
result.initial = retrieveInitialStateName;
result.states.push(retrieveInitialStateName);
result.transitions[retrieveInitialStateName] = [];
customConfigStatesWithHalt.forEach((state) => {
  // If initial state is the final state then we exit
  if (state === HALT_STATE) {
    result.transitions[retrieveInitialStateName].push({
      read: state,
      to_state: FINAL_STATE,
      write: state,
      action: ACTION_RIGHT,
    });
    return;
  }

  result.transitions[retrieveInitialStateName].push({
    read: state,
    to_state: GO_TO_INPUT_START_FOR_PREFIX + state,
    write: state,
    action: ACTION_RIGHT,
  });
});

// Then build "go-to-input-start-for_S",
customConfigStates.forEach((configState) => {
  const newState = GO_TO_INPUT_START_FOR_PREFIX + configState;
  result.states.push(newState);
  result.transitions[newState] = [];

  updatedAlphabet.forEach((character) => {
    if (character === INPUT_DECLERATION_START) {
      result.transitions[newState].push({
        read: character,
        to_state: READ_TAPE_FOR_PREFIX + configState,
        write: character,
        action: ACTION_RIGHT,
      });
      return;
    }

    result.transitions[newState].push({
      read: character,
      to_state: newState,
      write: character,
      action: ACTION_RIGHT,
    });
  });
});

// Then build "read-tape-for_CHAR"
for (const configState of customConfigStates) {
  const newState = READ_TAPE_FOR_PREFIX + configState;
  result.states.push(newState);
  result.transitions[newState] = [];

  for (const inputCharacter of customConfigInputFindableCharCollection) {
    result.transitions[newState].push({
      read: inputCharacter,
      to_state: `${SCANLEFT_TO_STATES_DECLERATION_FOR}${configState}(${inputCharacter})`,
      write: CURRENT_READ_CHARACTER,
      action: ACTION_LEFT,
    });
  }
}

// Then build "scanleft-to-states-decleration-for_S(0)"
for (const state of customConfigStates) {
  for (const inputCharacter of customConfigInputFindableCharCollection) {
    const newState = `${SCANLEFT_TO_STATES_DECLERATION_FOR}${state}(${inputCharacter})`;
    result.states.push(newState);
    result.transitions[newState] = [];

    for (const alphabetCharacter of updatedAlphabet) {
      switch (alphabetCharacter) {
        case INITIAL_STATE_DECLERATION_END: {
          result.transitions[newState].push({
            read: INITIAL_STATE_DECLERATION_END,
            to_state: `${FIND_STATE_PREFIX}${state}(${inputCharacter})`,
            write: INITIAL_STATE_DECLERATION_END,
            action: ACTION_RIGHT,
          });
          break;
        }
        default: {
          result.transitions[newState].push({
            read: alphabetCharacter,
            to_state: newState,
            write: alphabetCharacter,
            action: ACTION_LEFT,
          });
        }
      }
    }
  }
}

// Then build "find-state_S(1)"
for (const configState of customConfigStates) {
  for (const inputCharacter of customConfigInputFindableCharCollection) {
    const newState = `${FIND_STATE_PREFIX}${configState}(${inputCharacter})`;
    result.states.push(newState);
    result.transitions[newState] = [];

    for (const readstate of customConfigStates) {
      const readStateIsSearchedState = readstate === configState;
      if (readStateIsSearchedState === true) {
        result.transitions[newState].push({
          read: configState,
          to_state: `${FIND_STATE_TRANSITION_PREFIX}${configState}(${inputCharacter})`,
          write: configState,
          action: ACTION_RIGHT,
        });
      } else {
        result.transitions[newState].push({
          read: configState,
          to_state: `scanright-to-next-state-definition_S(0)`,
          write: alphabetCharacter,
          action: ACTION_RIGHT,
        });
      }
    }
  }
}
console.log(util.inspect(result, false, null, true /* enable colors */));
