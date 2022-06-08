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

const updatedAlphabet = [
  ...STATIC_ALPHABET,
  ...configStates,
  ...configReadWrite,
];
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
const goToInputStartForStateNames = customConfigStates.map(
  (state) => GO_TO_INPUT_START_FOR_PREFIX + state
);
result.states = [...result.states, ...goToInputStartForStateNames];
goToInputStartForStateNames.forEach((state) => {
  result.transitions[state] = [];
  updatedAlphabet.forEach((character) => {
    if (character === INPUT_DECLERATION_START) {
      result.transitions[state].push({
        read: character,
        to_state: READ_TAPE_FOR_PREFIX + state,
        write: character,
        action: ACTION_RIGHT,
      });
      return;
    }

    result.transitions[state].push({
      read: character,
      to_state: state,
      write: character,
      action: ACTION_RIGHT,
    });
  });
});

// Then build "read-tape-for_CHAR"
const ReadTapeForStateNames = customConfigStates.map(
  (state) => READ_TAPE_FOR_PREFIX + state
);
result.states = [...result.states, ...ReadTapeForStateNames];

for (const state of ReadTapeForStateNames) {
  result.transitions[state] = [];
  for (const character of configReadWrite) {
    result.transitions[state].push({
      read: character,
      to_state: `${SCANLEFT_TO_STATES_DECLERATION_FOR}${state}(${character})`,
      write: CURRENT_READ_CHARACTER,
      action: ACTION_LEFT,
    });
  }
}

console.log(util.inspect(result, false, null, true /* enable colors */));
