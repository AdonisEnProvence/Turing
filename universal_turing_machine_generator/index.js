import fs from "fs";

const STATE_DECLERATION_START = "{";
const STATE_DECLERATION_END = "}";
const TRANSITION_DECLERATION_START = "[";
const TRANSITION_DECLERATION_END = "]";
const INITIAL_STATE_DECLERATION_END = "~";
const INPUT_DECLERATION_START = "&";
const HALT_STATE = "H";

const FINAL_STATE_NAME = "HALT";
const ACTION_RIGHT = ">";
const ACTION_LEFT = "<";
const BLANK = ".";
const INPUT_BLANK_ALIAS = "_";
const CURRENT_READ_CHARACTER = "^";

const FINDABLE_ACTIONS_VALUES = [ACTION_LEFT, ACTION_RIGHT];

const FORMATTED_ACTION_LEFT = "LEFT";
const FORMATTED_ACTION_RIGHT = "RIGHT";

// Transitions prefix
const GO_TO_INPUT_START_FOR_PREFIX = "go-to-input-start-for_";
const READ_TAPE_FOR_PREFIX = "read-tape-for_";
const SCANLEFT_TO_STATES_DECLERATION_FOR =
  "scanleft-to-states-decleration-for_"; // scanleft-to-states-decleration-for_S(0)
const FIND_STATE_PREFIX = "find-state_";
const FIND_STATE_TRANSITION_PREFIX = "find-state-transition-for_";
const SCANRIGHT_TO_NEXT_STATE_DEFINITION_PREFIX = `scanright-to-next-state-definition_`;
const SCANRIGHT_TO_NEXT_TRANSITION = `scanright-to-next-state-transition_`;
const RETRIEVE_INITIAL_STATE_NAME = "retrieve_initial_state";
const RETRIEVE_TRANSITION_TO_STATE = "retrieve-transition-to_state";
const RETRIEVE_TRANSITION_ACTION = (toState) =>
  `retrieve-transition-${toState}_action`;
const RETRIEVE_TRANSITION_WRITE = ({ toState, action }) =>
  `retrieve-transition-${toState}_${action}_write`;
const EXECUTE_TRANSITION_TO_STATE_ACTION_WRITE = ({ toState, action, write }) =>
  `execute-transition-${toState}_${action}_${write}`;

const getFormattedExecuteTranstion = ({ toState, write, action }) =>
  EXECUTE_TRANSITION_TO_STATE_ACTION_WRITE({
    toState,
    action,
    write: write === INPUT_BLANK_ALIAS ? BLANK : write,
  });

const getFormattedAction = (action) => {
  switch (action) {
    case ACTION_LEFT: {
      return "LEFT";
    }
    case ACTION_RIGHT: {
      return "RIGHT";
    }
    default: {
      throw new Error(
        "encountered unkown action value inside GET_FORMATTED_ACTION " + action
      );
    }
  }
};

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
  CURRENT_READ_CHARACTER,
];

function init() {
  const generatorConfigFile = process.argv[2];
  const rawdata = fs.readFileSync(generatorConfigFile);
  const parsedConfig = JSON.parse(rawdata);

  const configInputCharacters = parsedConfig.inputCharacters;
  const configStates = parsedConfig.states;

  const StatesDuplicationReservedError = configStates.every(
    (state) => STATIC_ALPHABET.includes(state) === false
  );
  if (!StatesDuplicationReservedError) {
    console.error("StatesDuplicationReservedError");
    return 1;
  }

  const readWriteDuplicationReservedError = configInputCharacters.every(
    (character) => STATIC_ALPHABET.includes(character) === false
  );
  if (!readWriteDuplicationReservedError) {
    console.error("readWriteDuplicationReservedError");
    return 1;
  }

  const dynamicAlphabet = [...configStates, ...configInputCharacters];
  const finalAlphabet = [...STATIC_ALPHABET, ...dynamicAlphabet];
  const customConfigInputFindableCharCollection = [
    BLANK,
    ...configInputCharacters,
  ]; //after the &
  const customConfigStatesWithHalt = [HALT_STATE, ...configStates];
  const customConfigStates = configStates;

  const betweenTransitionsDeclerationFindableCharacters = [
    ...dynamicAlphabet,
    HALT_STATE,
    TRANSITION_DECLERATION_START,
    TRANSITION_DECLERATION_END,
    ACTION_LEFT,
    ACTION_RIGHT,
    INPUT_BLANK_ALIAS,
  ];
  const betweenStatesDeclerationFindableCharacters = [
    ...betweenTransitionsDeclerationFindableCharacters,
    STATE_DECLERATION_START,
    STATE_DECLERATION_END,
  ];
  const transitionReadWriteFindableCharacters = [
    ...configInputCharacters,
    INPUT_BLANK_ALIAS,
  ];

  const result = {
    name: "utm_machine",
    alphabet: finalAlphabet,
    blank: BLANK,
    finals: [FINAL_STATE_NAME],
    states: [FINAL_STATE_NAME],
    transitions: {},
  };
  // First build "retrieve_initial_state",

  result.initial = RETRIEVE_INITIAL_STATE_NAME;
  result.states.push(RETRIEVE_INITIAL_STATE_NAME);
  result.transitions[RETRIEVE_INITIAL_STATE_NAME] = [];
  customConfigStatesWithHalt.forEach((state) => {
    // If initial state is the final state then we exit
    if (state === HALT_STATE) {
      result.transitions[RETRIEVE_INITIAL_STATE_NAME].push({
        read: state,
        to_state: FINAL_STATE_NAME,
        write: state,
        action: FORMATTED_ACTION_RIGHT,
      });
      return;
    }

    result.transitions[RETRIEVE_INITIAL_STATE_NAME].push({
      read: state,
      to_state: GO_TO_INPUT_START_FOR_PREFIX + state,
      write: state,
      action: FORMATTED_ACTION_RIGHT,
    });
  });

  // Then build "go-to-input-start-for_S",
  customConfigStates.forEach((configState) => {
    const newState = GO_TO_INPUT_START_FOR_PREFIX + configState;
    result.states.push(newState);
    result.transitions[newState] = [];

    finalAlphabet.forEach((character) => {
      switch (character) {
        case INPUT_DECLERATION_START: {
          result.transitions[newState].push({
            read: character,
            to_state: READ_TAPE_FOR_PREFIX + configState,
            write: character,
            action: FORMATTED_ACTION_RIGHT,
          });
          break;
        }
        case BLANK: {
          // We should not be finding any blank character inside the machine config decleration
          // thanks to the BLANK_ALIAS usage
          break;
        }
        default: {
          result.transitions[newState].push({
            read: character,
            to_state: newState,
            write: character,
            action: FORMATTED_ACTION_RIGHT,
          });
        }
      }
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
        action: FORMATTED_ACTION_LEFT,
      });
    }
  }

  // Then build "scanleft-to-states-decleration-for_S(0)"
  for (const state of customConfigStates) {
    for (const inputCharacter of customConfigInputFindableCharCollection) {
      const newState = `${SCANLEFT_TO_STATES_DECLERATION_FOR}${state}(${inputCharacter})`;
      result.states.push(newState);
      result.transitions[newState] = [];

      for (const alphabetCharacter of finalAlphabet) {
        switch (alphabetCharacter) {
          case INITIAL_STATE_DECLERATION_END: {
            result.transitions[newState].push({
              read: INITIAL_STATE_DECLERATION_END,
              to_state: `${FIND_STATE_PREFIX}${state}(${inputCharacter})`,
              write: INITIAL_STATE_DECLERATION_END,
              action: FORMATTED_ACTION_RIGHT,
            });
            break;
          }
          default: {
            result.transitions[newState].push({
              read: alphabetCharacter,
              to_state: newState,
              write: alphabetCharacter,
              action: FORMATTED_ACTION_LEFT,
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
            read: readstate,
            to_state: `${FIND_STATE_TRANSITION_PREFIX}${configState}(${inputCharacter})`,
            write: readstate,
            action: FORMATTED_ACTION_RIGHT,
          });
        } else {
          result.transitions[newState].push({
            read: readstate,
            to_state: `${SCANRIGHT_TO_NEXT_STATE_DEFINITION_PREFIX}${configState}(${inputCharacter})`,
            write: readstate,
            action: FORMATTED_ACTION_RIGHT,
          });
        }
      }
    }
  }

  // Then build scanright-to-next-state-definition_S(0)
  for (const configState of customConfigStates) {
    for (const inputCharacter of customConfigInputFindableCharCollection) {
      const newState = `${SCANRIGHT_TO_NEXT_STATE_DEFINITION_PREFIX}${configState}(${inputCharacter})`;
      result.states.push(newState);
      result.transitions[newState] = [];

      for (const character of betweenStatesDeclerationFindableCharacters) {
        switch (character) {
          case STATE_DECLERATION_END: {
            result.transitions[newState].push({
              read: character,
              to_state: `${FIND_STATE_PREFIX}${configState}(${inputCharacter})`,
              write: character,
              action: FORMATTED_ACTION_RIGHT,
            });
            break;
          }
          default: {
            result.transitions[newState].push({
              read: character,
              to_state: newState,
              write: character,
              action: FORMATTED_ACTION_RIGHT,
            });
          }
        }
      }
    }
  }

  // Then buil "find-state-transition-for_S(0)"
  for (const configState of customConfigStates) {
    for (const inputCharacter of customConfigInputFindableCharCollection) {
      const newState = `${FIND_STATE_TRANSITION_PREFIX}${configState}(${inputCharacter})`;
      result.states.push(newState);
      result.transitions[newState] = [];

      const charToSkipCollection = [
        TRANSITION_DECLERATION_START,
        STATE_DECLERATION_START,
      ];
      for (const charToSkip of charToSkipCollection) {
        result.transitions[newState].push({
          read: charToSkip,
          to_state: newState,
          write: charToSkip,
          action: FORMATTED_ACTION_RIGHT,
        });
      }

      for (const transitionReadCharacter of transitionReadWriteFindableCharacters) {
        const inputCharacterIsTransitionCharacter =
          inputCharacter === transitionReadCharacter;

        if (inputCharacterIsTransitionCharacter) {
          result.transitions[newState].push({
            read: transitionReadCharacter,
            to_state: RETRIEVE_TRANSITION_TO_STATE,
            write: transitionReadCharacter,
            action: FORMATTED_ACTION_RIGHT,
          });

          continue;
        }

        const transitionReadCharacterIsBlankAlias =
          inputCharacter === BLANK &&
          transitionReadCharacter === INPUT_BLANK_ALIAS;

        if (transitionReadCharacterIsBlankAlias) {
          result.transitions[newState].push({
            read: transitionReadCharacter,
            to_state: RETRIEVE_TRANSITION_TO_STATE,
            write: transitionReadCharacter,
            action: FORMATTED_ACTION_RIGHT,
          });

          continue;
        }

        result.transitions[newState].push({
          read: transitionReadCharacter,
          to_state: `${SCANRIGHT_TO_NEXT_TRANSITION}${configState}(${inputCharacter})`,
          write: transitionReadCharacter,
          action: FORMATTED_ACTION_RIGHT,
        });
      }
    }
  }

  // Then build "go-to-next-state-transition_S(1)"
  for (const configState of customConfigStates) {
    for (const inputCharacter of customConfigInputFindableCharCollection) {
      const newState = `${SCANRIGHT_TO_NEXT_TRANSITION}${configState}(${inputCharacter})`;
      result.states.push(newState);
      result.transitions[newState] = [];

      for (const betweenTransitionsDefinitionChar of betweenTransitionsDeclerationFindableCharacters) {
        switch (betweenTransitionsDefinitionChar) {
          case TRANSITION_DECLERATION_END: {
            result.transitions[newState].push({
              read: betweenTransitionsDefinitionChar,
              to_state: `${FIND_STATE_TRANSITION_PREFIX}${configState}(${inputCharacter})`,
              write: betweenTransitionsDefinitionChar,
              action: FORMATTED_ACTION_RIGHT,
            });
            break;
          }
          default: {
            result.transitions[newState].push({
              read: betweenTransitionsDefinitionChar,
              to_state: newState,
              write: betweenTransitionsDefinitionChar,
              action: FORMATTED_ACTION_RIGHT,
            });
          }
        }
      }
    }
  }

  // Then build "retrieve-transition-to_state"
  result.states.push(RETRIEVE_TRANSITION_TO_STATE);
  result.transitions[RETRIEVE_TRANSITION_TO_STATE] = [];
  for (const toStateTarget of customConfigStatesWithHalt) {
    result.transitions[RETRIEVE_TRANSITION_TO_STATE].push({
      read: toStateTarget,
      to_state: RETRIEVE_TRANSITION_ACTION(toStateTarget),
      write: toStateTarget,
      action: FORMATTED_ACTION_RIGHT,
    });
  }

  //Then build "retrieve-transition-S_action"
  for (const toStateTarget of customConfigStatesWithHalt) {
    const newState = RETRIEVE_TRANSITION_ACTION(toStateTarget);
    result.states.push(newState);
    result.transitions[newState] = [];

    for (const action of FINDABLE_ACTIONS_VALUES) {
      result.transitions[newState].push({
        read: action,
        to_state: RETRIEVE_TRANSITION_WRITE({ toState: toStateTarget, action }),
        write: action,
        action: FORMATTED_ACTION_RIGHT,
      });
    }
  }

  // Then build "retrieve-transition-H_>_write"
  for (const toStateTarget of customConfigStatesWithHalt) {
    for (const action of FINDABLE_ACTIONS_VALUES) {
      const newState = RETRIEVE_TRANSITION_WRITE({
        toState: toStateTarget,
        action,
      });
      result.states.push(newState);
      result.transitions[newState] = [];

      for (const transitionWriteCharacter of transitionReadWriteFindableCharacters) {
        result.transitions[newState].push({
          read: transitionWriteCharacter,
          to_state: getFormattedExecuteTranstion({
            toState: toStateTarget,
            action,
            write: transitionWriteCharacter,
          }),
          write: transitionWriteCharacter,
          action: FORMATTED_ACTION_RIGHT,
        });
      }
    }
  }

  // Then build "execute-transition-S_>_."
  for (const toStateTarget of customConfigStatesWithHalt) {
    for (const action of FINDABLE_ACTIONS_VALUES) {
      for (const transitionWriteCharacter of transitionReadWriteFindableCharacters) {
        const newState = getFormattedExecuteTranstion({
          toState: toStateTarget,
          action,
          write: transitionWriteCharacter,
        });
        result.states.push(newState);
        result.transitions[newState] = [];

        for (const alphabetCharacter of finalAlphabet) {
          switch (alphabetCharacter) {
            case CURRENT_READ_CHARACTER: {
              const toStateOnCurrentReadCharacter =
                toStateTarget === HALT_STATE
                  ? FINAL_STATE_NAME
                  : READ_TAPE_FOR_PREFIX + toStateTarget;

              const write =
                transitionWriteCharacter === INPUT_BLANK_ALIAS
                  ? BLANK
                  : transitionWriteCharacter;

              result.transitions[newState].push({
                read: alphabetCharacter,
                to_state: toStateOnCurrentReadCharacter,
                write,
                action: getFormattedAction(action),
              });
              break;
            }
            default: {
              result.transitions[newState].push({
                read: alphabetCharacter,
                to_state: newState,
                write: alphabetCharacter,
                action: FORMATTED_ACTION_RIGHT,
              });
            }
          }
        }
      }
    }
  }

  fs.writeFileSync("./data.json", JSON.stringify(result, null, 2), "utf-8");
}

process.exit(init());
