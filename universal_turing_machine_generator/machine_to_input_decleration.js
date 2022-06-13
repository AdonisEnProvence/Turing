import fs from "fs";

const STATE_DECLERATION_START = "{";
const STATE_DECLERATION_END = "}";
const TRANSITION_DECLERATION_START = "[";
const TRANSITION_DECLERATION_END = "]";
const INITIAL_STATE_DECLERATION_END = "~";
const INPUT_DECLERATION_START = "&";

const ACTION_RIGHT = ">";
const ACTION_LEFT = "<";

const BLANK = ".";
const INPUT_BLANK_ALIAS = "_";
const FORMATTED_ACTION_LEFT = "LEFT";
const FORMATTED_ACTION_RIGHT = "RIGHT";

const DEFAULT_INPUT_LABEL = "YOUR_INPUT";

const getFormattedAction = (action) => {
  switch (action) {
    case FORMATTED_ACTION_LEFT: {
      return ACTION_LEFT;
    }
    case FORMATTED_ACTION_RIGHT: {
      return ACTION_RIGHT;
    }
    default: {
      throw new Error(
        "encountered unkown action value inside GET_FORMATTED_ACTION " + action
      );
    }
  }
};

const getFormattedReadWrite = (character) => {
  switch (character) {
    case BLANK: {
      return INPUT_BLANK_ALIAS;
    }
    default: {
      return character;
    }
  }
};

function init() {
  const machineConfigFilePath = process.argv[2];
  const rawdata = fs.readFileSync(machineConfigFilePath);
  const parsedMachineConfig = JSON.parse(rawdata);

  const stateCollection = parsedMachineConfig.states;
  const transitionCollection = parsedMachineConfig.transitions;
  const initialState = parsedMachineConfig.initial;

  let result = "";

  // first initial state
  result += initialState + INITIAL_STATE_DECLERATION_END;

  //Iterate for each states
  for (const state of stateCollection) {
    const stateTransitionCollection = transitionCollection[state];

    if (stateTransitionCollection === undefined) {
      continue;
    }

    result += state + STATE_DECLERATION_START;

    for (const {
      read,
      write,
      to_state: toState,
      action,
    } of stateTransitionCollection) {
      result += `${TRANSITION_DECLERATION_START}${getFormattedReadWrite(
        read
      )}${toState}${getFormattedAction(action)}${getFormattedReadWrite(
        write
      )}${TRANSITION_DECLERATION_END}`;
    }

    result += STATE_DECLERATION_END;
  }

  result += INPUT_DECLERATION_START + DEFAULT_INPUT_LABEL;
  console.log(JSON.stringify(result));
}

init();
