import { assign, createMachine, send } from "xstate";
import { AutomaticPlayingDelayMode, MachineExecution } from "../types";

import unaryAddMachine from "../../../our-machines/unary_add.json?raw";
import { submitButtonMachine } from "./submit-button";

const exec: MachineExecution = {
  blank: ".",
  tapeHistory: [
    {
      currentState: "pick_character",
      indexOnTape: 0,
      status: "continue",
      tape: ["a", "b", "c", "b", "a"],
    },
    {
      currentState: "go_to_end_and_find_a",
      indexOnTape: 1,
      status: "continue",
      tape: [".", "b", "c", "b", "a"],
    },
    {
      currentState: "go_to_end_and_find_a",
      indexOnTape: 2,
      status: "continue",
      tape: [".", "b", "c", "b", "a"],
    },
    {
      currentState: "go_to_end_and_find_a",
      indexOnTape: 3,
      status: "continue",
      tape: [".", "b", "c", "b", "a"],
    },
    {
      currentState: "go_to_end_and_find_a",
      indexOnTape: 4,
      status: "continue",
      tape: [".", "b", "c", "b", "a"],
    },
    {
      currentState: "go_to_end_and_find_a",
      indexOnTape: 5,
      status: "continue",
      tape: [".", "b", "c", "b", "a", "."],
    },
    {
      currentState: "is_a",
      indexOnTape: 4,
      status: "continue",
      tape: [".", "b", "c", "b", "a", "."],
    },
    {
      currentState: "go_to_beginning",
      indexOnTape: 3,
      status: "continue",
      tape: [".", "b", "c", "b", ".", "."],
    },
    {
      currentState: "go_to_beginning",
      indexOnTape: 2,
      status: "continue",
      tape: [".", "b", "c", "b", ".", "."],
    },
    {
      currentState: "go_to_beginning",
      indexOnTape: 1,
      status: "continue",
      tape: [".", "b", "c", "b", ".", "."],
    },
    {
      currentState: "go_to_beginning",
      indexOnTape: 0,
      status: "continue",
      tape: [".", "b", "c", "b", ".", "."],
    },
    {
      currentState: "pick_character",
      indexOnTape: 1,
      status: "continue",
      tape: [".", "b", "c", "b", ".", "."],
    },
    {
      currentState: "go_to_end_and_find_b",
      indexOnTape: 2,
      status: "continue",
      tape: [".", ".", "c", "b", ".", "."],
    },
    {
      currentState: "go_to_end_and_find_b",
      indexOnTape: 3,
      status: "continue",
      tape: [".", ".", "c", "b", ".", "."],
    },
    {
      currentState: "go_to_end_and_find_b",
      indexOnTape: 4,
      status: "continue",
      tape: [".", ".", "c", "b", ".", "."],
    },
    {
      currentState: "is_b",
      indexOnTape: 3,
      status: "continue",
      tape: [".", ".", "c", "b", ".", "."],
    },
    {
      currentState: "go_to_beginning",
      indexOnTape: 2,
      status: "continue",
      tape: [".", ".", "c", ".", ".", "."],
    },
    {
      currentState: "go_to_beginning",
      indexOnTape: 1,
      status: "continue",
      tape: [".", ".", "c", ".", ".", "."],
    },
    {
      currentState: "pick_character",
      indexOnTape: 2,
      status: "continue",
      tape: [".", ".", "c", ".", ".", "."],
    },
    {
      currentState: "go_to_end_and_find_c",
      indexOnTape: 3,
      status: "continue",
      tape: [".", ".", ".", ".", ".", "."],
    },
    {
      currentState: "is_c",
      indexOnTape: 2,
      status: "continue",
      tape: [".", ".", ".", ".", ".", "."],
    },
    {
      currentState: "write_is_palindrome",
      indexOnTape: 3,
      status: "continue",
      tape: [".", ".", ".", ".", ".", "."],
    },
    {
      currentState: "HALT",
      indexOnTape: 4,
      status: "final",
      tape: [".", ".", ".", "y", ".", "."],
    },
  ],
};

export const vizMachine =
  /** @xstate-layout N4IgpgJg5mDOIC5QDUCWsCuBDANqgXmAE4B0AggA4V4DGWALqgPYB2ABOm0WFhAJ4kAslhZYoqFlDYA3dNjyEibMAA8wNDI1YkACjix8JU2PTAVY5TUwC2DVDTbUDRtkwBmbgMR6DiUBSZYVC0WPxAVRABaAEYAJgA2EniAZgBWAAZUgE54+IAOWIAWWNTUgBoQPijo+IB2EmTCrOim2sLUxuK8gF9uirRMXAJicipaO1YOWC4efiERMRdZQYViZTUNEN19Q0k2EzMLMitbRgcnXalWbywMWDAwgKCQsIiEGNrYkjz41MLo5r5QrJeIJCpVd6gpJZLKpWLpWJZdKFFHI3r9ORDRSjaj2CbsTjcXgCYSicR7ZbyYZKVTqTTMFjbZx7A7mSz0Gx2c47FzXFQmBhgEhYNymIgACluHNO9kcPL2EDAOwAlJ4BlTsZRcXQQlMZsT5mSlpjVjSNvTtD5LvtTGzjtKuXLmVdQkgQE9ggzXtUsiRonlaqlatFamk4f7wYgvu14v74dEQ20aoH0SB1ViRlrxrrCbMSQtyVJKRmzXStlaXKzYJ4AHKqeg2syPQKe1je96NRJZNq1BEldL5eKFSPvWJ5VLfZKhhM5ZJFWIJVPp0047MMvVEuakxYUk3U9ZlhlM61VzzN55et1vYoTwr5PJZO+1Z+pPJ5YeVaqpaLfaLf6IDu+sQAcUS57pqYx4jm0ybvmRq7is+60psR4AEo8DQAAWkDKCwECuG4jbmJ46H3A2Vbnq2rqgG8KQ-qCmTZAURRZLEI5wiQd4-O037ZNkY5gYhEHaviG55oaO5FuBazIRajIAJIQDgYCeAAoqIABGylOtasm6hRboei8V5RKGyRJC+vZwpkC4jjEHHxFkyRxBkvbAcGPR9Gm0mkFmUHrrmBrboWMg+QeKHXAAymADZSpyZw6S4io7JRxk0YgCZ-BZ-woskILItEdkgj+qS-LUBS5G0pVpIJGqZpBOoBTB4nBS4thYRIYBsCI+ESBQmjhXJJCKcpngADJMLwqWXul7yFKGJDPm+yZTrESYjmtvqBjUzmhnk6QtO0tUlqu-mTIFW4Fm1WAdSwXU9RwLD9Q2elHqp5qMHs7WYZ13V4Y9z2ePy9CCsKorEOK8LpOkqrLtSp2NedzVBVdX03T9d1-b1T0Da92jvYeaO3fd-19Zo01tiZCAhtG7QDrEyTpMGj61Bt7SLdk8TpD8oJwrU3bHSufmIwSyOXfBUjfb9D1ky9H1HgAYjFWE4VLmMyzjcuHtcE1TYZLZpeEUbpOZQbBn+Q6-CbeUjsCvrzaUtR1OOPw-LUgvw8LokXXBklsGrJPY89g0hJ40UNrLFPUUb7ywoUi1vukSIhjCMIbVzJAlLkdRcaUJse8Ja5I-q4t+wHWMA7j8tRTF-vo79NBMIqUftpEWQFJnzlzv681dOUn4IMk77fMxuRTkiyLu6mLBN3AYRw4XZ2iyXvshcWpoh0eFYsraRwnI6Fy8h4LdU5EdtJHxMJjq+AbjnZcQ-k5-xDnOSdvs0Bf1SJ0ErxJa9hXjRk29jC73ZPFWUh89iU38AbGaMdIh5G+M+Fo5VSp3nyM+e+wESBP3hN+fmDNgTRE-r5Bq3sxar2NEJGS1cgHyhAYcE+s0z4pAvrCK+Ltb79whJEKc6RJwJByIUAcJsfgkIRuQ3+rUEJ1VLBFRk6F0Y4TAP9dwRF4D6wvNAmOeQAJ+g6NEIe85DFNHvgOBoU4UQJERG+N84ivY-1gn-KhsjN7aBGg8TRVFW4BnMmOBm3ZkzZEKHke+2Q-QwgRA+duRQ-yFHsWQxxLVUZSWoXIuSTD4EPnqP45IgS6jBNCQPM+wJvhjm5k7Go2Q0jxASd-JqUiUl12JhXWWbiFJKU8TArR0c3iRGdg0ZoMSfgm02htBaRRshM3KtzRmWQ6lF2Xk46Rkt67q1JprdpJACYoSJhjQOld6CZL6YzL4LRXyxivsIv48QNppA5sCP4JtHwp2SAspeYkUYS2afs1pmzAEkCVvQFW+Fy4a2DoA45iB+KdwBMCZErEr5s3qNtEM75mi6IRLUryC8v6LM+aXEKYKNkQtoVC94blBkAgKCMuc-MRw1C+IGB8cS-ilDeTinyEiknEnJTEaGiQGaGOAu+No187LZB-PtFICLQQwmfCQvlAFGawu7qKvudlcjx32iGEEMyQLEN6N0IAA */
  createMachine(
    {
      context: {
        stepIndex: 0,
        automaticPlayingDelayMode: "MEDIUM",
        input: "111+11",
        machineCode: unaryAddMachine,
        lastLoadingInput: undefined,
        lastLoadingMachineCode: undefined,
        machineExecution: undefined,
      },
      tsTypes: {} as import("./viz.typegen").Typegen0,
      schema: {
        context: {} as {
          stepIndex: number;
          automaticPlayingDelayMode: AutomaticPlayingDelayMode;
          machineCode: string;
          input: string;
          lastLoadingMachineCode: string | undefined;
          lastLoadingInput: string | undefined;
          machineExecution: MachineExecution | undefined;
        },
        events: {} as
          | { type: "Enable playing execution steps" }
          | { type: "Play" }
          | { type: "Pause" }
          | { type: "Next step" }
          | {
              type: "Set automatic playing delay";
              mode: AutomaticPlayingDelayMode;
            }
          | { type: "Reset steps" }
          | { type: "Set input"; input: string }
          | { type: "Set machine code"; machineCode: string }
          | { type: "Load" },
      },
      initial: "Application is ready",
      states: {
        "Application is ready": {
          invoke: {
            src: "Start submit button machine",
            id: "Submit button",
          },
          type: "parallel",
          states: {
            "Managing visualizer execution": {
              initial: "Idle",
              states: {
                "Playing steps": {
                  initial: "Automatic playing off",
                  states: {
                    "Automatic playing off": {
                      on: {
                        Play: {
                          target: "Automatic playing on",
                        },
                      },
                    },
                    "Automatic playing on": {
                      after: {
                        "automatic playing delay": {
                          actions: "Increment step index",
                          target: "Automatic playing on",
                          internal: false,
                        },
                      },
                      on: {
                        Pause: {
                          target: "Automatic playing off",
                        },
                      },
                    },
                  },
                  always: {
                    cond: "Has reached end of steps",
                    target: "Reached end of steps",
                  },
                  on: {
                    "Next step": {
                      actions: "Increment step index",
                      target: ".Automatic playing off",
                    },
                  },
                },
                "Reached end of steps": {
                  on: {
                    "Reset steps": {
                      actions: "Reset step index",
                      target: "Playing steps",
                    },
                  },
                },
                Idle: {
                  on: {
                    "Enable playing execution steps": {
                      target: "Playing steps",
                    },
                  },
                },
              },
              on: {
                "Set automatic playing delay": {
                  actions: "Assign automatic playing delay to context",
                },
              },
            },
            "Managing machine and input execution": {
              initial: "Idle",
              states: {
                Idle: {
                  on: {
                    Load: {
                      target: "Executing machine and input",
                    },
                  },
                },
                "Executing machine and input": {
                  entry: "Cache input and machine code into context",
                  after: {
                    "2000": {
                      actions: [
                        "Exit loading state from submit button",
                        "Assign machine execution to context",
                      ],
                      target: "Fetched machine and input execution",
                    },
                  },
                },
                "Fetched machine and input execution": {
                  entry: "Allow to play execution steps",
                  on: {
                    Load: {
                      target: "Executing machine and input",
                    },
                  },
                },
              },
              on: {
                "Set input": {
                  actions: "Assign input to context",
                },
                "Set machine code": {
                  actions: "Assign machine code to context",
                },
              },
            },
          },
        },
      },
      id: "Visualizer",
    },
    {
      actions: {
        "Increment step index": assign({
          stepIndex: (context) => context.stepIndex + 1,
        }),
        "Reset step index": assign({
          stepIndex: (context) => 0,
        }),
        "Assign automatic playing delay to context": assign({
          automaticPlayingDelayMode: (_, event) => event.mode,
        }),
        "Allow to play execution steps": send({
          type: "Enable playing execution steps",
        }),
        "Exit loading state from submit button": send(
          {
            type: "Finished loading",
          },
          {
            to: "Submit button",
          }
        ),
        "Assign input to context": assign({
          input: (_, event) => event.input,
        }),
        "Assign machine code to context": assign({
          machineCode: (_, event) => event.machineCode,
        }),
        "Cache input and machine code into context": assign({
          lastLoadingInput: (context) => context.input,
          lastLoadingMachineCode: (context) => context.machineCode,
        }),
        "Assign machine execution to context": assign({
          machineExecution: (context) => exec,
        }),
      },
      delays: {
        "automatic playing delay": (context) =>
          context.automaticPlayingDelayMode === "MEDIUM" ? 1000 : 500,
      },
      guards: {
        "Has reached end of steps": ({ stepIndex, machineExecution }) => {
          if (machineExecution === undefined) {
            return false;
          }

          return stepIndex >= machineExecution.tapeHistory.length - 1;
        },
      },
      services: {
        "Start submit button machine": submitButtonMachine,
      }
    }
  );
