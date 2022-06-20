import { assign, createMachine, send } from "xstate";
import { AutomaticPlayingDelayMode, MachineExecution } from "../types";

import unaryAddMachine from "../../../our-machines/unary_add.json?raw";
import { submitButtonMachine } from "./submit-button";

interface ServerErrorForMachineExecution {
  reason: string;
}

type ServerResponseForMachineExecution =
  | MachineExecution
  | ServerErrorForMachineExecution;

function isServerResponseForMachineExecution(
  response: unknown
): response is ServerResponseForMachineExecution {
  if (typeof response !== "object") {
    return false;
  }

  return (
    isMachineExecution(response as ServerResponseForMachineExecution) ||
    isServerErrorForMachineExecution(
      response as ServerResponseForMachineExecution
    )
  );
}

function assertIsServerResponseForMachineExecution(
  response: unknown
): asserts response is ServerResponseForMachineExecution {
  if (isServerResponseForMachineExecution(response) === false) {
    throw new Error(
      "Expected response to be a ServerResponseForMachineExecution"
    );
  }
}

function isMachineExecution(
  response: ServerResponseForMachineExecution
): response is MachineExecution {
  return "blank" in response && "tapeHistory" in response;
}

function assertIsMachineExecution(
  response: ServerResponseForMachineExecution
): asserts response is MachineExecution {
  if (isMachineExecution(response) === false) {
    throw new Error("Expected response to be a MachineExecution");
  }
}

function isServerErrorForMachineExecution(
  response: ServerResponseForMachineExecution
): response is ServerErrorForMachineExecution {
  return "reason" in response;
}

function assertIsServerErrorForMachineExecution(
  response: ServerResponseForMachineExecution
): asserts response is ServerErrorForMachineExecution {
  if (isServerErrorForMachineExecution(response) === false) {
    throw new Error("Expected response to be a ServerErrorForMachineExecution");
  }
}

export const vizMachine =
  /** @xstate-layout N4IgpgJg5mDOIC5QDUCWsCuBDANqgXmAE4B0AggA4V4DGWALqgPYB2ABOm0WFhAJ4kAslhZYoqFlDYA3dNjyEibMAA8wNDI1YkACjix8JU2PTAVY5TUwC2DVDTbUDRtkwBmbgMR6DiUBSZYVC0WPxAVRABaACYAVgAOEgBOeIA2AEYAFmiABgBmTPSk9PSAGhA+KOz0klSkgHZ6rLz62Jy6gF8O8rRMXAJicipaO1YOWC4efiERMRdZPoViZTUNEN19Q0k2EzMLMitbRgcnLalWbywMWDAwgKCQsIiESLyc2JJo9NS29NiCvLxTJpcqVF55dL1Eg5YE5IFJXLfeLxLo9OT9RRDaj2UbsTjcXgCYSicTbBbyAZKVTqTTMFgbZzbXbmSz0Gx2E6bFwXFQmBhgEhYNymIgACiubKO9kcXO2EDAmwAlJ5ehTMZRsXQQuNJoSZiT5uillTVrTtD4zjtTCyDpKOTLGedQkgQPdgnSnlE-jV-mlYoUcll6vl6qConEavEEYDohl6kk2rFYqiQKqMYMNSNtfipkTZqSpOT0yaaesLS5mbBPAA5VT0K1mO6Bd2sT0vTLxkh5OL1IEZVKpApJVJhl7+nIkdLxBL1H4I4f1FNp41YrN0nUE6bEuZko2UlalukMy2VzxNh4el3PaJJPKTyHxdLRCFT9I5aKhipVdqTwepVoQnkt6xOkeRLnu6rDDi2YTJueYGruiz7tSaxHgASjwNAABaQMoLAQK4bgNuYngYTc9aVueLbOqAzyZEkSSfHUHb1JksQ-JkeSxKOMSBiQ8TxsUxSgbO-rROBSGQZquIbrm+o7oWEHLChZr0gAkhAOBgJ4ACiogAEZaQ6loqdqlEum6jxXlEaSpNC0R+g5aQdu0PGZDkUIwkiw4MUkhRJBJaoZlBWrrjmerbgWMhKSWqEXAAymA9YSuyxzGS48qbFRVm0YgJT+rUrFZJxeSpF5PGlT6Py9rG-5sYOybdKmMWrtBYWwXJkWGpJymmiEngADJMLw2WXrlLyPhOgKAu+DlAYUZRfi8dTRMkXGAvRTTsf+gXFq1oVjOFW75i4tjYRIYBsCIBESBQmgHnF9I6X1p1YOdLCXddHAsHd9AzAA1i43AAI4YHA9ZsjsxDSIMiX4UDYCg+DngQKwAoSNITD-QKy6UvtMlHfBClsGdWEXVd+Hfb9D2qSQz2HtspPk19t2aADCNIyYbCQzcRAw6QcMQBzYMmAgGNMAdLAANo5AAuqNrbWQgAKTtk9F5G8A4Dh+o6zlC8QfjeXE-O8w67SumZtYdHURSdjNvWTH0UzdP33aZR706h9vvZ9lOs39wiA9sIMixDTBQ3zsNgPDweI6HnjEEQTCkE49BuMn1gkLjUlrtburHQhUhM07LOu-W7vaJ7tLe47vsu797Ox5zYcR-zJCC8L4NiywmOSzL8sWc2OXhFE-p2R2XE5HCqTqzrS3-okhS5E0U85A0Xzm3jluS7JtuFyTDvM37Zc0+sVeMDXR-12zAAiCqOmwODDULTL0PyujRy-UCeLyb+mIKwpiCijfFPZU2dgrSRgvnImUVi51ypm7F6lcXqXxLsfBud9ZRSCfrwCsf8BQ6E-kYBWNER7K1vJOVi+RSq5DSFxbiS0Eh3nci0AosR4zsX+JvHOVs8Q2wLsTOBzsEHlyQU9FBRdD5oOvvQFGaMSB8n-uA0g28Cb8Jga9H2wj-anw9hIg+WjS6-RIW2EoHYSD0VAsVL47RvijhnoxeEM9BwsNNtwiBuc+HQPkrAqR8CdEVxYJ4RK9Z-YmKVpELIaQSD1HWvkKe3poiZFHD2Wo3YITAjMakeIXF3EqJCmo7xXVUH+JPoE4JSUDG1zYDQJg8pwnjUiSBTIlCnEazfKVXWTQSBtD8mxL4cQUiZDyfjKBcEfGaOqUYxBh4LhDRGoPC8itGnZMSHUIE7EGJJINkkUckJGIQhvBkP4sY-gNC6E1FgdS4BhGUaM9qRS7aKR6rFWm5ZX57FZKlaUpxuQeAaWQyI7DVqBnaI+H4U5SrJKWq8WcnwDb1AcgmDyoFUgjNUWMzqTzoovN0eaLBxF9iHHtL87YyzXRDzGoCrINRogOWfLNJJfw2IVThfS2cKQ4QJgCk1O5GKHnjOKc8oKryywEvMv4Sl5LnhAuHNCN82TvggRyTPHiqQ4iTkfG8Zoasza8pavyvOgrsVFmNHi+kGEHa4U-oRQlALnj-n1u+d4M4MjOQqmVWoaQp6ItiMOUCjU0QvPuUarF+9TXITESQDSWl7Veinh8QcBQOEa39BrNyoEekdj9TQ+M2tA3NWDYarxxrw0xXNXGl4b4bxpOTQmVNJUeKNChLeOEa9HwOUseigpmK96CL8dospUaY23EWdRNskT2gtKjG0OoU9sjVT2WxT4BQmULQaCkRc+qi09oFWG-thj0EzMenTfRQjpkBywEHKQIdwbc3DrzNuHcm6h0rZE0qtR3JuuoZkBqn4wSNFWm8acU8DZfCaDyoNIqQ0lv3b4w9MjzWnoZpIhDIjG43rjnenm0NBgYRoGAVAMMCLcFgAEFgNxK2NDWbZTit53KxniLrBoMS-LFEfCUDIgJu2QL3X2+DUyj2iNmeIlDVSr7ocDp3LmOHI5EDfd8CcM9bHxB-X++xfl+IQjSAJZ8zb0g8c8bvARAmJMBKjefSZZmy4kEwQ-HBX8rTv0ITHKAlaigtPVVxv474gKIv-Ygdhdkoy9iSFOqcAzDO8OMxokpg7qaBOQ17VDgmZG2fvpaBzeD36Wuwtaym7g2CZV8GO4ezx2GJpdQJP1frgQjkYXCkLUZwsGyfFFnehMJlxYvUhyz3WhPpYJVl1+-J3PBlqKciEbRnzrwCwgNod4wtJJ+Kp94gIt1Qb2sWmLXWUvWYSxZs9A6L0KaBJ+lTamuJzbmmtKMaQpwJGnJBwt0HtudaFeJ6RIikMADEkp5YIueoTFbStUueKBdynxWjIlRW+Xso4-WONiB+JMK0FsGe3a93dob+NWa++ZkTJAftYFQFpAikNTKXSB2lgA0lcgA7uwR9ykiBJ3k6D6VUR-x2S+LOulXE2KsT2fkRIMIYyxOHO+FEmOtvY9g7j-riHEvE9J7hCnfUqfHYGwAVRYP9BnTPcNUlZ8nN92SPgzbpf8f4MY8ijj8lCdisSZ4JAXWimXFs5c7Y+9T77yuSdk-vafTXaH-bRp7v0QHA7aksDcKgKAGAiC4gUyL+FmRf1wijEkoXS0FVQjOa0YSgJoztcKaWg9qW-dRpV4H9XpYQ+V7CRz0hMqFWrU7Rn5ECIOzQoAwJHpYWSjdgnq0DbL3Ze8ZxyZvHpSDsiZT8ifiyK50wljOw+xE5nww+qu0BowyPdby94TBTvpWn0STR0+rYJIgIg+GFsLgE2hAjH8ok-6qz-DnafkK-UQGKJDC+nuxEUAMgmBch0EAA */
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
        serverExecutionError: undefined,
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
          serverExecutionError: string | undefined;
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
        services: {} as {
          "Execute machine and input on server": {
            data: ServerResponseForMachineExecution;
          };
        },
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
                Load: {
                  target: ".Idle",
                },
              },
            },
            "Managing machine and input execution": {
              initial: "Idle",
              states: {
                Idle: {},
                "Executing machine and input": {
                  entry: [
                    "Cache input and machine code into context",
                    "Reset step index",
                    "Reset execution server error",
                  ],
                  type: "parallel",
                  states: {
                    "Making request to server": {
                      initial: "Sending request",
                      states: {
                        "Sending request": {
                          invoke: {
                            src: "Execute machine and input on server",
                            onDone: [
                              {
                                actions: "Assign machine execution to context",
                                cond: "Is valid machine execution",
                                target: "Received response",
                              },
                              {
                                actions:
                                  "Assign execution server error to context",
                                target:
                                  "#Visualizer.Application is ready.Managing machine and input execution.Failed to execute machine and input.Known server error",
                              },
                            ],
                            onError: [
                              {
                                target:
                                  "#Visualizer.Application is ready.Managing machine and input execution.Failed to execute machine and input.Unknown server error",
                              },
                            ],
                          },
                        },
                        "Received response": {
                          type: "final",
                        },
                      },
                    },
                    "Delaying loading state": {
                      initial: "Pending",
                      states: {
                        Pending: {
                          after: {
                            "1000": {
                              target: "Reached end of delay",
                            },
                          },
                        },
                        "Reached end of delay": {
                          type: "final",
                        },
                      },
                    },
                  },
                  onDone: {
                    target: "Fetched machine and input execution",
                  },
                },
                "Fetched machine and input execution": {
                  entry: [
                    "Allow to play execution steps",
                    "Exit loading state from submit button",
                  ],
                },
                "Failed to execute machine and input": {
                  entry: [
                    "Enter error state from submit button",
                    "Allow to play execution steps",
                  ],
                  initial: "Known server error",
                  states: {
                    "Known server error": {},
                    "Unknown server error": {},
                    "Invalid machine configuration": {},
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
                Load: [
                  {
                    cond: "Machine configuration is not valid JSON",
                    target:
                      ".Failed to execute machine and input.Invalid machine configuration",
                  },
                  {
                    target: ".Executing machine and input",
                  },
                ],
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
          stepIndex: (_context) => 0,
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
        "Enter error state from submit button": send(
          {
            type: "Erred",
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
          machineExecution: (_, { data }) => {
            assertIsMachineExecution(data);

            return data;
          },
        }),
        "Assign execution server error to context": assign({
          serverExecutionError: (_, { data }) => {
            assertIsServerErrorForMachineExecution(data);

            return data.reason;
          },
        }),
        "Reset execution server error": assign({
          serverExecutionError: (_context) => undefined,
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
        "Is valid machine execution": (_context, { data }) => {
          return isMachineExecution(data);
        },
        "Machine configuration is not valid JSON": ({ machineCode }) => {
          let isInvalidJSON = false;

          try {
            JSON.parse(machineCode);
          } catch {
            isInvalidJSON = true;
          }

          return isInvalidJSON;
        },
      },
      services: {
        "Start submit button machine": submitButtonMachine,
        "Execute machine and input on server": async ({
          input,
          machineCode,
        }) => {
          const response = await fetch(
            "http://localhost:8080/execute-machine",
            {
              method: "POST",
              headers: {
                "Content-Type": "application/json",
              },
              body: JSON.stringify({
                input,
                machineConfig: JSON.parse(machineCode),
              }),
            }
          );
          const rawResponseBody = await response.json();

          assertIsServerResponseForMachineExecution(rawResponseBody);

          return rawResponseBody;
        },
      },
    }
  );
