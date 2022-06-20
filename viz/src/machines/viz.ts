import { assign, createMachine, send } from "xstate";
import { AutomaticPlayingDelayMode, MachineExecution } from "../types";

import unaryAddMachine from "../../../our-machines/unary_add.json?raw";
import { submitButtonMachine } from "./submit-button";

export const vizMachine =
  /** @xstate-layout N4IgpgJg5mDOIC5QDUCWsCuBDANqgXmAE4B0AggA4V4DGWALqgPYB2ABOm0WFhAJ4kAslhZYoqFlDYA3dNjyEibMAA8wNDI1YkACjix8JU2PTAVY5TUwC2DVDTbUDRtkwBmbgMR6DiUBSZYVC0WPxAVRABaAA4AJhIATgBGAFYEgGYAFnTY2IAGWIB2FKSAGhA+KNikhJJY9IA2PIaU6LyUlPTolMyAX17ytExcAmJyKlo7Vg5YLh5+IRExF1lhhWJlNQ0Q3X1DSTYTMwsyK1tGByd9qVZvLAxYMDCAoJCwiIRIpNjapMyGn5dXK5b6xcqVT5JP4kVoNbINdIpZpddJJfqDOQjRTjaj2KbsTjcXgCYSicQHVbyUZKVTqTTMFi7ZwHI7mSz0Gx2S57Fy3FQmBhgEhYNymIgACnuHPO9kcPIOEDAewAlJ4hlTsZRcXQQjM5sTFmSVpj1jStvTtD5rodTGzTtKuXLmTdQkgQC9ggz3ohYpkYaiOo0ks0MvVCuCoplatk8oUsnlMtFMvk0QMQOqsWMtZNdYT5iSluSpJTM2a6TsrS5WbBPAA5VT0G1mZ6BT2sb2fdJ5dJ1DopQoNBIJzKFJKIiOd9KFEiFX25Nr1eEpdHpk3UnE5hl6okLUnLClrxSbcsMpnW6ueFuvL1uj7-X7RBd5ArpBJxhITyL5HuFAoFBqFJkQEJoUK4ZqaG54rmsw7gWRoHms1LHtsp4AEo8DQAAWkDKCwECuG4TbmJ46GPI21ZXm2rqgB8AEpCQ6Svg03RJMxv6jhOeQwtESSzgkmQdAOSJ-OkYGHlmExQVueYGnuRYyOJZYodoACSEA4GAngAKKiAARhpTrWrSynsBRboem8t5RMxtQJK0QElIBXaDp++R+q0z6ZFCXZeX8fRpuB67ZlJ0wybuhbGohR7GRaLCeAAymAjZSpyFyGS4ip7JRlk0YgUI9CQAFeUBjFNF5n5BjCLSFHEDRFSkCLLgFimQTq0kwfmhr7sWinIbFngADJMLw2U3rlnwIn6qKIvx-a8YUA6flGXEjlOjSZH+jSxE1GJRRJ2r4tunVyS4thYRIYBsCI+ESBQmh9TsakaYNw0QKN7ZWZ8KT5CQwbfc02QCcGmQTox8TJltcLdG+hQJGJe2kMFbWhR1skRQcZ2YRdV14RwLB3Y2MU7Fp5qMBjWDnSwl3XXjBOLAA1i43AAI4YHAjYcocxDSGMiV4UzYCs+zngQKwQoSNITD00KgWapJyMEqj4XwVImPYzTt33UTp4kye5OU9TuOa-QDMC0LJhsJzjxEDzpB8xAZtsyYCAS0wCsANp5AAuu91HhD6wYkLVLQdPCvHdBO31+rOiKJttY5CfDGr7ZuKP6sr3VsGrVM4zd+Na6TOuF-rWM5xr+cm8IjMHCzTsc0wXM27zYD8zXgt154xBEEwpBOPQbg99YJCyynIWK+ncGZ9nht5wTD1F3rqsU6XM+05optt+b9eN7bJD2477Muywkse97vsdpELQ9kmz6AckNUgROzENIV1SPu0DVdjkSelq1h1hZPeS09c5r0JoXbQusUIl3VkbCuJAAAiSpnRsBwK9Ks9BBS6Bbg7SQnh+QYNMMKUUxBxTBmfKqEeiN5b-yVoA06y8YGzwLieCBxcl4GxAcbBBSDrSoN4OgzBOhsFGHPp9L805UTbQ8iOYoz50gTgWn6PIyQoyxDql5bosQf4QSRjQieXUgEMLLrAue2tWGLyzkY1exsRZixIAKQhlC-7QX0SdaBximFgJYYySB9J3HWIrqI8af1o7QxqgBQccYJxeRWgBJM0Q1p2V9NooK1CXGwQMfQjh5dTHgMZAAMSSlhHCwCcnMJMi9Ea5lWw5X9ggYMAESDtF4qkDacYkjRAaE-Rir9uhIhKDxPIqQUlywOuk466N2Er04RXee2h8lYFQBpfCnMiaXVKSYzQlS3rVOvB9cakRGhcX-G5GoLRYgRwqHlIZPZWhdDSFGBILF-K7WTlQsZ7VXGTMsdkzZXiKmJUbMbIJdSvisS4g0YMxReJQ1SB+K5CBcj0TaC+TyCSapdBGaPBWR00Yqx+dMsp-z+qAoJdjGgTBFQgo+F8V8tQchQkhZE5IeRogTgSX6AE95lGJgat0foaYWCUrgGEJxujxl4sziWU0czGSVhZLaE4ZxHRXF5B4alURXz0X4iBV8TFZxsoRV8OMQdqisu7DxDpqJohYveanceGS3E9QRrKs86DjjslSrKVVBx9nuhqWNOpWQg5eRhomN8iZWKfl4tfao3xvivmTN0G1zUEbOM+Y6750qkJmLlfKYwiqNWfFDYkDaq0MgJAAhcpauQZxnKjMxaIQ4dqrjTeKjNEz8XZuinkkg6Fl44WwQRIi8BdlUQ7N0a+s41GIkfFkBI8KIRfFaHUX0s7BzPgSay216a06Zq7b1XNJAnpPDHbUmlcRaifyKB06oU4nmuShE06GAl2hJlnTu9te7O1SsPXkotMROhVRyKOOICcH1Gv7PRblb5UgDgWi2sVaSO2SsMb8zxrqT0AdnNHK1s5BINXKginI2reJDm2hc2GQFP3Ie-ahrJhK-mut8WTKZjDQEbykLXdmlsG7W13vvTedcANqOiE07aWRijToapHb4dQOmpG+mteoKbXm-y-Q6n9aHGMYaPSxhj7GuFVwPhbK23MxjoRoGAVAPN8LcFgAEFgjwi1wgkUMlRWrFwpEjlOGcCZ+wUYif2GjHy6MZ204Z2Zem2Fko8Rx4zQmeNmabkQETCR4gFE6COAL20ukIuyLUYEC1igAguQ0EL9rcXhYM3F42zGYsbM8dw-NKC0EKsEcIyQIntpBzKk0ViAJWLyIRfB7ihHoizjhL5CrY8qt0P8TM3J3iSD6YW0S5ryC+E4ILZg-txT8JDvcGwTKvgz2Bo+Ip36HSF1pCKIxbzI3GmtHG5NoCUIZs4oAZktbTHosWMaxxxBLWtsCNMCJqMvWEz9bqtUBET9+IwnI8GP4cGAQfb0fuqeVjFvlNiithr2OiUAdRC-TLkmcsyYRdUQrt2IY3KKC0dHErqs-d072wp9B9uxYCUtkyRbviQpnJC6dQ4MWJmiR0BifYvIXJu+91Nbzd2afo6z0BrqFlLJwqs0m6zCd-Ow7DGEDV8hmskRxBFLFev3wZ6OKc5WFfqdo8rlnbHatRf-Wdv1F7uxG4BAUYMZuygIr+C-GdbQpysX+EULRDudFO7mwBv49QGJxqZW+Flhql0xxIEBdpr6No8Xt2p00ifc8p8ZaxdPwZM9RFhi-ICd208dCGQK3oQA */
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
        services: {} as {
          "Execute machine and input on server": {
            data: MachineExecution;
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
                Idle: {
                  on: {
                    Load: {
                      target: "Executing machine and input",
                    },
                  },
                },
                "Executing machine and input": {
                  entry: [
                    "Cache input and machine code into context",
                    "Reset step index",
                  ],
                  exit: "Exit loading state from submit button",
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
                                target: "Received response",
                              },
                            ],
                            onError: [
                              {
                                target:
                                  "#Visualizer.Application is ready.Managing machine and input execution.Failed to execute machine and input",
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
                  entry: "Allow to play execution steps",
                  on: {
                    Load: {
                      target: "Executing machine and input",
                    },
                  },
                },
                "Failed to execute machine and input": {
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
          machineExecution: (_, { data }) => data,
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

          return rawResponseBody as MachineExecution;
        },
      },
    }
  );
