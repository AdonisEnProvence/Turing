import { assign, createMachine, send } from "xstate";
import { AutomaticPlayingDelayMode, MachineExecution } from "../types";

import unaryAddMachine from "../../../our-machines/unary_add.json?raw";
import { submitButtonMachine } from "./submit-button";

export const vizMachine =
  /** @xstate-layout N4IgpgJg5mDOIC5QDUCWsCuBDANqgXmAE4B0AggA4V4DGWALqgPYB2ABOm0WFhAJ4kAslhZYoqFlDYA3dNjyEibMAA8wNDI1YkACjix8JU2PTAVY5TUwC2DVDTbUDRtkwBmbgMR6DiUBSZYVC0WPxAVRABaAA4AJhIATgBGAFYEgGYAFnTY2IAGWIB2FKSAGhA+KNikhJJY9IA2PIaU6LyUlPTolMyAX17ytExcAmJyKlo7Vg5YLh5+IRExF1lhhWJlNQ0Q3X1DSTYTMwsyK1tGByd9qVZvLAxYMDCAoJCwiIRIpNjapMyGn5dXK5b6xcqVT5JP4kVoNbINdIpZpddJJfqDOQjRTjaj2KbsTjcXgCYSicQHVbyUZKVTqTTMFi7ZwHI7mSz0Gx2S57Fy3FQmBhgEhYNymIgACnuHPO9kcPIOEDAewAlJ4hlTsZRcXQQjM5sTFmSVpj1jStvTtD5rodTGzTtKuXLmTdQkgQC9ggz3ohYpkYaiOo0ks0MvVCuCoplatk8oUsnlMtFMvk0QMQOqsWMtZNdYT5iSluSpJTM2a6TsrS5WbBPAA5VT0G1mZ6BT2sb2fdJ5dJ1DopQoNBIJzKFJKIiOd9KFEiFX25Nr1eEpdHpk3UnE5hl6okLUnLClrxSbcsMpnW6ueFuvL1uj7-X7RBd5ArpBJxhITyL5HuFAoFBqFJkQEJoUK4ZqaG54rmsw7gWRoHms1LHtsp4AEo8DQAAWkDKCwECuG4TbmJ46GPI21ZXm2rqgB8AEpCQ6Svg03RJMxv6jhOeQwtESSzgkmQdAOSJ-OkYGHlmExQVueYGnuRYyOJZYodoACSEA4GAngAKKiAARhpTrWrSynsBRboem8t5RMxtQJK0QElIBXaDp++R+q0z6ZFCXZeX8fRpuB67ZlJ0wybuhbGohR7GRaLCeAAymAjZSpyFyGS4ip7JRlk0YgUI9CQAFeUBjFNF5n5BjCLSFHEDRFSkCLLgFimQTq0kwfmhr7sWinIbFngADJMLw2U3rlnwIn6qKIvx-a8YUA6flGXEjlOjSZH+jSxE1GJRRJ2r4tunVyS4thYRIYBsCI+ESBQmh9TsakaYNw0QKN7ZWZ8q0zjVPEAVORTlRUPqFLUxSsaicZtH5O2rntpDBW1oUdbJEUHGdmEXVdeEcCwd2NjFOxaeajDo1g50sJd124-jngQKwQoSNITAANZCoFmqSUjBIo+F8FSBjWPU7d92E6exMnmTFNUzjIv0AgTNMNzADaeQALrvdR4R5bOJAOc09SxjUI4Tr69HFAkTTRHV21FG+Ynw61h1hXB3VsILlPYzdeOiyT4t+1LmOe8LPv0J4xBEEwpBOPQbhR9YJAc-tm7I-qfNux7Mve-jD3+5LAvk0HWc05oCssMzKvq5rHZfIBiSWwk-EiVkSRxBOI5+tkbT8Xkwat90-m7RqychTzaeu-JmdeyXBN+9oABiSVYThU8hznYu3ENI3ma2OXawg34wgt+Vwi03aMe3r568U-YAd01vW6BzWO4jzu8xPp2F0Lsuh7nC9YKgDS+EOQPUuqvH+tMt5vR3teD641IjJiSCQaIltWKxDiPCZME5Og-lSBtX0XY1pPyHqWJ20Fx5dUnl-YOEDfYnluIlRsctq6fUiHZP0v08hDl4o3Rupsmi9jqkVZiHRuwO2HgjLmb8KEnUDt-bOdCTIJSSu7ahl0aBMEVCw+BCQ4h1GmtUJMgF0EpAnF0P0j5ch1SnEOEC-Q0wsE0XAMISdJEHXIbBShkUJF-0ZJWFktoThnEdFcXkHhtH70iK+eiPdVoZEtrOaIn5eI9jiMGNoXQoTRFRNEcRpDX4eOOmjHq8NfFnirIE9kqVZShIOHA90u8xr7yyMgryb5AIoI6axZJcZkHVG+N8V8yZ755IggU9qMjikKVKRvPx8pjCBIiR8BBNREgbTiUxRJS1cgzhqC0VuxVEzFFGUFKRhTUb82mT42ZJB0KFxwmAHG7giLwBgVRDs3RUmzliI1R8WQ+HA0hK0OoBDYS2OyXkXJz8JFkImZ42RJTrlz0ZE9J4by97LLiLUBqORRxpJsUkwFX4oQkEhWkAS7QkyIihSQsZZy4VFMuSWU0villRE+VVXFrdqgEs-P2ei95WIFHyAmboJzObuIZRcjOajp5yzKaitlE0aoMWSLo9BTRcUfkBUUHsZshwLTaBChI4qR7cyOtKqh0s5W-xuRLFCciaEKPoEqqJkKSC8QAgiBIPxLYNFNqkGcUZGJZAEq+UMpq3EpzHvCqZ4DnVlMXvQZe+F40z1ZeippHw0jxHqMbHyPrG5gh1SOGcJReJJmSDxf8kbYWp1jZctN8qbnzwAUAtgIDCZgNlWvTQrqijTkTBqroSYCjBnDICgESDmKMWyVGOqiDa3jPrYymV1re2z3oVrCyWaoi-niK+GocRrbdjthOP48Riivi4QCKGv4GhLvpSu4krrgzNH0WOQxHcTGflnD2ICs4WgCSKN2GlcMJGvu7FxPNOQDnGO6L+y2etkw2RA6OBoqZ+hAA */
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
                  entry: "Cache input and machine code into context",
                  exit: "Exit loading state from submit button",
                  invoke: {
                    src: "Execute machine and input on server",
                    onDone: [
                      {
                        actions: [
                          "Assign machine execution to context",
                          "Reset step index",
                        ],
                        target: "Fetched machine and input execution",
                      },
                    ],
                    onError: [
                      {
                        target: "Failed to execute machine and input",
                      },
                    ],
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
                'Content-Type': 'application/json'
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
