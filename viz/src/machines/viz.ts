import { assign, createMachine, send } from "xstate";
import { AutomaticPlayingDelayMode, MachineExecution } from "../types";
import { submitButtonMachine } from "./submit-button";
import { getMachineCodeAndInput } from "../constants/sample-machines";

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

const { machineCode: defaultMachineCode, input: defaultInput } =
  getMachineCodeAndInput("unary_add");

export const vizMachine =
  /** @xstate-layout N4IgpgJg5mDOIC5QDUCWsCuBDANqgXmAE4B0AggA4V4DGWALqgPYB2ABOm0WFhAJ4kAslhZYoqFlDYA3dNjyEibMAA8wNDI1YkACjix8JU2PTAVY5TUwC2DVDTbUDRtkwBmbgMR6DiUBSZYVC0WPxAVRABaACYAVgAOEgBOeIA2AEYAFmiABgBmTPSk9PSAGhA+KOz0klSkgHZ6rLz62Jy6gF8O8rRMXAJicipaO1YOWC4efiERMRdZPoViZTUNEN19Q0k2EzMLMitbRgcnLalWbywMWDAwgKCQsIiESLyc2JJo9NS29NiCvLxTJpcqVF55dL1Eg5YE5IFJXLfeLxLo9OT9RRDaj2UbsTjcXgCYSicTbBbyAZKVTqTTMFgbZzbXbmSz0Gx2E6bFwXFQmBhgEhYNymIgACiubKO9kcXO2EDAmwAlJ5ehTMZRsXQQuNJoSZiT5uillTVrTtD4zjtTCyDpKOTLGedQkgQPdgnSnlFotEobF6hlonkkj9AcDoqCvQUSHkfci-u9UtFgyjuiBVRjBhqRtr8VMibNSVJyRmTTT1haXMzYJ4AHKqehWsx3QLu1iel6xVKJQrpNKxdJfH4-CMdwq1f6pQo5adJHLRTKotNGylY7N0nUE6bEuZk5eKFZlukMy1VzzNh4el3PJN5EglerxAcQ3vpOf1EeRTLtO+pPKpVoQkGeT9nki7psaq44jmEybvmBq7oslIHmsR4AEo8DQAAWkDKCwECuG4jbmJ46E3A2Vbnq2zqgM8mRJEknx1JkjSZJ2sSZMBH6IiQ8T1PRRQlC0PzZGBe6ZsMUHrrmerboWMhiaWKHaAAkhAOBgJ4ACiogAEbqQ6lrUkp7AUS6bqPFeUSztE0J-nOXadgiJQfm8mR3ik0TxNZfGxN6omIeqElalJMF5vqO5FgpyFmiwngAMpgA2ErsscBkuPKmyURZNGICU7G1PUhSZBxdmFC5GQkGxD7RKk-6sb+sT+Wq4mariG5hbJhoBcsRkxZ4AAyTC8Fll45S8j45NG8SAnOnlBj2H51DZSTAaGDR-LV9RNSWkHBWM0lbgWLi2FhEhgGwIj4RIFCaNF6yaaajDbCdmFnRdeEcCwN30DMADWLjcAAjhgcANmyOzENIgwJXhANgMDoOeBArAChI0hML9ArgSuWaSftoUyUdz1YKdLDnZdn3fXdR4PYexOk+TH3XZof1wwjJhsODNxEFDpAwxAbMgyYCBo0we0sAA2jkAC6I1tpZCCFQxDSsb2U50UkmQjq+RQ8ZOuQFMCGTpKBqbY4FrXQbqh3wVIL1vRTzMNr192PcdJOvWT71XV9LPCP92xA0LYNMBDPPQ2AsOB-DweeMQRBMKQTj0G4ifWCQ5stWu+PW3BEVsPbXuO77ztu9otMofTnuMz732s9H7Mh2HvMkPzgugyLLDo+LUuy2ZLbZeEuX9rUf6eRNNX8Xk2s5LraTZPkxVpCUC5mwpu1tQdedyYXNeU7dLs027VcO0zJckAAIgqjpsDgQ0C0y9D8rokcP1Ani8k-piCsKxCiq+05lSZ1ILjcW7VCa2wLh7U+tcD5l3pBXWkJ8i5nzrlfWUUg768ErF-AUOhX5GDltRIeCBfw2SBK+eoQZ-RwlaCOVI+RkjxASHCV8j5-TpG2hBUBm8CY23zrvb2+9S6HnLsfO20CUGwPoEjFGJA+Tf2ARvK2sFwo70kXvJ21MxF0wkQzIRTsiHtgKDUX8XY4jMT-A+JI2s2gMWYr5YCvlmH9hTGibqICgq8Nzmo92+ji5U0PhcBKDZDH9wvPLMakQshpBIFQ-4+Rpx-AHFrCoiA4hQjIRCYEK8uzAS4TjLxKiOpEz0dXAxJdtGxRCVA-RNAmDyiMQraJ-Y3KQnhL+CE+RUgjkaDUOxxVfIDliCkVe7jmqeMtiFHxnVkGaMqUE2Kg1hrhKou2SIDlkgxnaPEdoZiHzaw4skB87SHLfDiF0VMLAGlwDCEonhxSIH52LMaKpx4cF7FZClaUpxuQeCaVEv0NlZ47O+K4v8qSwSvH9J8eIPpPJJDaE0P8BSLbZzxHw7eXUJlvIrI-T5tpvmchvpE10A9RokOiWOb0cK3jennH8ViLkYWeR9HUXZXkRmoqznjDFMzSnyQ8bijBRF4CrMHs8SIIzUjQlfF2MFvYIWLTiLUViEJNZUPnE0blkz0XgP4XJF5SFFkkHQh7HCr8CKioBSQ-8UJdm5FiAkDh89yqTS7AwnI9Q4jBhNo1NeHjlHTNUbMyKQqTWqXUjayVACPidOYoivIwESofkKLedirQVqJj4rVC5AaJlBpziGgVRr9yLOjVEV8SZR4FB8km9iSaPyNBlQiJoPoqEcL4jqwtfLi2QMEQEuBoj6SRtuOKilMb2huU5e0WcMIap+kOR8QM2QiolAaCkLa+adoPODSU-tGiKmBPgSQRBT0ykwOEfXKQQdQac1DtzFubcG7BwrQgb0t4NXFThLs4MX5YjaxXneLyj5gy1qTW4pcgbd1Fv3QIw9g6RHGVPeI2p5TEPXsmI3e9zdBjoRoGAVAUN8LcFgAEFgNw30lGYdCL1msnXFGXjPOiPEIRwr4oisFYyoMFpg72uD6j-GoKHchs9fj0PCZ+v7duHMuaQ0GAAVRYL9a5AB3dg8dE6uBoBobgEAqMQlqPkP4XxXyAk8pCxAQJlr0TnMw9oQZAzdr4-qrFcyj0iZiih3RaHL1O0w7e2TD75OkAANJqY00QBOSgxa6cgG+uF7rhkBkyF2GE8RtbPmjLOfiZjfzeuc0UvdTzBMSekW8sT7mMPSZfXeuT4ciBUa9UZiEQyzNwqBL0potHvXMMKovEohWpmwZK+JvzCyT2VYvVIq96Cb5YLflaZ++Co5QDfRxNyN5IS9j9ImYZI4RlQgTX+yEMJ1VDb1VvXxVXJMVdQwOyTl9r6WgWzg5+ZqsIWo+u4NgGVfDjtJc8R8y7vXZs45CGqB2GgkGO5OU7HEigXd5a567035nHuHd5yuaOPM-Tmy9++b3TDrdfJ8Fa22+t7biAdlVwI4WuJGUmDiSOwFXdDb5mbWiTVTY5+jzQCXbwMNa6Z-IHXLMIF7DKp1CQZe9hA1Qln3i+3waE+Vk1AAxRKn38IPbV-Aprv4eL9hqoUXi8btYZA+OTr1h3po-C3eMndRWRsGrG5zibmP1dYFQOpfC4MXbnV11e8LTB1O4apFFxOb6NksvjImQM7E-Ti-a9CDi7GgwMM8orx5rubt6899733OGA+89xyQJTKnQ8mRC8oSPjWAfEMlZswMCI4irXM9PNJCANWVX-BChI2RarZ+K7nnHiG3le59zhf3j1A8Ice8pLu-QdeHvqSwNwqAoAYCILiaPKeLOpZ-QiZiyf2hQj+A0EZgkvJOe3dw53-HRt5+ERPwv0-Q4l6D2E-w5LAeVvaDZAfgwsiMfoVL0siJVCtJmqGHlNEMPi7m5mPrduWg3usmwokDOnUNOIPoul3iMkwpONKgAbVPknfoUsNo-vwHvv8IkO0nRJ0mZj0l3pEAiJ+rPBkHGHOJrKkDqtQYmHeA+PQUmowR+PRB8F5DGA0Kyr+CtJch0EAA */
  createMachine(
    {
      context: {
        stepIndex: 0,
        automaticPlayingDelayMode: "MEDIUM",
        input: defaultInput,
        machineCode: defaultMachineCode,
        lastLoadingInput: undefined,
        lastLoadingMachineCode: undefined,
        machineExecution: undefined,
        serverExecutionError: undefined,
        unknownErrorHasOccuredWhileSendingRequest: false,
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
          unknownErrorHasOccuredWhileSendingRequest: boolean;
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
                    "Reset that unknown error has occured while sending request",
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
                                target: "Known error occured",
                              },
                            ],
                            onError: [
                              {
                                target: "Unknown error occured",
                              },
                            ],
                          },
                        },
                        "Received response": {
                          type: "final",
                        },
                        "Unknown error occured": {
                          entry:
                            "Assign unknown error has occured while sending request to context",
                          type: "final",
                        },
                        "Known error occured": {
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
                  onDone: [
                    {
                      cond: "Unknown error has occured while sending request",
                      target:
                        "#Visualizer.Application is ready.Managing machine and input execution.Failed to execute machine and input.Unknown server error",
                    },
                    {
                      cond: "Known error has occured while sending request",
                      target:
                        "#Visualizer.Application is ready.Managing machine and input execution.Failed to execute machine and input.Known server error",
                    },
                    {
                      target: "Fetched machine and input execution",
                    },
                  ],
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
        "Assign unknown error has occured while sending request to context":
          assign({
            unknownErrorHasOccuredWhileSendingRequest: (_context) => true,
          }),
        "Reset that unknown error has occured while sending request": assign({
          unknownErrorHasOccuredWhileSendingRequest: (_context) => false,
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
        "Known error has occured while sending request": ({
          serverExecutionError,
        }) => serverExecutionError !== undefined,
        "Unknown error has occured while sending request": ({
          unknownErrorHasOccuredWhileSendingRequest,
        }) => unknownErrorHasOccuredWhileSendingRequest === true,
      },
      services: {
        "Start submit button machine": submitButtonMachine,
        "Execute machine and input on server": async ({
          input,
          machineCode,
        }) => {
          const response = await fetch(toApiUrl("/execute-machine"), {
            method: "POST",
            headers: {
              "Content-Type": "application/json",
            },
            body: JSON.stringify({
              input,
              machineConfig: JSON.parse(machineCode),
            }),
          });
          const rawResponseBody = await response.json();

          assertIsServerResponseForMachineExecution(rawResponseBody);

          return rawResponseBody;
        },
      },
    }
  );

function toApiUrl(route: string): string {
  const routeDoesNotStartWithSlash = route.startsWith("/") === false;
  if (routeDoesNotStartWithSlash === true) {
    throw new Error('route must start with "/"');
  }

  return `${import.meta.env.VITE_APP_URL}${route}`;
}
