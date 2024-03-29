// This file was automatically generated. Edits will be overwritten

export interface Typegen0 {
  "@@xstate/typegen": true;
  internalEvents: {
    "xstate.after(automatic playing delay)#Visualizer.Application is ready.Managing visualizer execution.Playing steps.Automatic playing on": {
      type: "xstate.after(automatic playing delay)#Visualizer.Application is ready.Managing visualizer execution.Playing steps.Automatic playing on";
    };
    "xstate.after(1000)#Visualizer.Application is ready.Managing machine and input execution.Executing machine and input.Delaying loading state.Pending": {
      type: "xstate.after(1000)#Visualizer.Application is ready.Managing machine and input execution.Executing machine and input.Delaying loading state.Pending";
    };
    "done.invoke.Visualizer.Application is ready.Managing machine and input execution.Executing machine and input.Making request to server.Sending request:invocation[0]": {
      type: "done.invoke.Visualizer.Application is ready.Managing machine and input execution.Executing machine and input.Making request to server.Sending request:invocation[0]";
      data: unknown;
      __tip: "See the XState TS docs to learn how to strongly type this.";
    };
    "error.platform.Visualizer.Application is ready.Managing machine and input execution.Executing machine and input.Making request to server.Sending request:invocation[0]": {
      type: "error.platform.Visualizer.Application is ready.Managing machine and input execution.Executing machine and input.Making request to server.Sending request:invocation[0]";
      data: unknown;
    };
    "xstate.init": { type: "xstate.init" };
    "": { type: "" };
    "done.invoke.Submit button": {
      type: "done.invoke.Submit button";
      data: unknown;
      __tip: "See the XState TS docs to learn how to strongly type this.";
    };
    "error.platform.Submit button": {
      type: "error.platform.Submit button";
      data: unknown;
    };
  };
  invokeSrcNameMap: {
    "Start submit button machine": "done.invoke.Submit button";
    "Execute machine and input on server": "done.invoke.Visualizer.Application is ready.Managing machine and input execution.Executing machine and input.Making request to server.Sending request:invocation[0]";
  };
  missingImplementations: {
    actions: never;
    services: never;
    guards: never;
    delays: never;
  };
  eventsCausingActions: {
    "Assign automatic playing delay to context": "Set automatic playing delay";
    "Increment step index":
      | "Next step"
      | "xstate.after(automatic playing delay)#Visualizer.Application is ready.Managing visualizer execution.Playing steps.Automatic playing on";
    "Reset step index": "Reset steps" | "Load";
    "Assign input to context": "Set input";
    "Assign machine code to context": "Set machine code";
    "Assign machine execution to context": "done.invoke.Visualizer.Application is ready.Managing machine and input execution.Executing machine and input.Making request to server.Sending request:invocation[0]";
    "Assign execution server error to context": "done.invoke.Visualizer.Application is ready.Managing machine and input execution.Executing machine and input.Making request to server.Sending request:invocation[0]";
    "Cache input and machine code into context": "Load";
    "Reset execution server error": "Load";
    "Reset that unknown error has occured while sending request": "Load";
    "Assign unknown error has occured while sending request to context": "error.platform.Visualizer.Application is ready.Managing machine and input execution.Executing machine and input.Making request to server.Sending request:invocation[0]";
    "Allow to play execution steps":
      | "done.state.Visualizer.Application is ready.Managing machine and input execution.Executing machine and input"
      | "Load";
    "Exit loading state from submit button": "done.state.Visualizer.Application is ready.Managing machine and input execution.Executing machine and input";
    "Enter error state from submit button":
      | "Load"
      | "done.state.Visualizer.Application is ready.Managing machine and input execution.Executing machine and input";
  };
  eventsCausingServices: {
    "Start submit button machine": "xstate.init";
    "Execute machine and input on server": "Load";
  };
  eventsCausingGuards: {
    "Has reached end of steps": "";
    "Machine configuration is not valid JSON": "Load";
    "Unknown error has occured while sending request": "done.state.Visualizer.Application is ready.Managing machine and input execution.Executing machine and input";
    "Known error has occured while sending request": "done.state.Visualizer.Application is ready.Managing machine and input execution.Executing machine and input";
    "Is valid machine execution": "done.invoke.Visualizer.Application is ready.Managing machine and input execution.Executing machine and input.Making request to server.Sending request:invocation[0]";
  };
  eventsCausingDelays: {
    "automatic playing delay":
      | "Play"
      | "xstate.after(automatic playing delay)#Visualizer.Application is ready.Managing visualizer execution.Playing steps.Automatic playing on";
  };
  matchesStates:
    | "Application is ready"
    | "Application is ready.Managing visualizer execution"
    | "Application is ready.Managing visualizer execution.Playing steps"
    | "Application is ready.Managing visualizer execution.Playing steps.Automatic playing off"
    | "Application is ready.Managing visualizer execution.Playing steps.Automatic playing on"
    | "Application is ready.Managing visualizer execution.Reached end of steps"
    | "Application is ready.Managing visualizer execution.Idle"
    | "Application is ready.Managing machine and input execution"
    | "Application is ready.Managing machine and input execution.Idle"
    | "Application is ready.Managing machine and input execution.Executing machine and input"
    | "Application is ready.Managing machine and input execution.Executing machine and input.Making request to server"
    | "Application is ready.Managing machine and input execution.Executing machine and input.Making request to server.Sending request"
    | "Application is ready.Managing machine and input execution.Executing machine and input.Making request to server.Received response"
    | "Application is ready.Managing machine and input execution.Executing machine and input.Making request to server.Unknown error occured"
    | "Application is ready.Managing machine and input execution.Executing machine and input.Making request to server.Known error occured"
    | "Application is ready.Managing machine and input execution.Executing machine and input.Delaying loading state"
    | "Application is ready.Managing machine and input execution.Executing machine and input.Delaying loading state.Pending"
    | "Application is ready.Managing machine and input execution.Executing machine and input.Delaying loading state.Reached end of delay"
    | "Application is ready.Managing machine and input execution.Fetched machine and input execution"
    | "Application is ready.Managing machine and input execution.Failed to execute machine and input"
    | "Application is ready.Managing machine and input execution.Failed to execute machine and input.Known server error"
    | "Application is ready.Managing machine and input execution.Failed to execute machine and input.Unknown server error"
    | "Application is ready.Managing machine and input execution.Failed to execute machine and input.Invalid machine configuration"
    | {
        "Application is ready"?:
          | "Managing visualizer execution"
          | "Managing machine and input execution"
          | {
              "Managing visualizer execution"?:
                | "Playing steps"
                | "Reached end of steps"
                | "Idle"
                | {
                    "Playing steps"?:
                      | "Automatic playing off"
                      | "Automatic playing on";
                  };
              "Managing machine and input execution"?:
                | "Idle"
                | "Executing machine and input"
                | "Fetched machine and input execution"
                | "Failed to execute machine and input"
                | {
                    "Executing machine and input"?:
                      | "Making request to server"
                      | "Delaying loading state"
                      | {
                          "Making request to server"?:
                            | "Sending request"
                            | "Received response"
                            | "Unknown error occured"
                            | "Known error occured";
                          "Delaying loading state"?:
                            | "Pending"
                            | "Reached end of delay";
                        };
                    "Failed to execute machine and input"?:
                      | "Known server error"
                      | "Unknown server error"
                      | "Invalid machine configuration";
                  };
            };
      };
  tags: never;
}
