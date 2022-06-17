// This file was automatically generated. Edits will be overwritten

export interface Typegen0 {
  "@@xstate/typegen": true;
  eventsCausingActions: {
    "Assign automatic playing delay to context": "Set automatic playing delay";
    "Increment step index":
      | "Next step"
      | "xstate.after(automatic playing delay)#Visualizer.Application is ready.Managing visualizer execution.Playing steps.Automatic playing on";
    "Reset step index": "Reset steps";
    "Assign input to context": "Set input";
    "Assign machine code to context": "Set machine code";
    "Exit loading state from submit button": "xstate.after(2000)#Visualizer.Application is ready.Managing machine and input execution.Executing machine and input";
    "Assign machine execution to context": "xstate.after(2000)#Visualizer.Application is ready.Managing machine and input execution.Executing machine and input";
    "Cache input and machine code into context": "Load";
    "Allow to play execution steps": "xstate.after(2000)#Visualizer.Application is ready.Managing machine and input execution.Executing machine and input";
  };
  internalEvents: {
    "xstate.after(automatic playing delay)#Visualizer.Application is ready.Managing visualizer execution.Playing steps.Automatic playing on": {
      type: "xstate.after(automatic playing delay)#Visualizer.Application is ready.Managing visualizer execution.Playing steps.Automatic playing on";
    };
    "xstate.after(2000)#Visualizer.Application is ready.Managing machine and input execution.Executing machine and input": {
      type: "xstate.after(2000)#Visualizer.Application is ready.Managing machine and input execution.Executing machine and input";
    };
    "": { type: "" };
    "xstate.init": { type: "xstate.init" };
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
  };
  missingImplementations: {
    actions: never;
    services: never;
    guards: never;
    delays: never;
  };
  eventsCausingServices: {
    "Start submit button machine": "xstate.init";
  };
  eventsCausingGuards: {
    "Has reached end of steps": "";
  };
  eventsCausingDelays: {
    "automatic playing delay": "xstate.init";
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
    | "Application is ready.Managing machine and input execution.Fetched machine and input execution"
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
                | "Fetched machine and input execution";
            };
      };
  tags: never;
}
