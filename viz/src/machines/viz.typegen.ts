// This file was automatically generated. Edits will be overwritten

export interface Typegen0 {
  "@@xstate/typegen": true;
  eventsCausingActions: {
    "Assign automatic playing delay to context": "Set automatic playing delay";
    "Increment step index":
      | "Next step"
      | "xstate.after(automatic playing delay)#Visualizer.Playing steps.Automatic playing on";
    "Reset step index": "Reset steps";
  };
  internalEvents: {
    "xstate.after(automatic playing delay)#Visualizer.Playing steps.Automatic playing on": {
      type: "xstate.after(automatic playing delay)#Visualizer.Playing steps.Automatic playing on";
    };
    "": { type: "" };
    "xstate.init": { type: "xstate.init" };
  };
  invokeSrcNameMap: {};
  missingImplementations: {
    actions: never;
    services: never;
    guards: "Has reached end of steps";
    delays: never;
  };
  eventsCausingServices: {};
  eventsCausingGuards: {
    "Has reached end of steps": "";
  };
  eventsCausingDelays: {
    "automatic playing delay": "xstate.init";
  };
  matchesStates:
    | "Playing steps"
    | "Playing steps.Automatic playing off"
    | "Playing steps.Automatic playing on"
    | "Reached end of steps"
    | { "Playing steps"?: "Automatic playing off" | "Automatic playing on" };
  tags: never;
}
