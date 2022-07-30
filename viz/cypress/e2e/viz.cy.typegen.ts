// This file was automatically generated. Edits will be overwritten

export interface Typegen0 {
  "@@xstate/typegen": true;
  internalEvents: {
    "xstate.init": { type: "xstate.init" };
  };
  invokeSrcNameMap: {};
  missingImplementations: {
    actions: never;
    services: never;
    guards: never;
    delays: never;
  };
  eventsCausingActions: {
    "Assign machine configuration to context": "Set machine configuration";
    "Assign input to context": "Set input";
    "Assign server execution will fail to context": "Make server execution fail";
    "Save old machine configuration and input": "Load machine";
  };
  eventsCausingServices: {};
  eventsCausingGuards: {
    "Machine execution is going to fail": "Load machine";
    "Input is invalid": "Load machine";
    "Configuration is different than last executed configuration":
      | "Set input"
      | "Set machine configuration";
    "Machine execution has succeeded": "Load machine";
    "Configuration is same as last executed configuration":
      | "Set input"
      | "Set machine configuration";
  };
  eventsCausingDelays: {};
  matchesStates:
    | "Machine execution"
    | "Machine execution.Page loaded"
    | "Machine execution.Machine loaded"
    | "Machine execution.Machine execution failed"
    | "Machine execution.Input is invalid"
    | "Editor state"
    | "Editor state.Synchronized"
    | "Editor state.Stale"
    | {
        "Machine execution"?:
          | "Page loaded"
          | "Machine loaded"
          | "Machine execution failed"
          | "Input is invalid";
        "Editor state"?: "Synchronized" | "Stale";
      };
  tags: never;
}
