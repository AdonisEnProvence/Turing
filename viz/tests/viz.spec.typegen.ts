// This file was automatically generated. Edits will be overwritten

export interface Typegen0 {
  "@@xstate/typegen": true;
  eventsCausingActions: {
    "Assign machine configuration to context": "Set machine configuration";
    "Assign input to context": "Set input";
    "Assign server execution will fail to context": "Make server execution fail";
  };
  internalEvents: {
    "xstate.init": { type: "xstate.init" };
  };
  invokeSrcNameMap: {};
  missingImplementations: {
    actions: "Assign machine configuration to context";
    services: never;
    guards: never;
    delays: never;
  };
  eventsCausingServices: {};
  eventsCausingGuards: {
    "Machine execution is going to fail": "Load machine";
    "Input is invalid": "Load machine";
    "New input is different than previous input": "Set input";
    "Machine execution has succeeded": "Load machine";
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
