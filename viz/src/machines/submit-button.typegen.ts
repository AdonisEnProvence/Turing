// This file was automatically generated. Edits will be overwritten

export interface Typegen0 {
  "@@xstate/typegen": true;
  internalEvents: {
    "xstate.after(2000)#Submit button.Success": {
      type: "xstate.after(2000)#Submit button.Success";
    };
    "xstate.after(2000)#Submit button.Error": {
      type: "xstate.after(2000)#Submit button.Error";
    };
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
    "Forward button has been clicked to parent": "Load";
  };
  eventsCausingServices: {};
  eventsCausingGuards: {};
  eventsCausingDelays: {};
  matchesStates: "Idle" | "Loading" | "Success" | "Error";
  tags: never;
}
