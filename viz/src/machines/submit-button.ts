import { ActorRefFrom, createMachine, sendParent } from "xstate";

export const submitButtonMachine =
  /** @xstate-layout N4IgpgJg5mDOIC5QGUCuAjAtgSwC4AJ1VdcB7AOwDoBJCAGzAGIAZUgQwkVAAdTY9sFLiAAeiALQBWAGyUALAE4A7EskAGAEzSAjNIWSlAGhABPRHIAclNWuka12gMxKLjhfYUBfT8bRY8hMRkVKwc2ORQjABi4diwABaQ+HTsEOFQwrz8uILkwmIIjpKSlK7S0nIaFhracroKxmYI2kqyFnKSFpJOHWpy0t6+GDgERCQUlKFpEYwAogBO85CZfAJCSKKIbo6UdnWOXZIKatXFjRKOatbaFl0ajhoKN9KOdoMgfiOB41RoAMZ-OCwRgiWC4Ni4MCUNgAM0h8wAFPYbABKRifAJjYKUf6A2DwDZZNZ5DYFcTaNRKXYKRxFWnaGmvSTnBAaVqUFyOORyJQM6QWClKd4Y0ZBCYLeakeYgsEQqGw+FImxqNEi77YiVSlbZXL5CRVWR6PoKRSVBT85ws8n3UrlCw2JTuAWObQabw+EDkUgQODCNVYia0Bja4l6hDiNnaawHZy1TrFZxyFnSK4KfRyE6utPdV7C4aYsUhVLpEM5dagAqvOS7e5Fe52CmWZOp9OZx76JwDD3+ws41AAoGl3WkrYvSgGaStTpKWncoymRBskoadQ6dyKY4mvP+UU-Sia+ZD8ubQqqceqU5dCyqBxJhfhpxXZyRioqPqKbdfAMknirMs-k9xBdK4jVNE1HgtecmgjNQdleAVKX5VQ01cd1PCAA */
  createMachine(
    {
      tsTypes: {} as import("./submit-button.typegen").Typegen0,
      schema: {
        events: {} as
          | { type: "Load" }
          | { type: "Finished loading" }
          | { type: "Erred" },
      },
      initial: "Idle",
      states: {
        Idle: {
          on: {
            Load: {
              actions: "Forward button has been clicked to parent",
              target: "Loading",
            },
          },
        },
        Loading: {
          on: {
            "Finished loading": {
              target: "Success",
            },
            Erred: {
              target: "Error",
            },
          },
        },
        Success: {
          after: {
            "2000": {
              target: "Idle",
            },
          },
        },
        Error: {
          after: {
            "2000": {
              target: "Idle",
            },
          },
        },
      },
      id: "Submit button",
    },
    {
      actions: {
        "Forward button has been clicked to parent": sendParent({
          type: "Load",
        }),
      },
    }
  );

export type SubmitButtonActorRef = ActorRefFrom<typeof submitButtonMachine>;
