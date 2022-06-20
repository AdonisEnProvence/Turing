import { ActorRefFrom, createMachine, sendParent } from "xstate";

export const submitButtonMachine =
  /** @xstate-layout N4IgpgJg5mDOIC5QGUCuAjAtgSwC4AJ1VdcB7AOwDoBJCAGzAGIAZUgQwkVAAdTY9sFLiAAeiALQBOACyUAjJIDMANgDsADgBMmgKzrJABgPSANCACeEuZQO7py7aoOLJO5ZM3KAvl7NoseITEZFSsHNjkUIwAYhHYsAAWkPh07BARUMK8-LiC5MJiCNI6BvIGOpqKzgaqhk46ZpYIapTSSnJymgptmk7SPn4YOAREJBSUaADGk3CwjCKwuGy4YJRsAGYrAE4AFJpGBgCUjP7DQWNUUzOw8Egg2QJCd4WaWvLKcqq96jrSmn-KZSNRD7TSUZTqdwyaQGOQGZTGTQ+XwgcikCBwYSnQKjEI0ehgLJ8R75Z5WRStDr-TSGRSqRTSaSKRTAhByaSqSiSVRuP6SOTqDrFVQDEDYkbBcZhdKRIk5PIFEHKHSUH5tIzMuqw1kIrmuGGCmmuOQqUXi854q6zOUkxXNWw2STqVTSdQ-XQyOSs9mc7m8mkCoXeFHm3FPHjE3Lh0RWAXyJRqLS6fRGUwWCQumz7N0IumuKpI5FAA */
  createMachine(
    {
      tsTypes: {} as import("./submit-button.typegen").Typegen0,
      initial: "Idle",
      schema: {
        events: {} as { type: "Load" } | { type: "Finished loading" },
      },
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
          },
        },
        Success: {
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
