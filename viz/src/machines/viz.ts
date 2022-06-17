import { assign, createMachine } from "xstate";
import { AutomaticPlayingDelayMode } from "../types";

export const vizMachine =
  /** @xstate-layout N4IgpgJg5mDOIC5QDUCWsCuBDANqgXmAE4B0ACjlgJ6oB2UABLAC5gAOsJAghswPYBbLM1QBjBm0o16DPgDM5AYgrVEoNn1ioRfWmpAAPRAAYSAVgDMAJgsB2AJzGL9gBz37ANmOeANCCqIAIwWACwkgS7OgV5mjk6BtgC+iX5omLgExORSdIws7Jw8-EIi4pLUubK0ylgYsGD6Glo6ekiGiAC0HiTGsSEJLoMWxoE2gWZ+AQhWVt0uZh5mvWZWgSH2tlZJKSBp2HiEpCrSeawc3LyCwmISOTK6igYswmAkWHKsRAAUtcXXZXdGBAwFIAJSKPYZQ7ZCoyfLnIpXUq3WGMXSNTTaVDotpGaYhbo2LYuYweWwhEIuEITfyIKwuQLmJYeDyUikWBYuZKpdD7TJHQFMM6wRQAOTABmYQvYGOa2NaoDxHWGYWcZmimwWHlWLkmnXpJGpTg8azMtlZgXsIW2PPSByyx0q8JFsqxOMViFZpnsfTWowSIVCeoQgXG5mtK1mbhc2tsNt2vKhWQASmAsKIABaQBhgWgQWRyaUcRSp+pS52ulr6PHzWwkQa2XqrWLGek0qYdNw9MxmKlmCn2AMhbkJu38xQAZTAUt+SJu5RODGBUkr8urnUiZm7W0sMdiFgsgWDtkZ-QjHk3ocDyR2tD4wPgbUh9oFqKLhUuJXngvkclX7vaBAOhCYwwmJWxIg5KkA2DaItx9UDFmCYJNnjZ9+RhRdnQuP5kQXSoAKaN0FUAqwFnDaxLXsUJqNZWDFhIBCCXVQ87C2Ed0OhR04WFf8SKVEIDXAyDe36clgw6UMLENakIMjdV3HYnZOJTNNM2zXN83kd8+PXBBXBcQ0bCtS0yMHON6MZVsVh7AkrQcYwuWUxMX103FOmpKwSGEg9RJg2kgIiboKVGRzFNsLUb0SIA */
  createMachine(
    {
      context: { stepIndex: 0, automaticPlayingDelayMode: "MEDIUM" },
      tsTypes: {} as import("./viz.typegen").Typegen0,
      schema: {
        context: {} as {
          stepIndex: number;
          automaticPlayingDelayMode: AutomaticPlayingDelayMode;
        },
        events: {} as
          | {
              type: "Play";
            }
          | { type: "Pause" }
          | { type: "Next step" }
          | {
              type: "Set automatic playing delay";
              mode: AutomaticPlayingDelayMode;
            }
          | { type: "Reset steps" },
      },
      on: {
        "Set automatic playing delay": {
          actions: "Assign automatic playing delay to context",
        },
      },
      initial: "Playing steps",
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
      },
      delays: {
        "automatic playing delay": (context) =>
          context.automaticPlayingDelayMode === "MEDIUM" ? 1000 : 500,
      },
    }
  );
