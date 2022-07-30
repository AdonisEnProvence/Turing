/// <reference types="cypress" />

import { test, expect } from "@playwright/test";
import { assign, createMachine } from "xstate";
import { createTestModel } from "@xstate/test";

type MachineType = "unary_add" | "palindrome";

function isInputValid(machineType: MachineType, input: string): boolean {
  switch (machineType) {
    case "unary_add": {
      return [...input].every(
        (character) => character === "1" || character === "+"
      );
    }
    case "palindrome": {
      return [...input].every((character) =>
        ["a", "b", "c"].includes(character)
      );
    }
    default: {
      throw new Error("Unknown machine type");
    }
  }
}

const vizSpecMachine =
  /** @xstate-layout N4IgpgJg5mDOIC5QDUCWAvABLADmAxgHQCyAhvgBaoB2YmYAHgQK4AuqA9tQMQDKYrTAFtyVWpnxcAZqijMATqXZdEoHB1ipl1VSAaIAtACYA7IQCs5owE5zABgDMRgCwAOE0bsmANCACeiACMDoGE1gBsJtbWNoHWgY7WAL5JvmhYuAQkojR0jCzafAKYNDhsuuqa2rr6CA7WdhbODq7mzvbhTnZtvgEIwaERUTHx8YkpaRjYeERklLn0TPhsnDxkANZ0sGDyAG47iwWrmFKkqAA2FRpaqzWIUWYx7q6uEYEm4c6BvUEhYZHRWJjeoTEDpaZZOZiPJLFZcbgAGQ4pAgwhytCuVVuSD0iFMZmarjsdkCrma4XC1gcPwQzhiYQSJi8lNcRiMDhMoPBmSIAFEIFoOPJsKwlGBCLw-NRKPIuBhIEVBKVyjjKjcVDjakZAuZCK9ie1wnFrO1zD5-EEdaE2SZms5mgl2lypjzCPzBcLYKLWOLJdKKLLqPKIIq0fNxJJqDI5IpqqrrnHQLV7aF2s4TKTXJ0up8ae4jIQ7CbwlZ4oFgg5nRkZm6BawhSKxRLRecwIjkaiROGwJj1TpNYhXq49Q47M4jOEx5SPEYaaTbIQvuZJ+9M-FnFWIXy6w2vU3eC22-wldQyqxe4ncXU7MPzK5jXFWoFwq9nHPmg5CCv3q4HG12skqRgi6NbuvWnrer6h6hl20ISNIsgKEo2JqAmKFXnezgWCY5gcpYN5jl4c7hNqhD1K0y6js4N5GCkQHUBwEBwLo3I1lCCz5Ms2iEAACqQMCYOcHaQBe6G1AYbhkZ4HwNCyTJ-jSzS6i8bIvu0E7xJWQGsZC6IwkcXDZN2gnCRAokakm9ykY4pItKycRjtYNIvp+ER2W4T4vJurrseInFwtQRlwf52gnGcrZmfGWIWVek66iYDiJbabkkgpFq0iSFjWGSOHLpYHgOOE3lsXphxcashAAJKnmwJSwCU1C7KQ5yoJFqHRf2lkIGSupFpECXvCapiuIpJEWH+FYUo4lKAZM1a6cZIXoWql7iR4UnMrJ2XyeYNIhOEX5eMuFIOF8p3ssVWRgbukESlKMpyugIlRX2dwIAYTiEMEXj2hOITNPEc43gdpYkad5bxCYriXduHqNj6zbNT2L2rUEJLxflRa2m4HJvul5YUmEd4vJSnhuFEMO1nDe4+uZnVXgYy5faO2N-YEAPfOlwxfY4tr1FElJ0jDdNvQYEQbTJdhyY4u3pXeYQmEyv7bRSZolnRSRAA */
  createMachine(
    {
      context: {
        machineExecutionWillFail: false,
        machineInput: "111+1111",
        previousInput: undefined,
        machineConfiguration: "unary_add",
        previousMachineConfiguration: undefined,
      },
      tsTypes: {} as import("./viz.cy.typegen").Typegen0,
      schema: {
        context: {} as {
          machineExecutionWillFail: boolean;
          previousInput: string | undefined;
          machineInput: string;
          machineConfiguration: MachineType;
          previousMachineConfiguration: MachineType | undefined;
        },
        events: {} as
          | { type: "Load machine" }
          | {
              type: "Set machine configuration";
              machineConfiguration: MachineType;
            }
          | { type: "Set input"; input: string }
          | { type: "Make server execution fail" },
      },
      type: "parallel",
      states: {
        "Machine execution": {
          initial: "Page loaded",
          states: {
            "Page loaded": {},
            "Machine loaded": {},
            "Machine execution failed": {},
            "Input is invalid": {},
          },
          on: {
            "Set machine configuration": {
              actions: "Assign machine configuration to context",
            },
            "Set input": {
              actions: "Assign input to context",
            },
            "Make server execution fail": {
              actions: "Assign server execution will fail to context",
            },
            "Load machine": [
              {
                cond: "Machine execution is going to fail",
                target: ".Machine execution failed",
              },
              {
                cond: "Input is invalid",
                target: ".Input is invalid",
              },
              {
                actions: "Save old machine configuration and input",
                target: ".Machine loaded",
              },
            ],
          },
        },
        "Editor state": {
          initial: "Synchronized",
          states: {
            Synchronized: {
              on: {
                "Set input": {
                  cond: "Configuration is different than last executed configuration",
                  target: "Stale",
                },
                "Set machine configuration": {
                  cond: "Configuration is different than last executed configuration",
                  target: "Stale",
                },
              },
            },
            Stale: {
              on: {
                "Load machine": {
                  cond: "Machine execution has succeeded",
                  target: "Synchronized",
                },
                "Set input": {
                  cond: "Configuration is same as last executed configuration",
                  target: "Synchronized",
                },
                "Set machine configuration": {
                  cond: "Configuration is same as last executed configuration",
                  target: "Synchronized",
                },
              },
            },
          },
        },
      },
      id: "Viz spec",
    },
    {
      actions: {
        "Assign server execution will fail to context": assign({
          machineExecutionWillFail: (_context) => true,
        }),
        "Assign input to context": assign({
          machineInput: (_context, { input }) => input,
        }),
        "Assign machine configuration to context": assign({
          machineConfiguration: (_context, { machineConfiguration }) =>
            machineConfiguration,
        }),
        "Save old machine configuration and input": assign({
          previousInput: ({ machineInput }) => machineInput,
          previousMachineConfiguration: ({ machineConfiguration }) =>
            machineConfiguration,
        }),
      },
      guards: {
        "Machine execution is going to fail": ({ machineExecutionWillFail }) =>
          machineExecutionWillFail === true,
        "Input is invalid": ({ machineInput }) => {
          const isValid = isInputValid("unary_add", machineInput);
          const isInvalid = isValid === false;

          return isInvalid;
        },
        "Machine execution has succeeded": ({
          machineExecutionWillFail,
          machineInput,
        }) =>
          machineExecutionWillFail === false &&
          isInputValid("unary_add", machineInput) === true,
        "Configuration is different than last executed configuration": (
          {
            machineInput,
            previousInput,
            machineConfiguration,
            previousMachineConfiguration,
          },
          event
        ) => {
          const isFirstMachineLoading =
            previousInput === undefined ||
            previousMachineConfiguration === undefined;
          if (isFirstMachineLoading === true) {
            return false;
          }

          switch (event.type) {
            case "Set input": {
              const isConfigurationDifferent =
                previousInput !== event.input ||
                machineConfiguration !== previousMachineConfiguration;

              return isConfigurationDifferent;
            }
            case "Set machine configuration": {
              const isConfigurationDifferent =
                machineInput !== previousInput ||
                previousMachineConfiguration !== event.machineConfiguration;

              return isConfigurationDifferent;
            }
            default: {
              throw new Error("Reached unreachable state");
            }
          }
        },
        "Configuration is same as last executed configuration": (
          {
            machineInput,
            previousInput,
            machineConfiguration,
            previousMachineConfiguration,
          },
          event
        ) => {
          const isFirstMachineLoading =
            previousInput === undefined ||
            previousMachineConfiguration === undefined;
          if (isFirstMachineLoading === true) {
            return true;
          }

          switch (event.type) {
            case "Set input": {
              const isSameConfiguration =
                previousInput === event.input &&
                machineConfiguration === previousMachineConfiguration;

              return isSameConfiguration;
            }
            case "Set machine configuration": {
              const isSameConfiguration =
                machineInput === previousInput &&
                previousMachineConfiguration === event.machineConfiguration;

              return isSameConfiguration;
            }
            default: {
              throw new Error("Reached unreachable state");
            }
          }
        },
      },
    }
  );

const vizSpecModel = createTestModel(vizSpecMachine, {
  eventCases: {
    "Set input": [
      {
        input: "aacbb",
      },
    ],
    "Set machine configuration": [
      {
        machineConfiguration: "palindrome",
      },
    ],
  },
});

describe("Viz", () => {
  vizSpecModel.getPaths().forEach((path) => {
    it(path.description, () => {
      cy.visit("http://localhost:53332");

      path.testSync({
        states: {
          "Machine execution.Page loaded": () => {
            cy.contains("Turing").should("be.visible");
          },
          "Machine execution.Machine loaded": () => {
            cy.contains("Running").should("be.visible");
          },
          "Machine execution.Machine execution failed": () => {
            cy.contains("Unknown server error").should("be.visible");
          },
          "Machine execution.Input is invalid": () => {
            cy.contains("An error occured during machine execution").should(
              "be.visible"
            );

            cy.contains("is not in the alphabet").should("be.visible");
          },
          "Editor state.Stale": () => {
            cy.contains("Stale").should("be.visible");
          },
        },
        events: {
          "Load machine": () => {
            cy.contains("Load").click();
          },
          "Make server execution fail": () => {
            cy.intercept("POST", "http://localhost:8080/execute-machine", {
              statusCode: 500,
            });
          },
          "Set input": ({ event }) => {
            if (event.type !== "Set input") {
              throw new Error("Incorrect event");
            }

            const { input } = event;

            cy.get('[placeholder="0101101..."]').clear().type(input);
          },
          "Set machine configuration": ({ event }) => {
            if (event.type !== "Set machine configuration") {
              throw new Error("Incorrect event");
            }

            const { machineConfiguration } = event;

            cy.contains("Samples")
              .click()
              .get('[id^="headlessui-menu-items"]')
              .contains(machineConfiguration)
              .click();
          },
        },
      });
    });
  });
});
