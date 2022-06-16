<script lang="ts" setup>
import { useMachine } from "@xstate/vue";
import { computed } from "vue";
import TheTape from "./components/TheTape.vue";
import { vizMachine } from "./machines/viz";
import {
  AutomaticPlayingDelayMode,
  MachineExecution,
  PlayingStatus,
} from "./types";

const exec: MachineExecution = {
  blank: ".",
  tapeHistory: [
    {
      currentState: "pick_character",
      indexOnTape: 0,
      status: "continue",
      tape: ["a", "b", "c", "b", "a"],
    },
    {
      currentState: "go_to_end_and_find_a",
      indexOnTape: 1,
      status: "continue",
      tape: [".", "b", "c", "b", "a"],
    },
    {
      currentState: "go_to_end_and_find_a",
      indexOnTape: 2,
      status: "continue",
      tape: [".", "b", "c", "b", "a"],
    },
    {
      currentState: "go_to_end_and_find_a",
      indexOnTape: 3,
      status: "continue",
      tape: [".", "b", "c", "b", "a"],
    },
    {
      currentState: "go_to_end_and_find_a",
      indexOnTape: 4,
      status: "continue",
      tape: [".", "b", "c", "b", "a"],
    },
    {
      currentState: "go_to_end_and_find_a",
      indexOnTape: 5,
      status: "continue",
      tape: [".", "b", "c", "b", "a", "."],
    },
    {
      currentState: "is_a",
      indexOnTape: 4,
      status: "continue",
      tape: [".", "b", "c", "b", "a", "."],
    },
    {
      currentState: "go_to_beginning",
      indexOnTape: 3,
      status: "continue",
      tape: [".", "b", "c", "b", ".", "."],
    },
    {
      currentState: "go_to_beginning",
      indexOnTape: 2,
      status: "continue",
      tape: [".", "b", "c", "b", ".", "."],
    },
    {
      currentState: "go_to_beginning",
      indexOnTape: 1,
      status: "continue",
      tape: [".", "b", "c", "b", ".", "."],
    },
    {
      currentState: "go_to_beginning",
      indexOnTape: 0,
      status: "continue",
      tape: [".", "b", "c", "b", ".", "."],
    },
    {
      currentState: "pick_character",
      indexOnTape: 1,
      status: "continue",
      tape: [".", "b", "c", "b", ".", "."],
    },
    {
      currentState: "go_to_end_and_find_b",
      indexOnTape: 2,
      status: "continue",
      tape: [".", ".", "c", "b", ".", "."],
    },
    {
      currentState: "go_to_end_and_find_b",
      indexOnTape: 3,
      status: "continue",
      tape: [".", ".", "c", "b", ".", "."],
    },
    {
      currentState: "go_to_end_and_find_b",
      indexOnTape: 4,
      status: "continue",
      tape: [".", ".", "c", "b", ".", "."],
    },
    {
      currentState: "is_b",
      indexOnTape: 3,
      status: "continue",
      tape: [".", ".", "c", "b", ".", "."],
    },
    {
      currentState: "go_to_beginning",
      indexOnTape: 2,
      status: "continue",
      tape: [".", ".", "c", ".", ".", "."],
    },
    {
      currentState: "go_to_beginning",
      indexOnTape: 1,
      status: "continue",
      tape: [".", ".", "c", ".", ".", "."],
    },
    {
      currentState: "pick_character",
      indexOnTape: 2,
      status: "continue",
      tape: [".", ".", "c", ".", ".", "."],
    },
    {
      currentState: "go_to_end_and_find_c",
      indexOnTape: 3,
      status: "continue",
      tape: [".", ".", ".", ".", ".", "."],
    },
    {
      currentState: "is_c",
      indexOnTape: 2,
      status: "continue",
      tape: [".", ".", ".", ".", ".", "."],
    },
    {
      currentState: "write_is_palindrome",
      indexOnTape: 3,
      status: "continue",
      tape: [".", ".", ".", ".", ".", "."],
    },
    {
      currentState: "HALT",
      indexOnTape: 4,
      status: "final",
      tape: [".", ".", ".", "y", ".", "."],
    },
  ],
};

const { state, send } = useMachine(vizMachine, {
  guards: {
    "Has reached end of steps": (context) =>
      context.stepIndex >= exec.tapeHistory.length - 1,
  },
});
const indexOnStepList = computed(() => state.value.context.stepIndex);
const playingStatus = computed<PlayingStatus>(() => {
  if (state.value.matches("Playing steps.Automatic playing off")) {
    return "paused";
  }

  if (state.value.matches("Playing steps.Automatic playing on")) {
    return "playing";
  }

  return "disabled";
});
const automaticPlayingDelayMode = computed(
  () => state.value.context.automaticPlayingDelayMode
);

function handlePlay() {
  send({
    type: "Play",
  });
}

function handlePause() {
  send({
    type: "Pause",
  });
}

function handleNextStep() {
  send({
    type: "Next step",
  });
}

function handleChangeAutomaticDelayMode(mode: AutomaticPlayingDelayMode) {
  send({
    type: "Set automatic playing delay",
    mode,
  });
}

function handleResetSteps() {
  send({
    type: "Reset steps",
  });
}
</script>

<template>
  <div class="min-h-full">
    <nav class="bg-white border-b border-gray-200">
      <div class="px-4 mx-auto max-w-7xl sm:px-6 lg:px-8">
        <div class="flex justify-between h-16">
          <div class="flex">
            <div class="flex items-center flex-shrink-0">
              <h1 class="text-3xl font-bold leading-tight text-gray-900">
                Turing
              </h1>
            </div>
          </div>
        </div>
      </div>
    </nav>

    <div class="py-10">
      <main>
        <div class="mx-auto max-w-7xl sm:px-6 lg:px-8">
          <div class="px-4 py-8 space-y-10 sm:px-0">
            <TheTape
              :steps="exec.tapeHistory"
              :blank-character="exec.blank"
              :index-on-step-list="indexOnStepList"
              :playing-status="playingStatus"
              :automatic-playing-delay-mode="automaticPlayingDelayMode"
              :on-play="handlePlay"
              :on-pause="handlePause"
              :on-next-step="handleNextStep"
              :on-change-automatic-playing-delay-mode="
                handleChangeAutomaticDelayMode
              "
              :on-reset-steps="handleResetSteps"
            />
          </div>
        </div>
      </main>
    </div>
  </div>
</template>
