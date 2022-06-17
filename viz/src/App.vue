<script lang="ts" setup>
import { useMachine } from "@xstate/vue";
import { computed } from "vue";
import TheTape from "./components/TheTape.vue";
import { vizMachine } from "./machines/viz";
import { AutomaticPlayingDelayMode, Steps } from "./types";

const blankCharacter = ".";
const steps: Steps = [
  [["0", "0", "1", "1"], 0],
  [[".", "0", "1", "1"], 1],
  [[".", "0", "1", "1"], 2],
  [[".", "0", "1", "1"], 3],
  [[".", "0", "1", "1", "."], 4],
  [[".", "0", "1", "1", "."], 3],
  [[".", "0", "1", ".", "."], 2],
  [[".", "0", "1", ".", "."], 1],
  [[".", "0", "1", ".", "."], 0],
  [[".", "0", "1", ".", "."], 1],
  [[".", ".", "1", ".", "."], 2],
  [[".", ".", "1", ".", "."], 3],
  [[".", ".", "1", ".", "."], 2],
  [[".", ".", ".", ".", "."], 1],
  [[".", ".", ".", ".", "."], 2],
  [[".", ".", "y", ".", "."], 1],
];

const { state, send } = useMachine(vizMachine, {
  guards: {
    "Has reached end of steps": (context) =>
      context.stepIndex >= steps.length - 1,
  },
});
const indexOnStepList = computed(() => state.value.context.stepIndex);

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
              :steps="steps"
              :blank-character="blankCharacter"
              :index-on-step-list="indexOnStepList"
              :on-play="handlePlay"
              :on-pause="handlePause"
              :on-next-step="handleNextStep"
              :on-change-automatic-playing-delay-mode="
                handleChangeAutomaticDelayMode
              "
              :on-reset-steps="handleResetSteps"
            />
            <p>Controls</p>
          </div>
        </div>
      </main>
    </div>
  </div>
</template>
