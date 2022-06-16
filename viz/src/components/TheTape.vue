<script lang="ts" setup>
import { ref, computed } from "vue";

// Must be an odd number.
const displayedTapeLength = 13;
const squaresAmountOnOneSideOfHead = Math.floor(displayedTapeLength / 2);
const addedBlankSpace = squaresAmountOnOneSideOfHead;

type Steps = [tape: string[], indexOfHead: number][];

const blankCharacter = ".";
const steps: Steps = fillStepsWithBlankSpacesOnSides([
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
]);

/**
 * We add enough blank squares to all steps so that
 * there will always be the same amount of squares on the tape.
 */
function fillStepsWithBlankSpacesOnSides(steps: Steps): Steps {
  return steps.map(([tape, indexOfHead]) => [
    [
      ...Array.from({ length: addedBlankSpace }).map(() => blankCharacter),
      ...tape,
      ...Array.from({ length: addedBlankSpace }).map(() => blankCharacter),
    ],
    indexOfHead,
  ]);
}

const keysReference = steps[steps.length - 1][0].map((_, index) => index);

type TapeSquareWithKey = { key: string; value: string };

type StepWithKey = [tape: TapeSquareWithKey[], indexOfHead: number];

function computeTapeListWithFixedKeys() {
  const tapeListWithKeyAndBlanks: StepWithKey[] = [];

  /**
   * Start from the biggest tape to ensure
   * that keys remain the same for smaller tapes.
   */
  for (const [tape, indexOfHead] of steps.slice().reverse()) {
    if (tape.length !== keysReference.length) {
      const tapeIsReducedOnLeft = indexOfHead === 0;

      if (tapeIsReducedOnLeft === true) {
        keysReference.shift();
      } else {
        keysReference.pop();
      }
    }

    tapeListWithKeyAndBlanks.push([
      tape.map(
        (value, index) =>
          ({
            key: `square:${keysReference[index]}`,
            value,
          } as TapeSquareWithKey)
      ),
      indexOfHead,
    ]);
  }

  tapeListWithKeyAndBlanks.reverse();

  return tapeListWithKeyAndBlanks;
}

const indexOnStepList = ref(0);

const tapeListWithKeyAndBlanks = computeTapeListWithFixedKeys();

const tape = computed(() => tapeListWithKeyAndBlanks[indexOnStepList.value][0]);
const headIndexOnTape = computed(
  () => tapeListWithKeyAndBlanks[indexOnStepList.value][1]
);

const displayedTape = computed(() => {
  return tape.value.slice(
    headIndexOnTape.value,
    displayedTapeLength + headIndexOnTape.value
  );
});

function goToPreviousIteration() {
  if (indexOnStepList.value > 0) {
    indexOnStepList.value--;
  }
}

function goToNextIteration() {
  if (indexOnStepList.value < steps.length - 1) {
    indexOnStepList.value++;
  }
}
</script>

<template>
  <div>
    <p>Index on tape: {{ headIndexOnTape }}</p>
    <p>Step: {{ indexOnStepList }}</p>

    <div>
      <div
        class="overflow-hidden"
        :style="{
          width: `calc(${displayedTapeLength} * 2.5rem - (2.5rem / 6 * 5 * 2))`,
        }"
      >
        <div
          class="relative inline-flex"
          :style="{ marginLeft: 'calc(-1 * 2.5rem / 6 * 5)' }"
        >
          <TransitionGroup
            leave-from-class="opacity-0"
            leave-active-class="absolute transition-all duration-[0s]"
            leave-to-class="opacity-0"
            move-class="transition-all duration-200"
          >
            <div
              v-for="{ key, value } in displayedTape"
              :id="key"
              :key="key"
              class="flex items-center justify-center w-10 h-10 text-center border border-gray-100"
            >
              <Transition
                mode="out-in"
                enter-from-class="opacity-0"
                enter-active-class="transition-opacity duration-100"
                leave-to-class="opacity-0"
                leave-active-class="transition-opacity duration-100"
              >
                <p :key="value">{{ value }}</p>
              </Transition>
            </div>
          </TransitionGroup>

          <div class="absolute inset-x-0 flex justify-center">
            <div class="w-10 h-10 bg-yellow-100 opacity-50" />
          </div>
        </div>
      </div>
    </div>

    <div class="flex space-x-2">
      <button @click="goToPreviousIteration">Go left</button>
      <button @click="goToNextIteration">Go right</button>
    </div>
  </div>
</template>
