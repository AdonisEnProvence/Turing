<script lang="ts" setup>
import { computed } from "vue";
import { AutomaticPlayingDelayMode, TapeStep } from "../types";

const props = defineProps<{
  blankCharacter: string;
  steps: TapeStep[];
  indexOnStepList: number;
  onPlay: () => void;
  onPause: () => void;
  onNextStep: () => void;
  onChangeAutomaticPlayingDelayMode: (mode: AutomaticPlayingDelayMode) => void;
  onResetSteps: () => void;
}>();

const blankCharacter = computed(() => props.blankCharacter);

// Must be an odd number.
const displayedTapeLength = 13;
const squaresAmountOnOneSideOfHead = Math.floor(displayedTapeLength / 2);
const addedBlankSpace = squaresAmountOnOneSideOfHead;

/**
 * We add enough blank squares to all steps so that
 * there will always be the same amount of squares on the tape.
 */
function fillStepsWithBlankSpacesOnSides(steps: TapeStep[]): TapeStep[] {
  return steps.map(({ tape, ...props }) => ({
    tape: [
      ...Array.from({ length: addedBlankSpace }).map(
        () => blankCharacter.value
      ),
      ...tape,
      ...Array.from({ length: addedBlankSpace }).map(
        () => blankCharacter.value
      ),
    ],
    ...props,
  }));
}

const steps = computed(() => fillStepsWithBlankSpacesOnSides(props.steps));

const keysReference = computed(() =>
  steps.value[steps.value.length - 1].tape.map((_, index) => index)
);

interface TapeSquareWithKey {
  key: string;
  value: string;
}

interface TapeStepWithKeyForEachSquare extends Omit<TapeStep, 'tape'> {
  tape: TapeSquareWithKey[];
}

function computeTapeListWithFixedKeys() {
  const tapeListWithKeyAndBlanks: TapeStepWithKeyForEachSquare[] = [];

  /**
   * Start from the biggest tape to ensure
   * that keys remain the same for smaller tapes.
   */
  for (const { tape, indexOnTape, ...props } of steps.value.slice().reverse()) {
    if (tape.length !== keysReference.value.length) {
      const tapeIsReducedOnLeft = indexOnTape === 0;

      if (tapeIsReducedOnLeft === true) {
        keysReference.value.shift();
      } else {
        keysReference.value.pop();
      }
    }

    tapeListWithKeyAndBlanks.push({
      tape: tape.map(
        (value, index) =>
          ({
            key: `square:${keysReference.value[index]}`,
            value,
          } as TapeSquareWithKey)
      ),
      indexOnTape,
      ...props
    });
  }

  tapeListWithKeyAndBlanks.reverse();

  return tapeListWithKeyAndBlanks;
}

const tapeListWithKeyAndBlanks = computeTapeListWithFixedKeys();

const tape = computed(() => tapeListWithKeyAndBlanks[props.indexOnStepList].tape);
const headIndexOnTape = computed(
  () => tapeListWithKeyAndBlanks[props.indexOnStepList].indexOnTape
);

const displayedTape = computed(() => {
  return tape.value.slice(
    headIndexOnTape.value,
    displayedTapeLength + headIndexOnTape.value
  );
});
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

    <div class="flex mt-4 space-x-6">
      <button @click="props.onPlay">Play</button>
      <button @click="props.onPause">Pause</button>
      <button @click="props.onNextStep">Next step</button>

      <div class="space-x-2">
        <button @click="onChangeAutomaticPlayingDelayMode('MEDIUM')">
          Medium
        </button>
        <button @click="onChangeAutomaticPlayingDelayMode('FAST')">Fast</button>
      </div>

      <button @click="props.onResetSteps">Reset</button>
    </div>
  </div>
</template>
