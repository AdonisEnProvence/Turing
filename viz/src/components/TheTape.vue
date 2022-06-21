<script lang="ts" setup>
import { computed } from "vue";
import { PlayIcon, PauseIcon } from "@heroicons/vue/outline";
import {
  ChevronDoubleRightIcon,
  RefreshIcon,
  FastForwardIcon,
} from "@heroicons/vue/solid";
import {
  AutomaticPlayingDelayMode,
  TapeStep,
  StatusOfExecution,
  PlayingStatus,
} from "../types";
import AppBadge, { AppBadgeStatus } from "./AppBadge.vue";

interface TapeSquareWithKey {
  key: string;
  value: string;
}

interface TapeStepWithKeyForEachSquare extends Omit<TapeStep, "tape"> {
  tape: TapeSquareWithKey[];
}

const EMPTY_STEP: TapeStep = {
  currentState: "None",
  indexOnTape: 0,
  status: "none",
  tape: ["-"],
};

const props = defineProps<{
  isTapeDisabled: boolean;
  blankCharacter: string | undefined;
  steps: TapeStep[] | undefined;
  indexOnStepList: number;
  playingStatus: PlayingStatus;
  automaticPlayingDelayMode: AutomaticPlayingDelayMode;
  onPlay: () => void;
  onPause: () => void;
  onNextStep: () => void;
  onChangeAutomaticPlayingDelayMode: (mode: AutomaticPlayingDelayMode) => void;
  onResetSteps: () => void;
}>();

const isMachineNotLoaded = computed(
  () => props.blankCharacter === undefined || props.steps === undefined
);

// Must be an odd number.
const displayedTapeLength = 17;
const squaresAmountOnOneSideOfHead = Math.floor(displayedTapeLength / 2);
const addedBlankSpace = squaresAmountOnOneSideOfHead;

/**
 * We add enough blank squares to all steps so that
 * there will always be the same amount of squares on the tape.
 */
function fillStepsWithBlankSpacesOnSides(
  steps: TapeStep[],
  blank: string
): TapeStep[] {
  return steps.map(({ tape, ...props }) => ({
    tape: [
      ...Array.from({ length: addedBlankSpace }).map(() => blank),
      ...tape,
      ...Array.from({ length: addedBlankSpace }).map(() => blank),
    ],
    ...props,
  }));
}

const steps = computed(() =>
  fillStepsWithBlankSpacesOnSides(
    props.steps ?? [EMPTY_STEP],
    props.blankCharacter ?? "-"
  )
);

function computeTapeListWithFixedKeys(steps: TapeStep[]) {
  const tapeListWithKeyAndBlanks: TapeStepWithKeyForEachSquare[] = [];

  const keysReference = steps[steps.length - 1].tape.map((_, index) => index);

  /**
   * Start from the biggest tape to ensure
   * that keys remain the same for smaller tapes.
   */
  for (const { tape, indexOnTape, ...props } of steps.slice().reverse()) {
    if (tape.length !== keysReference.length) {
      const tapeIsReducedOnLeft = indexOnTape === 0;

      if (tapeIsReducedOnLeft === true) {
        keysReference.shift();
      } else {
        keysReference.pop();
      }
    }

    tapeListWithKeyAndBlanks.push({
      tape: tape.map(
        (value, index) =>
          ({
            key: `square:${keysReference[index]}`,
            value,
          } as TapeSquareWithKey)
      ),
      indexOnTape,
      ...props,
    });
  }

  tapeListWithKeyAndBlanks.reverse();

  return tapeListWithKeyAndBlanks;
}

const tapeListWithKeyAndBlanks = computed(() =>
  computeTapeListWithFixedKeys(steps.value)
);
const currentStep = computed(
  () => tapeListWithKeyAndBlanks.value[props.indexOnStepList]
);
const tape = computed(() => currentStep.value.tape);
const headIndexOnTape = computed(() => currentStep.value.indexOnTape);
const currentState = computed(() => currentStep.value.currentState);
const stateOfExecution = computed(() => currentStep.value.status);
const stateOfExecutionBadgeStatus = computed(() => {
  const badgeStatus: Record<StatusOfExecution, AppBadgeStatus> = {
    continue: "pending",
    blocked: "error",
    final: "success",
    none: "error",
  };

  return badgeStatus[stateOfExecution.value];
});
const prettyStateOfExecution = computed(() => {
  const prettyNames: Record<StatusOfExecution, string> = {
    continue: "Running",
    blocked: "Blocked",
    final: "Reached final state",
    none: "Not loaded",
  };

  return prettyNames[stateOfExecution.value];
});
const disablePlayingControls = computed(
  () =>
    props.playingStatus === "disabled" || props.playingStatus === "not-started"
);
const disableRestartControl = computed(() => props.playingStatus !== "disabled");

const displayedTape = computed(() => {
  return tape.value.slice(
    headIndexOnTape.value,
    displayedTapeLength + headIndexOnTape.value
  );
});

function handlePlayPauseButtonClick() {
  if (props.playingStatus === "playing") {
    props.onPause();
  } else {
    props.onPlay();
  }
}

function handleAutomaticPlayingDelayMode() {
  props.onChangeAutomaticPlayingDelayMode(
    props.automaticPlayingDelayMode === "FAST" ? "MEDIUM" : "FAST"
  );
}
</script>

<template>
  <div class="flex justify-center w-full">
    <div class="flex flex-col space-y-8 w-full">
      <div class="flex items-center justify-between">
        <p>
          State: <code>{{ currentState }}</code>
        </p>

        <AppBadge :status="stateOfExecutionBadgeStatus" size="medium">
          {{ prettyStateOfExecution }}
        </AppBadge>
      </div>

      <div class="w-full h-14 overflow-hidden relative">
        <div class="absolute left-1/2 flex justify-center" :style="{ transform: 'translateX(-50%)' }">
          <div
            class="overflow-hidden"
            :style="{
              width: `calc(${displayedTapeLength} * 3.5rem - (3.5rem / 6 * 5 * 2))`,
            }"
          >
            <div
              class="relative inline-flex"
              :style="{ marginLeft: 'calc(-1 * 3.5rem / 6 * 5)' }"
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
                  class="flex items-center justify-center text-center border border-gray-100 w-14 h-14"
                >
                  <Transition
                    mode="out-in"
                    enter-active-class="duration-300 ease-out"
                    enter-from-class="opacity-0"
                    enter-to-class="opacity-100"
                    leave-active-class="duration-200 ease-in"
                    leave-from-class="opacity-100"
                    leave-to-class="opacity-0"
                  >
                    <p :key="value">
                      <code>{{ value }}</code>
                    </p>
                  </Transition>
                </div>
              </TransitionGroup>

              <div class="absolute inset-x-0 flex justify-center">
                <div class="bg-yellow-100 opacity-50 w-14 h-14" />
              </div>
            </div>
          </div>
        </div>
      </div>

      <div class="flex items-center mx-auto space-x-6">
        <button
          :disabled="disablePlayingControls"
          :aria-label="playingStatus === 'playing' ? 'Pause' : 'Play'"
          @click="handlePlayPauseButtonClick"
          class="flex items-center justify-center w-10 h-10 rounded-full"
        >
          <PauseIcon
            v-if="playingStatus === 'playing'"
            class="w-10 h-10 text-slate-500"
          />
          <PlayIcon v-else class="w-10 h-10 text-slate-500" />
        </button>

        <button
          :disabled="disablePlayingControls"
          aria-label="Go to next step"
          @click="props.onNextStep"
          class="flex items-center justify-center w-8 h-8 rounded-full"
        >
          <ChevronDoubleRightIcon class="w-6 h-6 text-slate-500" />
        </button>

        <button
          :aria-label="
            props.automaticPlayingDelayMode === 'MEDIUM'
              ? 'Go to fast mode'
              : 'Go to medium mode'
          "
          @click="handleAutomaticPlayingDelayMode"
          class="relative flex items-center justify-center w-8 h-8 rounded-full"
        >
          <FastForwardIcon
            :class="[
              'w-6 h-6',
              props.automaticPlayingDelayMode === 'MEDIUM'
                ? 'text-slate-500'
                : 'text-yellow-400',
            ]"
          />

          <div
            v-if="props.automaticPlayingDelayMode === 'FAST'"
            aria-hidden="true"
            class="absolute bottom-0 w-1 h-1 -translate-x-1/2 bg-yellow-400 rounded-full left-1/2"
          />
        </button>

        <button
          :disabled="disableRestartControl"
          aria-label="Restart execution"
          @click="props.onResetSteps"
          class="flex items-center justify-center w-8 h-8 rounded-full"
        >
          <RefreshIcon class="w-6 h-6 text-slate-500" />
        </button>
      </div>
    </div>

    <Transition
      enter-active-class="transition-opacity duration-300 ease-out"
      enter-from-class="opacity-0"
      enter-to-class="opacity-100"
      leave-active-class="transition-opacity duration-200 ease-in"
      leave-from-class="opacity-100"
      leave-to-class="opacity-0"
    >
      <div
        v-if="isTapeDisabled"
        class="absolute inset-0 bg-white bg-opacity-75"
      />
    </Transition>
  </div>
</template>
