<script lang="ts" setup>
import { useActor, useMachine } from "@xstate/vue";
import { computed } from "vue";
import { ArrowNarrowRightIcon, CheckIcon } from "@heroicons/vue/solid";
import TheTape from "./components/TheTape.vue";
import { vizMachine } from "./machines/viz";
import { SubmitButtonActorRef } from "./machines/submit-button";
import { AutomaticPlayingDelayMode, PlayingStatus } from "./types";
import AppCodeEditor from "./components/AppCodeEditor.vue";
import AppBadge from "./components/AppBadge.vue";

const { state, send } = useMachine(vizMachine);
const indexOnStepList = computed(() => state.value.context.stepIndex);
const playingStatus = computed<PlayingStatus>(() => {
  if (
    state.value.matches(
      "Application is ready.Managing visualizer execution.Playing steps.Automatic playing off"
    )
  ) {
    return "paused";
  }

  if (
    state.value.matches(
      "Application is ready.Managing visualizer execution.Playing steps.Automatic playing on"
    )
  ) {
    return "playing";
  }

  return "disabled";
});
const automaticPlayingDelayMode = computed(
  () => state.value.context.automaticPlayingDelayMode
);
const isConfigurationStale = computed(() => {
  const { input, lastLoadingInput, machineCode, lastLoadingMachineCode } =
    state.value.context;

  if (lastLoadingInput === undefined || lastLoadingMachineCode === undefined) {
    return false;
  }

  return input !== lastLoadingInput || machineCode !== lastLoadingMachineCode;
});
const isExecutingInputAndMachine = computed(
  () =>
    state.value.matches(
      "Application is ready.Managing machine and input execution.Executing machine and input"
    ) === true
);
const steps = computed(() => {
  if (isExecutingInputAndMachine.value === true) {
    return undefined;
  }

  return state.value.context.machineExecution?.tapeHistory;
});
const blankCharacter = computed(() => {
  if (isExecutingInputAndMachine.value === true) {
    return undefined;
  }

  return state.value.context.machineExecution?.blank;
});

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

function handleInputTextFieldChange(event: Event) {
  const target = event.target as HTMLInputElement;

  send({
    type: "Set input",
    input: target.value,
  });
}

function handleMachineCodeChange(machineCode: string) {
  send({
    type: "Set machine code",
    machineCode,
  });
}

const { state: submitButtonState, send: submitButtonSend } = useActor(
  computed(() => state.value.children["Submit button"] as SubmitButtonActorRef)
);
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
              :is-tape-disabled="isExecutingInputAndMachine"
              :steps="steps"
              :blank-character="blankCharacter"
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

            <div class="bg-white border-b border-gray-200 shadow sm:rounded-lg">
              <div class="flex items-center px-4 py-5 sm:px-6">
                <h3 class="text-lg font-medium leading-6 text-gray-900">
                  Configuration
                </h3>

                <Transition
                  leave-active-class="duration-100 ease-in"
                  leave-from-class="opacity-100"
                  leave-to-class="opacity-0"
                >
                  <AppBadge
                    v-if="isConfigurationStale"
                    size="small"
                    status="pending"
                    title="Configuration has been changed but not loaded yet"
                    class="ml-4"
                  >
                    Stale
                  </AppBadge>
                </Transition>
              </div>

              <div class="px-4 py-5 space-y-6 sm:px-6">
                <div>
                  <label
                    for="input"
                    class="block text-sm font-medium text-gray-700"
                  >
                    Input
                  </label>

                  <div class="mt-1">
                    <input
                      type="text"
                      name="input"
                      id="input"
                      :value="state.context.input"
                      class="block w-full font-mono border-gray-300 rounded-md shadow-sm focus:ring-yellow-500 focus:border-yellow-500 sm:text-sm"
                      placeholder="0101101..."
                      @input="handleInputTextFieldChange"
                    />
                  </div>
                </div>

                <div>
                  <p class="block text-sm font-medium text-gray-700">Machine</p>

                  <AppCodeEditor
                    :code="state.context.machineCode"
                    class="mt-1 h-96"
                    @update:code="handleMachineCodeChange"
                  />
                </div>

                <div class="flex justify-center">
                  <button
                    type="button"
                    :disabled="submitButtonState.matches('Idle') === false"
                    :class="[
                      'transition-opacity inline-flex items-center px-4 py-2 text-sm font-medium border border-transparent rounded-md shadow-sm focus:outline-none focus:ring-2 focus:ring-offset-2',
                      submitButtonState.matches('Success')
                        ? 'text-green-800 bg-green-100 focus:ring-green-500'
                        : 'text-yellow-800 bg-yellow-100 hover:bg-yellow-200 focus:ring-yellow-500',
                    ]"
                    @click="submitButtonSend({ type: 'Load' })"
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
                      <span
                        v-if="submitButtonState.matches('Idle') === true"
                        key="idle"
                        class="inline-flex items-center"
                      >
                        Load
                        <ArrowNarrowRightIcon
                          class="ml-2 -mr-0.5 h-4 w-4"
                          aria-hidden="true"
                        />
                      </span>

                      <span
                        v-else-if="
                          submitButtonState.matches('Loading') === true
                        "
                        key="loading"
                        class="inline-flex items-center"
                      >
                        <!-- From spin chapter of https://tailwindcss.com/docs/animation -->

                        Processing...

                        <svg
                          class="w-5 h-5 ml-3 -mr-1 text-yellow-500 animate-spin"
                          xmlns="http://www.w3.org/2000/svg"
                          fill="none"
                          viewBox="0 0 24 24"
                        >
                          <circle
                            class="opacity-25"
                            cx="12"
                            cy="12"
                            r="10"
                            stroke="currentColor"
                            stroke-width="4"
                          ></circle>
                          <path
                            class="opacity-75"
                            fill="currentColor"
                            d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z"
                          ></path>
                        </svg>
                      </span>

                      <span
                        v-else
                        key="success"
                        class="inline-flex items-center"
                      >
                        Loaded

                        <CheckIcon class="w-5 h-5 ml-3 -mr-1 text-green-500" />
                      </span>
                    </Transition>
                  </button>
                </div>
              </div>
            </div>
          </div>
        </div>
      </main>
    </div>
  </div>
</template>
