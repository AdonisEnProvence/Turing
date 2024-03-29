<script lang="ts" setup>
import { useActor, useMachine } from "@xstate/vue";
import { computed } from "vue";
import {
  ArrowNarrowRightIcon,
  CheckIcon,
  XCircleIcon,
  XIcon,
} from "@heroicons/vue/solid";
import TheTape from "./components/TheTape.vue";
import { vizMachine } from "./machines/viz";
import { SubmitButtonActorRef } from "./machines/submit-button";
import { AutomaticPlayingDelayMode, PlayingStatus } from "./types";
import AppCodeEditor from "./components/AppCodeEditor.vue";
import AppBadge from "./components/AppBadge.vue";
import TheMachineConfiguration from "./components/TheMachineConfiguration.vue";

const { state, send } = useMachine(vizMachine);
const indexOnStepList = computed(() => state.value.context.stepIndex);
const playingStatus = computed<PlayingStatus>(() => {
  if (
    state.value.matches(
      "Application is ready.Managing visualizer execution.Idle"
    )
  ) {
    return "not-started";
  }

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

const hasServerExecutionErrorOccured = computed(
  () =>
    state.value.matches(
      "Application is ready.Managing machine and input execution.Failed to execute machine and input"
    ) === true
);
const errorToDisplay = computed(() => {
  const isMachineConfigurationInvalidJSON =
    state.value.matches(
      "Application is ready.Managing machine and input execution.Failed to execute machine and input.Invalid machine configuration"
    ) === true;
  if (isMachineConfigurationInvalidJSON === true) {
    return "Machine configuration must be valid JSON";
  }

  const isUnknownServerError =
    state.value.matches(
      "Application is ready.Managing machine and input execution.Failed to execute machine and input.Unknown server error"
    ) === true;
  if (isUnknownServerError === true) {
    return "Unknown server error";
  }

  const isKnownServerError = state.value.matches(
    "Application is ready.Managing machine and input execution.Failed to execute machine and input.Known server error"
  );
  if (isKnownServerError === true) {
    return state.value.context.serverExecutionError;
  }

  return undefined;
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

function setInput(input: string) {
  send({
    type: "Set input",
    input,
  });
}

function handleInputTextFieldChange(event: Event) {
  const target = event.target as HTMLInputElement;

  setInput(target.value);
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

          <div>
            <a
              href="https://github.com/AdonisEnProvence/Turing"
              target="_blank"
              rel="nofollow noopener"
              class="inline-flex items-center p-1 m-4 text-white bg-gray-800 border border-transparent rounded-md shadow-lg hover:bg-gray-900 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-gray-800"
            >
              <span class="sr-only">See the project on Github</span>

              <svg
                width="24"
                height="24"
                viewBox="0 0 16 16"
                fill="currentColor"
              >
                <path
                  fill-rule="evenodd"
                  d="M8 0C3.58 0 0 3.58 0 8c0 3.54 2.29 6.53 5.47 7.59.4.07.55-.17.55-.38 0-.19-.01-.82-.01-1.49-2.01.37-2.53-.49-2.69-.94-.09-.23-.48-.94-.82-1.13-.28-.15-.68-.52-.01-.53.63-.01 1.08.58 1.23.82.72 1.21 1.87.87 2.33.66.07-.52.28-.87.51-1.07-1.78-.2-3.64-.89-3.64-3.95 0-.87.31-1.59.82-2.15-.08-.2-.36-1.02.08-2.12 0 0 .67-.21 2.2.82.64-.18 1.32-.27 2-.27.68 0 1.36.09 2 .27 1.53-1.04 2.2-.82 2.2-.82.44 1.1.16 1.92.08 2.12.51.56.82 1.27.82 2.15 0 3.07-1.87 3.75-3.65 3.95.29.25.54.73.54 1.48 0 1.07-.01 1.93-.01 2.2 0 .21.15.46.55.38A8.013 8.013 0 0016 8c0-4.42-3.58-8-8-8z"
                ></path>
              </svg>
            </a>
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
                <div
                  v-if="hasServerExecutionErrorOccured === true"
                  class="p-4 rounded-md bg-red-50"
                >
                  <div class="flex">
                    <div class="flex-shrink-0">
                      <XCircleIcon
                        class="w-5 h-5 text-red-400"
                        aria-hidden="true"
                      />
                    </div>

                    <div class="ml-3">
                      <h3 class="text-sm font-medium text-red-800">
                        An error occured during machine execution
                      </h3>

                      <div class="mt-2 text-sm text-red-700">
                        <ul role="list" class="pl-5 space-y-1 list-disc">
                          <li class="whitespace-pre-line">
                            {{ errorToDisplay }}
                          </li>
                        </ul>
                      </div>
                    </div>
                  </div>
                </div>

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

                  <TheMachineConfiguration
                    :code="state.context.machineCode"
                    :on-code-change="handleMachineCodeChange"
                    :on-input-change="setInput"
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
                        : submitButtonState.matches('Error')
                        ? 'text-red-800 bg-red-100 focus:ring-red-500'
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
                        v-else-if="submitButtonState.matches('Error') === true"
                        key="error"
                        class="inline-flex items-center"
                      >
                        Failed

                        <XIcon class="w-5 h-5 ml-3 -mr-1 text-red-500" />
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

          <footer>
            <p class="text-center">
              Made by Paul Rastoin and Baptiste Devessier
            </p>
          </footer>
        </div>
      </main>
    </div>
  </div>
</template>
