<script lang="ts" setup>
import { Menu, MenuButton, MenuItem, MenuItems } from "@headlessui/vue";
import { ChevronDownIcon } from "@heroicons/vue/solid";
import AppCodeEditor from "./AppCodeEditor.vue";
import {
  sampleMachinesName,
  getMachineCodeAndInput,
} from "../constants/sample-machines";

const props = defineProps<{
  code: string;
  onCodeChange: (code: string) => void;
  onInputChange: (input: string) => void;
}>();

function handleSampleMachineClick(machineName: string) {
  const { machineCode, input } = getMachineCodeAndInput(machineName);

  props.onCodeChange(machineCode);
  props.onInputChange(input);
}
</script>

<template>
  <div class="relative">
    <AppCodeEditor :code="code" class="mt-1 h-96" @update:code="onCodeChange" />

    <div class="absolute top-0 right-0 m-4">
      <Menu as="div" class="relative inline-block text-left">
        <div>
          <MenuButton
            class="inline-flex justify-center w-full px-4 py-2 text-sm font-medium text-gray-700 bg-white border border-gray-300 rounded-md shadow-sm hover:bg-gray-50 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-offset-gray-100 focus:ring-yellow-500"
          >
            Samples

            <ChevronDownIcon class="w-5 h-5 ml-2 -mr-1" aria-hidden="true" />
          </MenuButton>
        </div>

        <Transition
          enter-active-class="transition duration-100 ease-out"
          enter-from-class="transform scale-95 opacity-0"
          enter-to-class="transform scale-100 opacity-100"
          leave-active-class="transition duration-75 ease-in"
          leave-from-class="transform scale-100 opacity-100"
          leave-to-class="transform scale-95 opacity-0"
        >
          <MenuItems
            class="absolute right-0 w-56 mt-2 origin-top-right bg-white rounded-md shadow-lg ring-1 ring-black ring-opacity-5 focus:outline-none"
          >
            <div class="flex flex-col py-1">
              <MenuItem
                v-for="machineName of sampleMachinesName"
                :key="machineName"
                v-slot="{ active }"
              >
                <button
                  :class="[
                    active ? 'bg-gray-100 text-gray-900' : 'text-gray-700',
                    'text-start px-4 py-2 text-sm',
                  ]"
                  @click="handleSampleMachineClick(machineName)"
                >
                  {{ machineName }}
                </button>
              </MenuItem>
            </div>
          </MenuItems>
        </Transition>
      </Menu>
    </div>
  </div>
</template>
