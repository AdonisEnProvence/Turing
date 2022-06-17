<script lang="ts" setup>
import { VAceEditor } from "vue3-ace-editor";
import ace from "ace-builds";
import "ace-builds/src-noconflict/mode-json";
import "ace-builds/src-noconflict/theme-chrome";
import workerJsonUrl from "ace-builds/src-noconflict/worker-json?url"; // For vite

ace.config.setModuleUrl("ace/mode/json_worker", workerJsonUrl);

const props = defineProps<{
  code: string;
}>();

const emit = defineEmits<{
  (event: "update:code", value: string): void;
}>();
</script>

<template>
  <VAceEditor
    :value="props.code"
    lang="json"
    :options="{
      tabSize: 4,
    }"
    @update:value="emit('update:code', $event)"
  />
</template>
