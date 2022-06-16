<script lang="ts">
export type AppBadgeStatus = "pending" | "success" | "error";
export type AppBadgeSize = "small" | "medium";
</script>

<script lang="ts" setup>
import { defineProps, computed } from "vue";

const props = defineProps<{ status: AppBadgeStatus; size: AppBadgeSize }>();

const colorClasses = computed(() => {
  const classesForStatus: Record<AppBadgeStatus, string> = {
    success: "bg-green-100 text-green-800",
    pending: "bg-yellow-100 text-yellow-800",
    error: "bg-red-100 text-red-800",
  };

  return classesForStatus[props.status];
});

const sizeClasses = computed(() => {
  const classesForSize: Record<AppBadgeSize, string> = {
    small: "px-2.5 py-0.5 rounded-full text-xs",
    medium: "px-3 py-0.5 rounded-full text-sm",
  };

  return classesForSize[props.size];
});
</script>

<template>
  <span
    :class="['inline-flex items-center font-medium', colorClasses, sizeClasses]"
  >
    <slot />
  </span>
</template>
