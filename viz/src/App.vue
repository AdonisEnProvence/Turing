<script lang="ts" setup>
import TheTape from "./components/TheTape.vue";
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
            <TheTape />
            <p>Controls</p>
          </div>
        </div>
      </main>
    </div>
  </div>
</template>
