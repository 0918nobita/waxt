<script lang="ts">
    import type { TooltipState } from "./tooltipState";

    export let state: TooltipState;
</script>

<main>
    {#if state.type === "visible"}
        <div
            class="error-tooltip"
            style={`top: ${state.y + 10}px; left: ${state.x + 10}px`}
        >
            {@html state.msg}
        </div>
    {/if}
</main>

<style>
    .error-tooltip {
        position: absolute;
        padding: 5px;
        border: 1px solid #e0e0e0;
        background-color: #202020;
        white-space: nowrap;
        z-index: 3;
    }
</style>
