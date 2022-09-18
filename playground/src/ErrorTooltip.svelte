<script lang="ts">
    import type { TooltipState } from "./tooltipState";

    export let state: TooltipState;
</script>

{#if state.type === "visible"}
    <div class="error-tooltip" style={`top: ${state.y}px; left: ${state.x}px`}>
        {@html state.msg}
    </div>
{/if}

<style>
    .error-tooltip {
        position: absolute;
        z-index: 3;
        padding: 5px;
        border: 1px solid #e0e0e0;
        background-color: #202020;
        white-space: nowrap;
        font-family: monospace;
    }
</style>
