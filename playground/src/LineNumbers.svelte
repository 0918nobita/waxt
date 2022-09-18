<script lang="ts">
    import { onDestroy } from "svelte";

    import { computeEditorHeight, store } from "./store";

    let lineNumbers: HTMLDivElement = null;

    let numLines = 1;

    const unsubscribe = store.subscribe((s) => {
        if (s.type === "wantLineHeight") {
            const lineHeight = parseFloat(
                window.getComputedStyle(
                    document.getElementsByClassName("line-number").item(0)
                ).height
            );

            store.set({
                type: "wantInput",
                lineHeight,
                numLines: 1,
                errorMarkers: [],
            });
            return;
        }

        if (s.type === "wantInput") {
            numLines = s.numLines;

            const height = computeEditorHeight({
                lineHeight: s.lineHeight,
                numLines: s.numLines,
            });

            lineNumbers.style.height = height;
        }
    });

    onDestroy(unsubscribe);
</script>

<div id="lineNumbers" bind:this={lineNumbers}>
    {#each Array(numLines) as _, i}
        <div class="line-number">{i + 1}</div>
    {/each}
</div>

<style>
    #lineNumbers {
        display: flex;
        flex-direction: column;
        align-items: flex-end;
        min-height: 100%;
        font-family: monospace;
        border-right: 1px solid #777;
        padding: 0 10px;
        font-family: monospace;
    }
</style>
