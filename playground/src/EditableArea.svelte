<script lang="ts">
    import { onDestroy, onMount } from "svelte";

    import { lexicalAnalysis } from "../fable-out/Program.js";
    import type { LexError } from "./lexerType";
    import { computeEditorHeight, store, type ErrorMarker } from "./store";
    import { getParts } from "./getParts";
    import { partsToHtml } from "./partsToHtml";
    import { stringifyLexError } from "./stringifyLexError";
    import type { TooltipState } from "./tooltipState.js";

    let highlighted: HTMLElement = null;
    let textAreaElement: HTMLTextAreaElement = null;

    let state: { lineHeight: number } | null = null;
    let errorMarkers: ErrorMarker[] = [];

    const unsubscribe = store.subscribe((s) => {
        state = s.type === "wantInput" ? { lineHeight: s.lineHeight } : null;
    });

    const inputHandler = () => {
        highlighted.innerText = textAreaElement.value;

        const matches = textAreaElement.value.match(/\n/g);
        const numLines = matches !== null ? matches.length + 1 : 1;

        if (state === null) return;

        const height = computeEditorHeight({
            numLines,
            lineHeight: state.lineHeight,
        });

        textAreaElement.style.height = height;

        store.set({
            type: "wantInput",
            numLines,
            lineHeight: state.lineHeight,
            errorMarkers: [],
        });
    };

    const executeLexer = (): LexError[] =>
        (
            lexicalAnalysis(textAreaElement.value) as Array<
                [string, number, number]
            >
        ).map(([msg, line, col]) => [msg, line, col, line, col]);

    let timer: NodeJS.Timer;

    const keyupHandler = () => {
        clearTimeout(timer);

        timer = setTimeout(() => {
            const errors = executeLexer();

            const partsPerLine = getParts(textAreaElement.value, errors);

            highlighted.innerHTML = partsToHtml(partsPerLine);

            const textAreaRect = textAreaElement.getBoundingClientRect();
            const editorPageOffset = {
                x: textAreaRect.x - window.pageXOffset,
                y: textAreaRect.y - window.pageYOffset,
            };

            const markers: ErrorMarker[] = [];
            for (const errorElement of [
                ...highlighted.querySelectorAll(".error"),
            ]) {
                const errorIndices = (
                    errorElement as HTMLElement
                ).dataset.errorIndices
                    .split(",")
                    .map(parseInt);

                const msg = errorIndices
                    .map((errorIndex) => stringifyLexError(errors, errorIndex))
                    .join("<br>");

                const boundingClientRect = errorElement.getBoundingClientRect();
                const x =
                    boundingClientRect.left +
                    window.pageXOffset -
                    editorPageOffset.x;
                const y =
                    boundingClientRect.top +
                    window.pageYOffset -
                    editorPageOffset.y;
                const width = boundingClientRect.width;
                const height = boundingClientRect.height;

                markers.push({
                    msg,
                    x,
                    y,
                    width,
                    height,
                });
            }

            store.update((s) => {
                if (s.type !== "wantInput") return s;
                s.errorMarkers = markers;
                return s;
            });

            errorMarkers = markers;
        }, 750);
    };

    let editableArea: HTMLDivElement = null;
    let tooltipState: TooltipState = { type: "hidden" };

    const mouseMoveHandler = (e: MouseEvent) => {
        const msgs = errorMarkers.flatMap((errorMarker) => {
            if (
                e.offsetX >= errorMarker.x &&
                e.offsetX <= errorMarker.x + errorMarker.width &&
                e.offsetY >= errorMarker.y &&
                e.offsetY <= errorMarker.y + errorMarker.height
            ) {
                return [errorMarker.msg];
            } else {
                return [];
            }
        });

        if (msgs.length === 0 && tooltipState.type === "visible") {
            tooltipState = { type: "hidden" };
            return;
        }

        if (msgs.length > 0) {
            tooltipState = {
                type: "visible",
                msg: msgs.join("<br>"),
                x: e.offsetX + 10,
                y: e.offsetY + 10,
            };
        }
    };

    onMount(() => {
        editableArea.addEventListener("mousemove", mouseMoveHandler);
    });

    onDestroy(() => {
        if (editableArea !== null) {
            editableArea.removeEventListener("mousemove", mouseMoveHandler);
        }
        unsubscribe();
    });
</script>

<div id="editableArea" bind:this={editableArea}>
    <pre id="highlighted"><code bind:this={highlighted} /></pre>

    <textarea
        bind:this={textAreaElement}
        on:input={inputHandler}
        on:keyup={keyupHandler}
    />

    {#each errorMarkers as marker}
        <div
            class="error-marker"
            style={`top: ${marker.y}px; left: ${marker.x}px; width: ${marker.width}px; height: ${marker.height}px`}
        />
    {/each}

    {#if tooltipState.type === "visible"}
        <div
            id="errorTooltip"
            style={`top: ${tooltipState.y}px; left: ${tooltipState.x}px`}
        >
            {@html tooltipState.msg}
        </div>
    {/if}
</div>

<style>
    #editableArea {
        position: relative;
        width: 100%;
    }

    #highlighted {
        position: absolute;
        z-index: 1;
        top: 0;
        left: 0;
        width: 100%;
        color: #ccc;
        font-family: monospace;
    }

    textarea {
        position: absolute;
        z-index: 2;
        top: 0;
        left: 0;
        width: 100%;
        min-height: 100%;
        resize: none;
        color: transparent;
        caret-color: #ccc;
        background: transparent;
        border: none;
        outline: none;
        font-family: monospace;
    }

    #errorTooltip {
        position: absolute;
        z-index: 3;
        padding: 5px;
        border: 1px solid #e0e0e0;
        background-color: #202020;
        white-space: nowrap;
        font-family: monospace;
    }
</style>
