<script lang="ts">
    import { onMount } from "svelte";

    import { lexicalAnalysis } from "../fable-out/Program.js";
    import { getParts } from "./getParts";
    import type { LexError } from "./lexerType";
    import { partsToHtml } from "./partsToHtml";
    import { stringifyLexError } from "./stringifyLexError";
    import type { TooltipState } from "./tooltipState";
    import ErrorTooltip from "./ErrorTooltip.svelte";

    let tooltipState: TooltipState = { type: "hidden" };

    type ErrorRangeWithMsg = {
        msg: string;
        x: number;
        y: number;
        width: number;
        height: number;
    };

    let errorRanges: ErrorRangeWithMsg[] = [];

    let editorElement: HTMLTextAreaElement = null;
    let highlighted: HTMLElement = null;
    let filters: HTMLDivElement = null;

    onMount(() => {
        editorElement.value = highlighted.innerHTML = `(add 3 4)`;

        let handler = (e: MouseEvent) => {
            const msgs = errorRanges.flatMap((errorRange) => {
                if (
                    e.pageX >= errorRange.x &&
                    e.pageX <= errorRange.x + errorRange.width &&
                    e.pageY >= errorRange.y &&
                    e.pageY <= errorRange.y + errorRange.height
                ) {
                    return [errorRange.msg];
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
                    x: e.pageX,
                    y: e.pageY,
                };
            }
        };

        window.addEventListener("mousemove", handler);

        return () => {
            window.removeEventListener("mousemove", handler);
        };
    });

    const executeLexer = (): LexError[] => {
        const src = editorElement.value;

        return (lexicalAnalysis(src) as Array<[string, number, number]>).map(
            ([msg, line, col]) => [msg, line, col, line, col]
        );
    };

    let timer;

    const keyupHandler = () => {
        clearTimeout(timer);

        timer = setTimeout(() => {
            const errors = executeLexer();

            const partsPerLine = getParts(editorElement.value, errors);

            highlighted.innerHTML = partsToHtml(partsPerLine);

            filters.innerHTML = "";

            errorRanges = [];

            for (const errorElement of [
                ...highlighted.querySelectorAll(".error"),
            ]) {
                const clientRect = errorElement.getBoundingClientRect();

                const errorIndices = (
                    errorElement as HTMLElement
                ).dataset.errorIndices
                    .split(",")
                    .map(parseInt);

                const msg = errorIndices
                    .map((errorIndex) => stringifyLexError(errors, errorIndex))
                    .join("<br>");

                const filter = document.createElement("div");
                filter.classList.add("filter");
                const x = window.scrollX + clientRect.x;
                const y = window.scrollY + clientRect.y;
                const width = clientRect.width;
                const height = clientRect.height;
                filter.style.top = `${y}px`;
                filter.style.left = `${x}px`;
                filter.style.width = `${width}px`;
                filter.style.height = `${height}px`;

                errorRanges.push({ msg, x, y, width, height });

                filters.appendChild(filter);
            }
        }, 750);
    };

    const inputHandler = () => {
        highlighted.innerHTML = editorElement.value;
    };
</script>

<main>
    <h1>WAXT Playground</h1>

    <div id="editorContainer">
        <pre id="highlighted"><code bind:this={highlighted} /></pre>
        <textarea
            id="editor"
            spellcheck="false"
            aria-hidden="true"
            bind:this={editorElement}
            on:keyup={keyupHandler}
            on:input={inputHandler}
        />
    </div>

    <div id="filters" bind:this={filters} />

    <ErrorTooltip state={tooltipState} />
</main>

<style>
    #editor {
        position: absolute;
        z-index: 2;
        top: 0;
        left: 0;
        width: 100%;
        height: 100vh;
        padding: 10px;
        resize: none;
        background: transparent;
        color: transparent;
        font-family: monospace;
        border: none;
        caret-color: #ccc;
    }

    #highlighted {
        position: absolute;
        z-index: 1;
        top: 0;
        left: 0;
        width: 100%;
        height: 100vh;
        padding: 10px;
        color: #ccc;
        font-family: monospace;
    }

    #editorContainer {
        position: relative;
        margin: 10px;
        padding: 10px;
    }
</style>
