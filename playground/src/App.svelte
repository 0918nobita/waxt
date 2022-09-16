<script lang="ts">
    import { onMount } from "svelte";

    import { lexicalAnalysis } from "../fable-out/Program.js";
    import { insertErrorElements } from "./insertErrorElements.js";
    import type { LexError } from "./lexerType";

    let editorElement: HTMLTextAreaElement = null;
    let highlighted: HTMLElement = null;
    let filters: HTMLDivElement = null;

    onMount(() => {
        editorElement.value = highlighted.innerHTML = `(add 3 4)`;
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

            const getErrorMsg = (errorIndex: number): string => {
                const [msg, startPos, startCol, endPos, endCol] =
                    errors[errorIndex];
                return `${msg} (${startPos + 1}:${startCol + 1} - ${
                    endPos + 1
                }:${endCol + 1})`;
            };

            highlighted.innerHTML = insertErrorElements(
                editorElement.value,
                errors
            );
            filters.innerHTML = "";
            for (const errorElement of [
                ...highlighted.querySelectorAll(".error"),
            ]) {
                const errorIndices = (
                    errorElement as HTMLElement
                ).dataset.errorIndices
                    .split(",")
                    .map(parseInt);

                const rect = errorElement.getBoundingClientRect();

                const filter = document.createElement("div");
                filter.classList.add("filter");
                filter.style.top = `${rect.y}px`;
                filter.style.left = `${rect.x}px`;
                filter.style.width = `${rect.width}px`;
                filter.style.height = `${rect.height}px`;

                const tooltip = document.createElement("div");
                tooltip.classList.add("error-tooltip");
                tooltip.innerHTML = errorIndices.map(getErrorMsg).join("<br>");

                filter.appendChild(tooltip);

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
