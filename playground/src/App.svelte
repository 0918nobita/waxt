<script lang="ts">
    import { onMount } from "svelte";

    import { lexicalAnalysis } from "../fable-out/Program.js";
    import type { LexError } from "./lexerType";
    import { insertErrorMessages } from "./insertErrorMessages";

    let editorElement: HTMLDivElement = null;

    onMount(() => {
        editorElement.innerHTML = `(add 3 4)`;
    });

    const executeLexer = () => {
        const src = editorElement.innerText;

        const errors: LexError[] = (
            lexicalAnalysis(src) as Array<[string, number, number]>
        ).map(([msg, line, col]) => [msg, line, col, line, col]);

        if (errors.length > 0) console.error(errors);

        editorElement.innerHTML = insertErrorMessages(src, errors);
    };

    const keydownHandler = (event: KeyboardEvent) => {
        if (event.key !== "Backspace") return;

        const selection = window.getSelection();
        const range = selection.getRangeAt(0);

        if (
            range.startContainer instanceof Element &&
            range.startContainer.classList.contains("error")
        ) {
            range.startContainer.remove();
        }
    };
</script>

<main>
    <h1>WAXT Playground</h1>

    <div
        id="editor"
        contenteditable="true"
        spellcheck="false"
        bind:this={editorElement}
        on:keydown={keydownHandler}
    />

    <button id="executeButton" on:click={executeLexer}>Execute lexer</button>
</main>

<style>
    #editor {
        padding: 10px;
        border: 1px solid #ccc;
    }

    #executeButton {
        margin: 10px;
    }
</style>
