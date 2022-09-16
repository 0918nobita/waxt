<script lang="ts">
    import { onMount } from "svelte";

    import { lexicalAnalysis } from "../fable-out/Program.js";
    import { insertErrorMessages } from "./insertErrorMessages.js";
    import type { LexError } from "./lexerType";

    let editorElement: HTMLTextAreaElement = null;
    let srcHtml = "";

    onMount(() => {
        editorElement.value = srcHtml = `(add 3 4)`;
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
            srcHtml = insertErrorMessages(editorElement.value, executeLexer());
        }, 750);
    };

    const inputHandler = () => {
        srcHtml = editorElement.value;
    };
</script>

<main>
    <h1>WAXT Playground</h1>

    <div id="editorContainer">
        <pre id="highlighted"><code>{@html srcHtml}</code></pre>
        <textarea
            id="editor"
            spellcheck="false"
            aria-hidden="true"
            bind:this={editorElement}
            on:keyup={keyupHandler}
            on:input={inputHandler}
        />
    </div>
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
