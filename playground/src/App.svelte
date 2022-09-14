<script lang="ts">
    import { lexicalAnalysis } from "../fable-out/Program.js";

    let src = "";

    const executeLexer = () => {
        lexicalAnalysis(src);
    };
</script>

<main>
    <h1>WAXT Playground</h1>

    <textarea bind:value={src} placeholder="ソースコード" />

    <button on:click={executeLexer}>Execute lexer</button>
</main>
