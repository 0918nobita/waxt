<script lang="ts">
    import { onMount } from "svelte";

    import EditorContent from "./EditorContent.svelte";
    import { store } from "./store";

    onMount(() => {
        store.set({ type: "wantLineHeight" });
    });
</script>

<main>
    <div id="page">
        <header>WAXT Playground</header>
        <div id="editorContainer">
            <EditorContent />
        </div>
    </div>
</main>

<style>
    header {
        padding: 10px;
    }

    #page {
        display: flex;
        flex-direction: column;
        height: 100vh;
    }

    #editorContainer {
        height: 100%;
        overflow-y: scroll;
    }
</style>
