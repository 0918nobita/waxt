<script lang="ts">
    import EditableArea from "./EditableArea.svelte";
    import LineNumbers from "./LineNumbers.svelte";
</script>

<div id="editorContent">
    <LineNumbers />
    <EditableArea />
</div>

<style>
    #editorContent {
        display: flex;
        height: 100%;
    }
</style>
