<script lang="ts">
    import { onMount } from "svelte";

    import { lexicalAnalysis } from "../fable-out/Program.js";

    let editorElement: HTMLDivElement = null;

    onMount(() => {
        editorElement.innerHTML = `(add 3 4)`;
    });

    type LexError = [string, number, number, number, number];

    const insertErrorMsgs = (src: string, errors: LexError[]) => {
        let lines = [...src].reduce(
            (lines: Array<string[]>, current: string) =>
                current === "\n"
                    ? [...lines, []]
                    : [...lines.slice(0, -1), [...lines.slice(-1)[0], current]],
            [[]]
        );

        console.log(lines);

        let errorIndexArrays: Array<number[]> = [...Array(src.length)].fill([]);

        for (let i = 0; i < errors.length; i++) {
            const [_, startLine, startCol, endLine, endCol] = errors[i];
            // TODO: 各エラーの発生位置を errorIndexArrays にまとめる
        }

        // TODO: errorIndexArrays 中の連続した重複要素をまとめ、
        // エラー表示用の span 要素を挿入した HTML を生成する
    };

    const executeLexer = () => {
        const src = editorElement.innerText;
        const errors: LexError[] = lexicalAnalysis(src);
        if (errors.length > 0) console.error(errors);
        insertErrorMsgs(src, errors);
    };
</script>

<main>
    <h1>WAXT Playground</h1>

    <div contenteditable="true" spellcheck="false" bind:this={editorElement} />

    <button on:click={executeLexer}>Execute lexer</button>
</main>
