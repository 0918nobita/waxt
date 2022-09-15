<script lang="ts">
    import { onMount } from "svelte";

    import { lexicalAnalysis } from "../fable-out/Program.js";

    let editorElement: HTMLDivElement = null;

    onMount(() => {
        editorElement.innerHTML = `(add 3 4)`;
    });

    type LexError = [string, number, number, number, number];

    type Line = Array<{ c: string; errorIndices: number[] }>;

    type Part =
        | { type: "ok"; src: string }
        | { type: "error"; src: string; errorIndices: number[] };

    const insertErrorMsgs = (src: string, errors: LexError[]): string => {
        let lines = [...src].reduce(
            (lines: Line[], c: string) =>
                c === "\n"
                    ? [...lines, []]
                    : [
                          ...lines.slice(0, -1),
                          [...lines.slice(-1)[0], { c, errorIndices: [] }],
                      ],
            [[]]
        );

        for (let i = 0; i < errors.length; i++) {
            const [_, startLine, startCol, endLine, endCol] = errors[i];

            for (let line = startLine; line <= endLine; line++)
                for (let col = startCol; col <= endCol; col++)
                    lines[line][col].errorIndices.push(i);
        }

        const partsPerLine: Array<Part[]> = [];

        for (let lineIndex = 0; lineIndex < lines.length; lineIndex++) {
            const parts: Part[] = [];

            for (
                let colIndex = 0;
                colIndex < lines[lineIndex].length;
                colIndex++
            ) {
                const { c, errorIndices } = lines[lineIndex][colIndex];

                if (parts.length === 0) {
                    parts.push(
                        errorIndices.length === 0
                            ? { type: "ok", src: c }
                            : { type: "error", src: c, errorIndices }
                    );
                    continue;
                }

                const lastPart = parts[parts.length - 1];
                if (lastPart.type === "ok") {
                    if (errorIndices.length === 0) {
                        lastPart.src += c;
                    } else {
                        parts.push({ type: "error", src: c, errorIndices });
                    }
                } else if (errorIndices.length === 0) {
                    parts.push({ type: "ok", src: c });
                } else if (
                    lastPart.errorIndices.toString() === errorIndices.toString()
                ) {
                    lastPart.src += c;
                } else {
                    parts.push({
                        type: "error",
                        src: c,
                        errorIndices,
                    });
                }
            }

            partsPerLine.push(parts);
        }

        const getErrorMsg = (errorIndex: number): string => {
            const [msg, startPos, startCol, endPos, endCol] =
                errors[errorIndex];
            return `${msg} (${startPos + 1}:${startCol + 1} - ${endPos + 1}:${
                endCol + 1
            })`;
        };

        return partsPerLine
            .map((parts) =>
                parts
                    .map((part) => {
                        if (part.type == "ok") {
                            return part.src;
                        } else {
                            const errorMsgs = part.errorIndices
                                .map(getErrorMsg)
                                .join("<br>");
                            const errorTooltip = `<span class="error-tooltip">${errorMsgs}</span>`;
                            return `<span class="error">${errorTooltip}${part.src}</span>`;
                        }
                    })
                    .join("")
            )
            .join("<br>");
    };

    const executeLexer = () => {
        const src = editorElement.innerText;

        const errors: LexError[] = (
            lexicalAnalysis(src) as Array<[string, number, number]>
        ).map(([msg, line, col]) => [msg, line, col, line, col]);

        if (errors.length > 0) console.error(errors);

        editorElement.innerHTML = insertErrorMsgs(src, errors);
    };
</script>

<main>
    <h1>WAXT Playground</h1>

    <div
        id="editor"
        contenteditable="true"
        spellcheck="false"
        bind:this={editorElement}
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
