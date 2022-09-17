import type { LexError, Line, Part } from "./lexerType";

const splitPerLine = (src: string): Line[] =>
    [...src].reduce(
        (lines: Line[], c: string) =>
            c === "\n"
                ? [...lines, []]
                : [
                      ...lines.slice(0, -1),
                      [...lines.slice(-1)[0], { c, errorIndices: [] }],
                  ],
        [[]]
    );

const registerErrorIndices = (
    lines: Line[],
    errors: ReadonlyArray<LexError>
) => {
    for (let i = 0; i < errors.length; i++) {
        const [_, startLine, startCol, endLine, endCol] = errors[i];

        for (let line = startLine; line <= endLine; line++)
            for (let col = startCol; col <= endCol; col++)
                lines[line][col].errorIndices.push(i);
    }
};

const getPartsPerLine = (lines: Line[]): Array<Part[]> => {
    const partsPerLine: Array<Part[]> = [];

    for (let lineIndex = 0; lineIndex < lines.length; lineIndex++) {
        const parts: Part[] = [];

        for (let colIndex = 0; colIndex < lines[lineIndex].length; colIndex++) {
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

    return partsPerLine;
};

export const insertErrorElements = (
    src: string,
    errors: LexError[]
): string => {
    let lines = splitPerLine(src);

    registerErrorIndices(lines, errors);

    return getPartsPerLine(lines)
        .map((parts) =>
            parts
                .map((part) => {
                    if (part.type == "ok") {
                        return part.src;
                    } else {
                        const errorIndices = part.errorIndices.toString();
                        return `<span class="error" data-error-indices="${errorIndices}">${part.src}</span>`;
                    }
                })
                .join("")
        )
        .join("<br>");
};
