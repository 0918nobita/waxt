import type { Part } from "./lexerType";

export const partsToHtml = (
    partsPerLine: ReadonlyArray<readonly Part[]>
): string =>
    partsPerLine
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
