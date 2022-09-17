import type { LexError } from "./lexerType";

export const stringifyLexError = (
    errors: ReadonlyArray<LexError>,
    errorIndex: number
): string => {
    const [msg, startPos, startCol, endPos, endCol] = errors[errorIndex];
    return `${msg} (${startPos + 1}:${startCol + 1} - ${endPos + 1}:${
        endCol + 1
    })`;
};
