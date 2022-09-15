export type LexError = [string, number, number, number, number];

export type Line = Array<{ c: string; errorIndices: number[] }>;

export type Part =
    | { type: "ok"; src: string }
    | { type: "error"; src: string; errorIndices: number[] };
