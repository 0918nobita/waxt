import { writable } from "svelte/store";

export type ErrorMarker = {
    msg: string;
    x: number;
    y: number;
    width: number;
    height: number;
};

export type State =
    | {
          type: "initial";
      }
    | {
          type: "wantLineHeight";
      }
    | {
          type: "wantInput";
          lineHeight: number;
          numLines: number;
          errorMarkers: ErrorMarker[];
      };

export const store = writable<State>({
    type: "initial",
});

export const computeEditorHeight = ({
    lineHeight,
    numLines,
}: {
    lineHeight: number;
    numLines: number;
}) => `calc(${lineHeight * (numLines - 1)}px + 100%)`;
