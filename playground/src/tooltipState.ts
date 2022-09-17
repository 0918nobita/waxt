export type TooltipState =
    | {
          type: "visible";
          msg: string;
          x: number;
          y: number;
      }
    | {
          type: "hidden";
      };
