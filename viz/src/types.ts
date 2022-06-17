export type StatusOfExecution = "continue" | "blocked" | "final";

export interface TapeStep {
  currentState: string;
  indexOnTape: number;
  status: StatusOfExecution;
  tape: string[];
}

export interface MachineExecution {
  blank: string;
  tapeHistory: TapeStep[];
}

export type AutomaticPlayingDelayMode = "MEDIUM" | "FAST";

export type PlayingStatus = "playing" | "paused" | "disabled";
