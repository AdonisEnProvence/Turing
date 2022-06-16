export interface TapeStep {
    currentState: string;
    indexOnTape: number;
    status: "continue" | 'blocked' | 'final'
    tape: string[]
}

export interface MachineExecution {
    blank: string;
    tapeHistory: TapeStep[]
}

export type AutomaticPlayingDelayMode = "MEDIUM" | "FAST";
