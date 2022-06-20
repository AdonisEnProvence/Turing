import SampleMachines from "../../../our-machines/input-samples.json";

const AllMachinesAsStrings = import.meta.globEager(
  "../../../our-machines/*.json",
  {
    as: "raw",
  }
);

export const sampleMachinesName = Object.keys(SampleMachines);

export function getMachineCodeAndInput(machineName: string): {
  machineCode: string;
  input: string;
} {
  return {
    machineCode: AllMachinesAsStrings[
      `../../../our-machines/${machineName}.json`
    ] as unknown as string,
    input: (SampleMachines as Record<string, string>)[machineName],
  };
}
