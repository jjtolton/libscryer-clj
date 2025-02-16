/* tslint:disable */
/* eslint-disable */
/**
*/
export class Machine {
  free(): void;
/**
* @param {string} query
* @returns {any}
*/
  runQuery(query: string): any;
}
/**
*/
export class MachineBuilder {
  free(): void;
/**
* @returns {MachineBuilder}
*/
  static new(): MachineBuilder;
/**
* @returns {Machine}
*/
  build(): Machine;
}
/**
*/
export class QueryState {
  free(): void;
/**
* @returns {any}
*/
  next(): any;
/**
*/
  drop(): void;
}

export type InitInput = RequestInfo | URL | Response | BufferSource | WebAssembly.Module;

export interface InitOutput {
  readonly memory: WebAssembly.Memory;
  readonly __wbg_machinebuilder_free: (a: number) => void;
  readonly machinebuilder_new: () => number;
  readonly machinebuilder_build: (a: number) => number;
  readonly __wbg_machine_free: (a: number) => void;
  readonly machine_runQuery: (a: number, b: number, c: number, d: number) => void;
  readonly __wbg_querystate_free: (a: number) => void;
  readonly querystate_next: (a: number, b: number) => void;
  readonly querystate_drop: (a: number) => void;
  readonly ring_core_0_17_8_bn_mul_mont: (a: number, b: number, c: number, d: number, e: number, f: number) => void;
  readonly __wbindgen_malloc: (a: number, b: number) => number;
  readonly __wbindgen_realloc: (a: number, b: number, c: number, d: number) => number;
  readonly __wbindgen_add_to_stack_pointer: (a: number) => number;
  readonly __wbindgen_exn_store: (a: number) => void;
}

export type SyncInitInput = BufferSource | WebAssembly.Module;
/**
* Instantiates the given `module`, which can either be bytes or
* a precompiled `WebAssembly.Module`.
*
* @param {SyncInitInput} module
*
* @returns {InitOutput}
*/
export function initSync(module: SyncInitInput): InitOutput;

/**
* If `module_or_path` is {RequestInfo} or {URL}, makes a request and
* for everything else, calls `WebAssembly.instantiate` directly.
*
* @param {InitInput | Promise<InitInput>} module_or_path
*
* @returns {Promise<InitOutput>}
*/
export default function __wbg_init (module_or_path?: InitInput | Promise<InitInput>): Promise<InitOutput>;
