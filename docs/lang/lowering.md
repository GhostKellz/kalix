# Kalix Lowering Conventions

This memo captures the rules Phase 2 uses when translating the semantic AST into the Kalix IR and downstream ZVM bytecode.

## Register Model

- IR registers are symbolic handles (`stack`, `temp`, `constant`).
- `stack` indices correspond to function-local slots (parameters + `let` bindings) allocated sequentially.
- `temp` indices denote transient values produced by expression lowering; lifetimes are single-expression.
- Constants embed the literal value (`U256`) directly and do not allocate storage.

## Local Bindings

- Parameters are assigned slots in declaration order beginning at `0`.
- `let` bindings allocate the next available slot; rebinding the same name is rejected by the semantic stage and treated as an error during lowering.
- Reads of locals emit `load_local` into a fresh temp register; writes emit `store_local` with the slot and source register.

## State Access

- Each contract `state` member receives a deterministic storage slot via `StateMapper`.
- `state.foo` expression emits `load_state` into a temp register.
- `state.foo = expr` emits `store_state` with the resolved slot and the value register.
- `table` access is not yet lowered; upcoming work will extend the mapper to hashed keys.

## Expressions Supported

- Arithmetic operators `+ - * / %` lower to the corresponding `add/sub/mul/div/mod_` IR variants.
- Integer literals must be non-negative; negative literals require an explicit unary minus handling pass.
- Boolean literals map to constants `0`/`1`.
- Identifier references resolve to local loads; `state` identifiers are only valid as member bases.
- Assignment expressions return the stored register to enable chaining.

## Control Flow

- Each function receives a synthetic `label` at entry and a trailing `ret` if no explicit return occurs.
- `return` statements emit `ret` immediately and short-circuit further lowering for the enclosing block.
- Branching constructs are not yet lowered; future milestones will introduce block scoping and jump targets.

## Code Generation Bridge

- `codegen.CodeGen` currently maps arithmetic/local/state IR instructions to pseudo-ZVM opcodes for smoke tests.
- Real bytecode emission will later replace the placeholder opcodes with canonical ZVM encodings and integrate stack scheduling.

These conventions span the current lowering pass and will evolve as we introduce control flow, tables, events, and full bytecode emission.
