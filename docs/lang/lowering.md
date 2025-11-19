# Lowering Conventions

This note captures the current rules that the Kalix backend applies when converting the high-level AST into IR instructions. It is intentionally short so the lowering logic can evolve without re-reading the source code to understand expectations.

## Register Allocation
- Parameters and `let` bindings are assigned monotonically increasing local slots per function. Re-using a name is rejected during lowering.
- Temporary registers (`ir.Register.temp`) are generated for every expression that materialises a value. They are not re-used within a function; the code generator currently treats temporaries as stack positions.
- The lowering pass treats stack discipline explicitly: evaluating an expression leaves its value on the stack so subsequent IR instructions can consume it without extra moves.

## Control Flow
- Each function receives a dedicated entry label. Structured control flow (`if`, `while`, blocks) is lowered into explicit labels and jumps.
- `if` statements always emit a join label even when both branches return; this keeps the instruction stream well-formed for later analysis.
- `while` loops use three labels: condition, body, and exit. The body is only re-entered when it does not produce an early `return`.
- Cascaded `else if` chains reuse the same join handling, and boolean short-circuit expressions (`&&`, `||`, `!`) compile to dedicated logical IR instructions.

## Storage Resources
- Contract `state` declarations receive sequential 64-bit slots, followed by `table` declarations.
- Member accesses of the form `state.foo` become `load_state`/`store_state` IR instructions using the mapped slot.
- Table indexing (`state.foo[key]`) lowers into `load_table`/`store_table` instructions. The key expression is evaluated immediately before table operations so it remains on the stack for hashing.

## Table Hashing
- Both table loads and stores emit a `TABLEHASH` sequence in code generation: push slot, swap with the key, hash, then delegate to `SLOAD`/`SSTORE`.
- Lowering ensures the order of evaluation keeps the key above the value on the stack for stores, matching the hashing helper used in the harness.

## Fixture Generation
- `renderControlFlowFixture` renders a canonical instruction listing for a representative control-flow heavy contract.
- `verifyControlFlowFixture` regenerates the fixture and compares it to `src/backend/testdata/lowering/control_flow.json`. Running `zig build update-fixtures` rewrites the fixture when IR changes are intentional.

## Testing Checklist
- Add a unit test in `lowering.zig` whenever a new syntactic feature emits bespoke IR.
- Add corresponding code generation or harness tests to guarantee the emitted instructions execute as intended.
- Update the fixture (via the build step) when control flow output changes.

## Gas Reporting
- `CodeGen.emit` now tracks the latest gas report internally. Call `gasReport()` for read-only access or `takeGasReport()` to transfer ownership of the aggregated metrics (including per-function data).
- The artifact builder consumes this report directly, so downstream tooling receives the same accounting that the emitter used without recomputing totals.
