# Kalix Core Semantics

## Primitive Types

| Type | Size | Notes |
|------|------|-------|
| `u8`, `u16`, `u32`, `u64`, `u128` | fixed | Unsigned integers with wrap-free semantics; overflow raises a deterministic fault. |
| `i8`, `i16`, `i32`, `i64`, `i128` | fixed | Two's complement; overflow traps. |
| `bool` | 1 byte | Only `true` or `false`. |
| `bytes<N>` | N | Fixed-length byte array. |
| `bytes` | dynamic | Heap-backed byte slice with explicit bounds. |
| `string` | dynamic | UTF-8 encoded alias of `bytes`. |
| `Address` | 32 bytes | Hedera account or contract identifier. |
| `Hash` | 32 bytes | Keccak-256 digest. |

Numeric literals must fit the annotated type or the smallest default integer type able to contain them. Conversions require explicit `as` casts.

## Ownership & Borrowing

Kalix enforces deterministic borrowing rules inspired by Rust:

- `let` bindings are immutable by default; use `let mut` for mutable locals.
- `let const` introduces compile-time constants; the initializer must be a literal value.
- Only one mutable reference to a resource exists at a time.
- Borrow scopes follow lexical blocks; the compiler refuses re-entry while a mutable borrow is live.
- Contract state (`state <name>`) acts as a persistent resource. Reading requires an immutable borrow; writing requires exclusive access.
- No implicit copies for non-primitive aggregates; structs and tables move by default.

These rules prevent data races under concurrent execution and enable the compiler to reject non-deterministic aliasing.

## Contract Resources

- **State (`state <name>: <Type>;`)** defines a single storage slot or structure owned by the contract. Each state member receives a deterministic slot assignment used during ZVM lowering.
- **Table (`table <name>: Map<K, V>;`)** declares an associative container backed by storage hashing (slot + key) semantics. Tables are mutable resources and follow the same borrowing rules as state.
- **Event (`event <Name>(fields...);`)** declares a log payload layout. Emitting an event lowers to ZVM `LOG` opcodes with the declared parameter ordering.
- Resources respect visibility qualifiers (`pub`) and must be declared once per contract.

## Memory Model

- Memory allocations are deterministic and capped per transaction.
- Dynamic containers (tables, bytes, string) rely on a deterministic bump allocator. Exhaustion results in a recoverable error.
- Stack depth is bounded; recursion is disallowed during Phase 1.

## Error Model

- All functions return either a value or `Result<T, E>` via explicit return types.
- `panic` is illegal; use `return err(MyError)` for failure paths.
- Contract-level failures map to a ZVM revert opcode with the provided error code.
- Errors propagate with `try`-like sugar planned for later phases; Phase 1 uses explicit `match` statements.

## Deterministic Gas Semantics

- Each operation has a static gas cost defined in the backend (Phase 2).
- Phase 1 enforces cost-awareness by requiring explicit annotations on loops: `for (items) [gas: 5] { ... }` (syntax reserved).
- Heap and table operations include deterministic cost accounting emitted by the compiler.

## Hedera Integration Assumptions

- Access to Hedera services occurs via well-defined syscalls stubbed in Phase 1 tests.
- Account and token state transitions must be validated by the runtime; the language forbids implicit state mutation outside declared `state` members.

These semantics provide the foundation for deterministic compilation and prepare the pipeline for the ZVM backend work in Phase 2.
