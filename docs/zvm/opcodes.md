# ZVM Opcode & Syscall Catalog

This catalog summarizes the ZVM instructions and Hedera syscalls relevant to Kalix Phase 2 backend work. Use it as the authoritative reference when designing IR lowering and bytecode emission.

## Core Arithmetic & Logic

| Opcode | Stack Effect | Notes |
|--------|--------------|-------|
| `ADD` | `(a, b) -> (a + b)` | Unsigned addition, traps on overflow. |
| `SUB` | `(a, b) -> (a - b)` | Wrap-free subtraction. |
| `MUL` | `(a, b) -> (a * b)` | Deterministic multiplication. |
| `DIV` | `(a, b) -> (a / b)` | Division by zero traps. |
| `MOD` | `(a, b) -> (a % b)` | Remainder operation. |
| `ADDMOD` | `(a, b, m) -> ((a + b) mod m)` | Useful for bounded arithmetic. |
| `EXP` | `(base, exponent) -> base^exponent` | Gas-heavy; avoid in Phase 1 contracts. |

## Comparison & Flow Control

| Opcode | Stack Effect | Notes |
|--------|--------------|-------|
| `LT` | `(a, b) -> (a < b)` | Returns `0` or `1`. |
| `GT` | `(a, b) -> (a > b)` | |
| `EQ` | `(a, b) -> (a == b)` | |
| `ISZERO` | `(a) -> (a == 0)` | |
| `JUMP` | `(target) -> ()` | Unconditional jump to absolute offset. |
| `JUMPI` | `(target, cond) -> ()` | Conditional jump when `cond != 0`. |
| `HALT` | `() -> ()` | Terminates execution successfully. |
| `REVERT` | `(code) -> ()` | Terminates with failure; code surfaced to caller. |

## Memory & Storage

| Opcode | Stack Effect | Notes |
|--------|--------------|-------|
| `PUSH<N>` | `() -> (value)` | Push immediate value (`1 <= N <= 32`). |
| `POP` | `(a) -> ()` | Discard value. |
| `DUP<N>` | `(…, x_N, …) -> (…, x_N, x_N, …)` | Duplicate stack slot. |
| `SWAP<N>` | Swap top-of-stack with deeper element. |
| `MLOAD/MSTORE` | `(offset) -> (word)` / `(offset, word)` | Linear memory access. |
| `SLOAD` | `(slot) -> (word)` | Persistent contract storage read. |
| `SSTORE` | `(slot, word) -> ()` | Persistent contract storage write. |
| `TLOAD` | `(slot) -> (word)` | Transient storage read (cleared per tx). |
| `TSTORE` | `(slot, word) -> ()` | Transient storage write. |

## Logging & Events

| Opcode | Stack Effect | Notes |
|--------|--------------|-------|
| `LOG0` | `(offset, length) -> ()` | Emit event without topics. |
| `LOG1` | `(topic, offset, length) -> ()` | Emit event with one topic. |
| `LOG2` | `(topic1, topic2, offset, length)` | Additional LOG opcodes exist up to `LOG4`. |

## Hedera Token Service (HTS) Syscalls

| Syscall | Stack Inputs | Stack Output | Description |
|---------|-------------|--------------|-------------|
| `HTS_TRANSFER` | `token_id, from, to, amount` | `status` | Transfer fungible tokens. |
| `HTS_MINT` | `token_id, amount` | `status` | Mint tokens to treasury. |
| `HTS_BURN` | `token_id, amount` | `status` | Burn tokens. |
| `HTS_ASSOCIATE` | `account, token_id` | `status` | Associate account with token. |
| `HTS_CREATE` | `params_offset, params_len` | `status` | Create new token. |

## Hedera Consensus Service (HCS) Syscalls

| Syscall | Stack Inputs | Stack Output | Description |
|---------|-------------|--------------|-------------|
| `HCS_SUBMIT` | `topic_id, message_offset, message_len` | `status` | Submit message to topic. |
| `HCS_CREATE_TOPIC` | `params_offset, params_len` | `status` | Create new topic. |

## Integration Notes

- HTS/HCS syscalls return a status word (`1` success, `0` failure). Kalix lowering should wrap these in `Result<(), HederaError>` helpers.
- Gas accounting for syscalls is additive to intrinsic instruction costs; profile each syscall when building the gas analyzer.
- Storage slots follow Solidity-style layout: sequential assignment for simple values, `keccak(slot ++ key)` for mappings (Kalix tables).
- LOG opcodes expect ABI-encoded payloads in memory; Kalix event lowering will allocate buffers, write fields, then invoke the appropriate `LOG<N>` instruction.

Keep this document updated as ZVM introduces new instructions or Hedera extends its syscall surface.
