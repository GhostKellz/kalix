# Kalix Language Syntax Overview

Kalix is a contract-first language targeting the Hedera network and the ZVM runtime. The syntax borrows from Zig and Rust while enforcing deterministic semantics suitable for smart contracts.

## File Structure

A Kalix source file declares one or more `contract` blocks. Each block defines state, events, tables, and functions. Example:

```kalix
contract Treasury {
    state balance: u64;

    fn deposit(amount: u64) {
        state.balance += amount;
    }
}
```

## Lexical Elements

- **Identifiers**: start with `_` or ASCII letter; may contain digits and `_`.
- **Keywords**: `contract`, `state`, `table`, `event`, `fn`, `pub`, `let`, `const`, `return`, `if`, `else`, `true`, `false`, `mut`, `view`, `payable`.
- **Literals**:
  - Integers: decimal (`42`, `1_000`), hex (`0xFF`), binary (`0b1010`).
  - Strings: double-quoted UTF-8; escapes include `\n`, `\r`, `\t`, `\"`, `\\`.
  - Booleans: `true`, `false`.
- **Comments**: `//` single-line, `/* ... */` nested block comments.

## Declarations

### Contracts

```
contract <Identifier> { ContractItem* }
```

Contract items may be state declarations, events, tables, or functions.

### State

```
state <Identifier>: <Type>;
```

Optionally annotate with visibility modifiers:

```
state pub ledger: Map<Address, Token>;
```

### Table

```
table <Identifier>: Map<KeyType, ValueType>;
```

Tables provide deterministic associative storage. Visibility modifiers (`pub`) follow the same rules as state declarations.

### Event

```
event <Identifier>(FieldList?);
```

Event fields use the same `<name>: <Type>` syntax as function parameters and are immutable. Events describe structured log payloads emitted by contracts.

### Functions

```
fn <Identifier>(ParamList?) (-> <Type>)? Block
```

- Parameters support immutability specifiers: `mut` or `const`.
- A return type defaults to `()` (unit) when omitted.
- Functions may declare capability modifiers such as `view` or `payable` before the return arrow.

## Statements

- `let` / `const` bindings: `let mut temp = amount;`, `let const limit = 100;`
- Assignment: `identifier = expression;`
- Return: `return expression;`
- Conditional: `if condition { ... } else { ... }`
- Block: `{ Statement* }`

`state` bindings are accessed via `state.<name>` within contract scope. Tables and events will receive dedicated helper APIs in Phase 2.

## Expressions

Operators follow Zig precedence; Kalix restricts to deterministic forms:

1. Member access: `expr.member`
2. Call: `callee(arguments)`
3. Unary: `!`, `-`, `not`
4. Multiplicative: `*`, `/`, `%`
5. Additive: `+`, `-`
6. Comparisons: `<`, `>`, `<=`, `>=`
7. Equality: `==`, `!=`
8. Logical and: `&&`
9. Logical or: `||`

Short-circuit semantics apply to `&&` and `||`.

## Modules and Imports

Phase 1 supports single-file contracts. A future phase introduces module imports via `use` statements.

## Determinism Notes

- No implicit type coercions; all conversions are explicit.
- Integer literals adopt the smallest fitting type or must be annotated.
- Strings are immutable byte slices; operations are bounded.

Refer to `docs/lang/grammar.peg` for a precise grammar.
