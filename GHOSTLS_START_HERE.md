# GHOSTLS — START HERE

GhostLS is the upcoming language-server experience for Kalix. This guide explains how to consume the compiler front-end APIs that already expose syntax, semantic diagnostics, rich metadata, and ABI documents. Treat this as your launch checklist when bootstrapping the GhostLS repository.

---

## 1. Development Environment
- **Language + Tooling**: Zig 0.16.x (matching the Kalix repo toolchain).
- **Compiler API entrypoint**: `src/frontend/api.zig` provides `analyzeSource`, diagnostics, metadata, and ABI payloads in one call.
- **Tests**: `zig build test` should always pass before using a new Kalix snapshot inside GhostLS.

---

## 2. Consuming the Frontend API
1. Allocate the source buffer (UTF-8 Kalix contract/module).
2. Call `analyzeSource(allocator, source)`.
3. Inspect the returned `AnalysisResult`:
   - `diagnostics`: semantic issues with severity and messages.
   - `metadata`: symbol/type graph used for hovers, completions, and navigation.
   - `abis`: per-contract JSON ABI documents pre-built by the backend `AbiBuilder`.
4. Always call `result.deinit()` once you have copied any data that needs to live beyond the analysis frame.

### Sample Skeleton (pseudo-Zig)
```zig
const frontend = @import("kalix/src/frontend/api.zig");

fn analyze(allocator: std.mem.Allocator, source: []const u8) !void {
    var result = try frontend.analyzeSource(allocator, source);
    defer result.deinit();

    // use result.diagnostics, result.metadata, result.abis
}
```

---

## 3. Feature Map for GhostLS MVP
- **Lex / Syntax Highlights**: drive off AST spans produced during parsing (already part of `result.tree`).
- **Diagnostics Panel**: stream `result.diagnostics` back to the editor; messages originate from `semantics.describeError`.
- **Hover Metadata**: `result.metadata.contracts`, `states`, `tables`, `events`, and `functions` contain names, visibility, mutability, and type signatures.
- **Completions**: reuse metadata collections to propose identifiers within the active contract scope.
- **ABI Introspection**: `result.abis` provides stable JSON for code generation helpers, documentation, and UI renderings.
- **Gas/Storage Reporting (Stretch)**: the backend now instruments per-function gas/storage usage; once the CLI surfaces this data, GhostLS can present it inline.

---

## 4. Data Lifetimes & Allocators
- The frontend owns the returned buffers; GhostLS must copy data before `result.deinit()`.
- Diagnostics messages are heap-allocated strings; avoid holding raw pointers after teardown.
- ABI JSON strings are valid UTF-8 snapshots, suitable for direct serialization.

---

## 5. Planned Integrations (Next Sprint)
1. Expose gas/storage telemetry through a CLI command or JSON artifact.
2. Expand ABI fixtures to cover more contract shapes (multi-contract, indexed events, tables).
3. Publish stable metadata schemas so GhostLS can deserialize without Zig bindings.

---

## 6. Repository Wiring Checklist
- [ ] Vendor or submodule the `kalix` repo to access the frontend API.
- [ ] Add a thin FFI boundary (Zig → Rust/TypeScript) if GhostLS is implemented outside Zig.
- [ ] Implement a document manager that caches `AnalysisResult` per open file.
- [ ] Map Kalix diagnostics to LSP ranges using AST span information.
- [ ] Build hover/completion resolvers backed by `metadata` and `abis`.

---

## 7. Reference Tests
- `src/frontend/api.zig` tests validate ABI exposure under success/error paths.
- `src/backend/abi_builder.zig` golden fixtures guarantee ABI stability.
- `src/backend/codegen.zig` and `src/backend/gas.zig` tests cover bytecode emission and resource profiling.

Use these tests as guardrails when updating Kalix and GhostLS in tandem.

---

## 8. Support & Hand-off
- **Primary Contacts**: Kalix compiler team (`#kalix-dev` Slack channel).
- **Issue Tracker Tags**: `ghostls`, `lsp`, `tooling`.
- **Docs**: `docs/lang/`, `docs/zvm/`, and this file for onboarding.

Once GhostLS reaches MVP, migrate this guide into the GhostLS repository root and keep it updated alongside API changes.
