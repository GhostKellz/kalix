# KALIX Roadmap
_A phased plan for delivering a Hedera-first smart contract language and tooling suite._

---

## Phase 1 — Language Foundations (Zig)
- **Goals**: Define the Kalix surface syntax, grammar, and deterministic semantics in Zig.
- **Deliverables**:
  - Author `docs/lang/syntax.md` and `docs/lang/grammar.peg`.
  - Implement lexer, parser, and AST modules under `src/frontend/`.
  - Establish primitive types, ownership rules, and error semantics documentation.
- **Exit Criteria**: Round-trip parser tests pass for seed programs; AST validation detects malformed input.

## Phase 2 — ZVM Backend
- **Goals**: Map Kalix IR to ZVM bytecode with Hedera syscall support.
- **Deliverables**:
  - Define IR structures and lowering passes in `src/backend/`.
  - Emit ZVM opcodes, bytecode writer, and fixture tests.
  - Draft syscall catalog for HTS/HCS interactions.
- **Exit Criteria**: Sample contracts compile to executable bytecode validated by a ZVM harness.

## Phase 3 — Hedera Integration
- **Goals**: Bridge Kalix runtime abstractions to Hedera network primitives.
- **Deliverables**:
  - Implement HTS, HCS, accounts, and ledger storage bindings.
  - Provide receipt translation utilities and consensus simulation scripts.
  - Document node setup and state mapping.
- **Exit Criteria**: Demo contract deploys to Hedera testnet with end-to-end event and state verification.

## Phase 4 — Tooling (Rust)
- **Goals**: Deliver developer-first tooling via CLI and language server.
- **Deliverables**:
  - `kalix-lsp`: built atop `ghostls` with `grove` tree-sitter grammar for syntax highlighting, hover, autocomplete, diagnostics, formatter hooks.
  - `kalixc` CLI: format, compile, deploy, bytecode inspection, ABI generation commands.
  - Testing harness with local VM simulator and transaction replay.
- **Exit Criteria**: Toolchain supports edit-compile-test loop with integration tests covering CLI workflows.

## Phase 5 — Security & PQC
- **Goals**: Harden Kalix with post-quantum cryptography and static analysis.
- **Deliverables**:
  - Integrate QCK signature suite and deterministic gas profiler.
  - Static analyzer for common contract vulnerabilities.
  - WASM sandbox mode and permission scoping policies.
- **Exit Criteria**: Security test suite passes; PQC crypto audited and benchmarked.

## Phase 6 — Ecosystem Infrastructure
- **Goals**: Establish package distribution and interoperability bridges.
- **Deliverables**:
  - `kalixpm` package manager with registry, resolver, semantic versioning, SBOM attestations.
  - Solidity → Kalix translator MVP; WASM bridge feasibility study.
  - Chainlink/HashLink oracle bindings and sample integrations.
  - Mirror node adapters for ingesting external oracle feeds into Kalix state models.
  - Cross-network bridge templates for synchronizing Hedera state with EVM rollups.
- **Exit Criteria**: Third-party package published via `kalixpm`; translator compiles reference contracts.

## Phase 7 — Launch Readiness
- **Goals**: Prepare public release, education, and marketing assets.
- **Deliverables**:
  - Documentation site, tutorials, and example contract catalog.
  - Developer outreach plan with Hedera ecosystem partners.
  - Launch presentation “Kalix: The Successor to Solidity.”
- **Exit Criteria**: Public beta announcement with onboarding funnel and feedback channels live.

## Phase 8 — Developer Adoption Programs
- **Goals**: Accelerate community growth and training.
- **Deliverables**:
  - Bootcamp curricula, certification tracks, and hackathon starter kits.
  - Community governance handbook and contributor guidelines.
  - Ambassador program with incentive structure.
- **Exit Criteria**: First cohort completes training; community maintainers onboarded.

## Phase 9 — Enterprise & Compliance Integration
- **Goals**: Enable regulated deployments and enterprise confidence.
- **Deliverables**:
  - Compliance templates for KYC/AML workflows and audit logging.
  - Enterprise-grade monitoring integrations (SIEM, observability).
  - Legal and risk documentation for Hedera-specific operations.
- **Exit Criteria**: Pilot enterprise deployment meets compliance review and emits verifiable audit trail.

## Phase 10 — Long-Term R&D
- **Goals**: Sustain innovation and prepare for future platform shifts.
- **Deliverables**:
  - Roadmap for multi-VM compilation targets (WASM, zkVM).
  - PQC upgrade path tracking NIST standardization.
  - Research partnerships for advanced type systems, formal verification, and gas economics.
- **Exit Criteria**: Annual R&D report published with funded initiatives and experimental prototypes.
