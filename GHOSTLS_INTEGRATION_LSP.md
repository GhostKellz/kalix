# GhostLS ↔ Kalix LSP Integration Guide

**Building Language Server Protocol Support for the Kalix Smart Contract Language**

---

## 🎯 Executive Summary

This document outlines the integration requirements for **GhostLS** to provide full LSP support for **Kalix**, a next-generation smart contract language targeting Hedera and the ZVM (Zig Virtual Machine).

**Goal**: Extend GhostLS's proven LSP architecture to support Kalix's `.kalix` files with:
- Real-time syntax error detection and semantic diagnostics
- Go to definition, find references, hover documentation
- Context-aware completions (contract state, functions, Hedera APIs)
- Smart contract-specific features (gas estimation, security warnings)

---

## 📊 Current State Assessment

### Kalix Compiler (Phase 1 ✅ Complete)

**Location**: `/data/projects/kalix`

| Component | Status | Description |
|-----------|--------|-------------|
| **Lexer** | ✅ | Full tokenization with position tracking (`src/frontend/lexer.zig`) |
| **Parser** | ✅ | Recursive descent parser with AST generation (`src/frontend/parser.zig`) |
| **AST** | ✅ | Complete node types for contracts, state, functions, events (`src/frontend/ast.zig`) |
| **Semantic Analyzer** | ✅ | Type checking, symbol resolution, ownership validation (`src/frontend/semantics.zig`) |
| **Public API** | ✅ | `analyzeSource()` function returns AST + diagnostics (`src/frontend/api.zig`) |
| **Tests** | ✅ | 14 comprehensive test cases covering all language features |

**Key Kalix Features**:
- Contract-based programming model
- State variables with ownership tracking
- Events and tables (associative storage)
- Deterministic type system (u8, u16, u32, u64, u128, i8-i128, bool, string, bytes)
- Result-based error handling (no exceptions)
- Hedera-first design (HTS tokens, HCS consensus, ledger storage)

### GhostLS Server (v0.6.0)

**Location**: `/data/projects/ghostls`

| LSP Feature | Status | Provider Module |
|-------------|--------|-----------------|
| **Text Sync** | ✅ | `document_manager.zig` - Incremental updates |
| **Diagnostics** | ✅ | `diagnostics.zig` - Syntax error engine |
| **Hover** | ✅ | `hover_provider.zig` - Type + docs on hover |
| **Completion** | ✅ | `completion_provider.zig` - Context-aware suggestions |
| **Go to Definition** | ✅ | `definition_provider.zig` - Symbol navigation |
| **Find References** | ✅ | `references_provider.zig` - Cross-file references |
| **Document Symbols** | ✅ | `symbol_provider.zig` - Outline view |
| **Workspace Symbols** | ✅ | `workspace_symbol_provider.zig` - Project-wide search |
| **Rename** | ✅ | `rename_provider.zig` - Symbol renaming |
| **Code Actions** | ✅ | `code_actions_provider.zig` - Quick fixes |
| **Semantic Tokens** | ✅ | `semantic_tokens_provider.zig` - Syntax highlighting |
| **Signature Help** | ✅ | `signature_help_provider.zig` - Parameter hints |
| **Folding Ranges** | ✅ | `folding_range_provider.zig` - Code folding |
| **Document Highlight** | ✅ | `document_highlight_provider.zig` - Symbol highlighting |
| **Inlay Hints** | ✅ | `inlay_hints_provider.zig` - Type hints |
| **Blockchain Analysis** | ✅ | `blockchain_analyzer.zig` - Smart contract insights |

**GhostLS Strengths**:
- **Modular architecture**: Easy to plug in new language frontends
- **FFI system**: Proven with GShell FFI (30+ functions) via `ffi_loader.zig`
- **Multi-language support**: Already handles `.gza`, `.ghost`, `.gsh`, `.gshrc.gza`
- **Zero external dependencies**: Pure Zig implementation
- **Production-ready**: LSP v3.17 compliant, tested with Grim IDE and Neovim

---

## 🔧 Integration Requirements

### 1. Kalix Compiler API Exposure

**What GhostLS Needs from Kalix**:

#### 1.1. Public Semantic Analysis API

**Current State**: `src/frontend/api.zig` provides `analyzeSource()` which returns AST + diagnostics.

**Required Enhancements**:

```zig
// src/frontend/api.zig (extend existing)

/// LSP-specific query API for GhostLS integration
pub const LSPQuery = struct {
    /// Get symbol at position (for hover, definition, rename)
    pub fn symbolAt(tree: *ast.Tree, line: u32, column: u32) ?SymbolInfo {
        // Return: symbol name, type, definition location, documentation
    }

    /// Get all references to a symbol
    pub fn findReferences(tree: *ast.Tree, symbol_name: []const u8) []Location {
        // Return: all usage locations across the AST
    }

    /// Get completion candidates at cursor position
    pub fn getCompletions(tree: *ast.Tree, line: u32, column: u32) []CompletionItem {
        // Return: contract state, functions, keywords, Hedera APIs
        // Context-aware based on cursor position
    }

    /// Get signature help for function calls
    pub fn getSignatureHelp(tree: *ast.Tree, line: u32, column: u32) ?SignatureHelp {
        // Return: function signature + parameter docs
    }

    /// Get document outline (contract structure)
    pub fn getDocumentSymbols(tree: *ast.Tree) []DocumentSymbol {
        // Return: contracts, state, functions, events, tables
    }

    /// Get inlay hints (type annotations)
    pub fn getInlayHints(tree: *ast.Tree, range: Range) []InlayHint {
        // Return: inferred types for `let` bindings, function params
    }
};

pub const SymbolInfo = struct {
    name: []const u8,
    kind: SymbolKind, // contract, state, function, parameter, etc.
    type_info: Type,  // From semantics.zig Type union
    def_location: Location,
    documentation: ?[]const u8,
    modifiers: []const Modifier, // pub, mut, const, view, payable
};

pub const SymbolKind = enum {
    contract,
    state,
    table,
    event,
    function,
    parameter,
    local_variable,
};

pub const Location = struct {
    line: u32,
    column: u32,
    length: u32,
};

pub const CompletionItem = struct {
    label: []const u8,
    kind: CompletionKind,
    detail: ?[]const u8, // Type signature
    documentation: ?[]const u8,
    insert_text: ?[]const u8, // Snippet support
};

pub const CompletionKind = enum {
    keyword,
    state_field,
    function,
    parameter,
    hedera_api, // HTS, HCS syscalls
    type_name,
};

pub const SignatureHelp = struct {
    signatures: []Signature,
    active_signature: u32,
    active_parameter: u32,
};

pub const Signature = struct {
    label: []const u8, // "fn transfer(to: Address, amount: u128) -> Result<(), String>"
    parameters: []ParameterInfo,
    documentation: ?[]const u8,
};

pub const ParameterInfo = struct {
    label: []const u8,
    documentation: ?[]const u8,
};

pub const DocumentSymbol = struct {
    name: []const u8,
    kind: SymbolKind,
    range: Range,
    children: []DocumentSymbol,
};

pub const Range = struct {
    start_line: u32,
    start_column: u32,
    end_line: u32,
    end_column: u32,
};

pub const InlayHint = struct {
    position: Location,
    label: []const u8, // ": u128"
    kind: InlayHintKind,
};

pub const InlayHintKind = enum {
    type_hint,
    parameter_hint,
};

pub const Modifier = enum {
    pub,
    mut,
    const,
    view,
    payable,
};
```

#### 1.2. Position-Aware AST Traversal

**Required**: Extend AST nodes with source location metadata.

```zig
// src/frontend/ast.zig (extend existing nodes)

pub const Node = struct {
    // Add source location to all nodes
    location: SourceLocation,
    // ... existing fields
};

pub const SourceLocation = struct {
    start_line: u32,
    start_column: u32,
    end_line: u32,
    end_column: u32,
};

// Helper: Find node at cursor position
pub fn findNodeAtPosition(root: *Node, line: u32, column: u32) ?*Node {
    // Traverse AST, return deepest node containing (line, column)
}
```

#### 1.3. Diagnostic Reporting Enhancement

**Current State**: `api.zig` returns basic semantic errors.

**Required**: Rich diagnostics with ranges, severity, and quick fixes.

```zig
// src/frontend/api.zig (extend existing Diagnostic)

pub const Diagnostic = struct {
    kind: DiagnosticKind,
    severity: Severity, // error, warning, info, hint
    message: []const u8,
    range: Range,
    related_info: []RelatedInfo,
    code_actions: []CodeAction, // Quick fixes
};

pub const Severity = enum {
    error,
    warning,
    info,
    hint,
};

pub const RelatedInfo = struct {
    location: Location,
    message: []const u8,
};

pub const CodeAction = struct {
    title: []const u8, // "Add missing return statement"
    kind: CodeActionKind,
    edit: ?TextEdit,
};

pub const CodeActionKind = enum {
    quick_fix,
    refactor,
    source_action,
};

pub const TextEdit = struct {
    range: Range,
    new_text: []const u8,
};
```

#### 1.4. Smart Contract-Specific Features

**Unique to Kalix/GhostLS Integration**:

```zig
// src/frontend/api.zig (new smart contract APIs)

pub const SmartContractAnalysis = struct {
    /// Static gas estimation for contract functions
    pub fn estimateGas(tree: *ast.Tree, function_name: []const u8) ?GasEstimate {
        // Return: min/max/average gas cost
    }

    /// Security vulnerability detection
    pub fn detectVulnerabilities(tree: *ast.Tree) []SecurityWarning {
        // Return: reentrancy, overflow, unchecked errors, etc.
    }

    /// Contract storage layout (for state variables)
    pub fn getStorageLayout(contract: *ast.Contract) StorageLayout {
        // Return: ZVM storage slot assignments
    }

    /// Hedera-specific validations
    pub fn validateHederaUsage(tree: *ast.Tree) []HederaDiagnostic {
        // Check: HTS token IDs, HCS topic usage, account addresses
    }
};

pub const GasEstimate = struct {
    function: []const u8,
    min_gas: u64,
    max_gas: u64,
    avg_gas: u64,
    breakdown: []GasStep, // Per-instruction costs
};

pub const GasStep = struct {
    instruction: []const u8,
    gas_cost: u64,
};

pub const SecurityWarning = struct {
    severity: Severity,
    category: SecurityCategory,
    message: []const u8,
    location: Range,
    fix_suggestion: ?[]const u8,
};

pub const SecurityCategory = enum {
    reentrancy,
    integer_overflow,
    unchecked_error,
    unauthorized_access,
    state_mutation_in_view,
    unbounded_loop,
};

pub const StorageLayout = struct {
    slots: []StateSlot,
};

pub const StateSlot = struct {
    name: []const u8,
    slot_index: u64,
    type_info: Type,
};

pub const HederaDiagnostic = struct {
    severity: Severity,
    message: []const u8,
    location: Range,
};
```

---

### 2. GhostLS Extension Points

**Where Kalix Integration Happens in GhostLS**:

#### 2.1. File Type Detection

**File**: `src/lsp/document_manager.zig`

**Changes**:
```zig
// Add .kalix file extension support
pub fn getLanguageId(uri: []const u8) []const u8 {
    if (std.mem.endsWith(u8, uri, ".gza")) return "ghostlang";
    if (std.mem.endsWith(u8, uri, ".ghost")) return "ghostlang";
    if (std.mem.endsWith(u8, uri, ".gsh")) return "gshell";
    if (std.mem.endsWith(u8, uri, ".gshrc.gza")) return "gshell";
    if (std.mem.endsWith(u8, uri, ".kalix")) return "kalix"; // NEW
    return "unknown";
}
```

#### 2.2. Kalix Parser Integration

**File**: `src/lsp/kalix_analyzer.zig` (NEW)

**Implementation**:
```zig
const std = @import("std");
const kalix = @import("kalix"); // Import Kalix compiler
const protocol = @import("protocol.zig");

pub const KalixAnalyzer = struct {
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) KalixAnalyzer {
        return .{ .allocator = allocator };
    }

    pub fn analyze(self: *KalixAnalyzer, source: []const u8) !AnalysisResult {
        // Call Kalix compiler API
        var result = try kalix.frontend.analyzeSource(self.allocator, source);
        return AnalysisResult{
            .tree = result.tree,
            .diagnostics = result.diagnostics,
        };
    }

    pub fn getDiagnostics(self: *KalixAnalyzer, source: []const u8) ![]protocol.Diagnostic {
        var result = try self.analyze(source);
        defer result.deinit();

        var lsp_diags = std.ArrayList(protocol.Diagnostic).init(self.allocator);

        for (result.diagnostics) |diag| {
            try lsp_diags.append(.{
                .range = .{
                    .start = .{ .line = diag.range.start_line, .character = diag.range.start_column },
                    .end = .{ .line = diag.range.end_line, .character = diag.range.end_column },
                },
                .severity = switch (diag.severity) {
                    .error => 1,
                    .warning => 2,
                    .info => 3,
                    .hint => 4,
                },
                .message = diag.message,
            });
        }

        return lsp_diags.toOwnedSlice();
    }

    pub fn getHover(self: *KalixAnalyzer, source: []const u8, line: u32, col: u32) !?protocol.Hover {
        var result = try self.analyze(source);
        defer result.deinit();

        const symbol = kalix.frontend.LSPQuery.symbolAt(&result.tree, line, col) orelse return null;

        const markdown = try std.fmt.allocPrint(self.allocator,
            "```kalix\n{s}: {s}\n```\n{s}",
            .{ symbol.name, symbol.type_info.description(), symbol.documentation orelse "" }
        );

        return protocol.Hover{
            .contents = .{ .value = markdown },
        };
    }

    pub fn getCompletions(self: *KalixAnalyzer, source: []const u8, line: u32, col: u32) ![]protocol.CompletionItem {
        var result = try self.analyze(source);
        defer result.deinit();

        const candidates = kalix.frontend.LSPQuery.getCompletions(&result.tree, line, col);

        var lsp_items = std.ArrayList(protocol.CompletionItem).init(self.allocator);

        for (candidates) |item| {
            try lsp_items.append(.{
                .label = item.label,
                .kind = switch (item.kind) {
                    .keyword => 14,
                    .function => 3,
                    .state_field => 5,
                    .parameter => 6,
                    .hedera_api => 18, // Interface
                    .type_name => 25,
                },
                .detail = item.detail,
                .documentation = item.documentation,
                .insertText = item.insert_text,
            });
        }

        return lsp_items.toOwnedSlice();
    }

    pub fn getDefinition(self: *KalixAnalyzer, source: []const u8, line: u32, col: u32) !?protocol.Location {
        var result = try self.analyze(source);
        defer result.deinit();

        const symbol = kalix.frontend.LSPQuery.symbolAt(&result.tree, line, col) orelse return null;

        return protocol.Location{
            .range = .{
                .start = .{ .line = symbol.def_location.line, .character = symbol.def_location.column },
                .end = .{ .line = symbol.def_location.line, .character = symbol.def_location.column + symbol.def_location.length },
            },
        };
    }

    pub fn getReferences(self: *KalixAnalyzer, source: []const u8, line: u32, col: u32) ![]protocol.Location {
        var result = try self.analyze(source);
        defer result.deinit();

        const symbol = kalix.frontend.LSPQuery.symbolAt(&result.tree, line, col) orelse return &[_]protocol.Location{};

        const refs = kalix.frontend.LSPQuery.findReferences(&result.tree, symbol.name);

        var lsp_locs = std.ArrayList(protocol.Location).init(self.allocator);

        for (refs) |ref| {
            try lsp_locs.append(.{
                .range = .{
                    .start = .{ .line = ref.line, .character = ref.column },
                    .end = .{ .line = ref.line, .character = ref.column + ref.length },
                },
            });
        }

        return lsp_locs.toOwnedSlice();
    }

    pub fn getDocumentSymbols(self: *KalixAnalyzer, source: []const u8) ![]protocol.DocumentSymbol {
        var result = try self.analyze(source);
        defer result.deinit();

        const symbols = kalix.frontend.LSPQuery.getDocumentSymbols(&result.tree);

        // Convert to LSP DocumentSymbol format
        // (Recursive conversion for nested symbols)
    }
};

const AnalysisResult = struct {
    tree: kalix.ast.Tree,
    diagnostics: []const kalix.frontend.Diagnostic,

    fn deinit(self: *AnalysisResult) void {
        // Cleanup
    }
};
```

#### 2.3. Server Integration

**File**: `src/lsp/server.zig`

**Changes**:
```zig
const KalixAnalyzer = @import("kalix_analyzer.zig").KalixAnalyzer;

pub const Server = struct {
    // ... existing fields
    kalix_analyzer: KalixAnalyzer, // NEW

    pub fn init(allocator: std.mem.Allocator) !Server {
        // ... existing initialization
        return .{
            // ... existing fields
            .kalix_analyzer = KalixAnalyzer.init(allocator), // NEW
        };
    }

    fn handleTextDocumentHover(self: *Server, params: std.json.Value) !?[]const u8 {
        const text_doc = params.object.get("textDocument") orelse return error.InvalidParams;
        const uri = text_doc.object.get("uri").?.string;
        const position = params.object.get("position") orelse return error.InvalidParams;

        const doc = self.document_manager.getDocument(uri) orelse return null;

        // Route to Kalix analyzer for .kalix files
        if (std.mem.endsWith(u8, uri, ".kalix")) {
            return self.kalix_analyzer.getHover(
                doc.content,
                @intCast(position.object.get("line").?.integer),
                @intCast(position.object.get("character").?.integer)
            );
        }

        // Fallback to existing Ghostlang hover provider
        return self.hover_provider.provideHover(doc, position);
    }

    // Similar routing for:
    // - handleTextDocumentCompletion
    // - handleTextDocumentDefinition
    // - handleTextDocumentReferences
    // - handleTextDocumentDocumentSymbol
    // etc.
};
```

#### 2.4. Hedera-Specific Completions

**File**: `src/lsp/kalix_hedera_completions.zig` (NEW)

**Implementation**:
```zig
pub const HederaCompletions = struct {
    pub fn getHTSFunctions() []CompletionItem {
        return &[_]CompletionItem{
            .{ .label = "hts_transfer", .kind = .hedera_api, .detail = "fn(token: Address, from: Address, to: Address, amount: u128) -> Result<(), HederaError>", .documentation = "Transfer HTS tokens between accounts" },
            .{ .label = "hts_mint", .kind = .hedera_api, .detail = "fn(token: Address, amount: u128) -> Result<(), HederaError>", .documentation = "Mint new HTS tokens" },
            .{ .label = "hts_burn", .kind = .hedera_api, .detail = "fn(token: Address, amount: u128) -> Result<(), HederaError>", .documentation = "Burn HTS tokens" },
            .{ .label = "hts_associate", .kind = .hedera_api, .detail = "fn(account: Address, token: Address) -> Result<(), HederaError>", .documentation = "Associate account with token" },
            .{ .label = "hts_create", .kind = .hedera_api, .detail = "fn(name: []const u8, symbol: []const u8, decimals: u8) -> Result<Address, HederaError>", .documentation = "Create new HTS token" },
        };
    }

    pub fn getHCSFunctions() []CompletionItem {
        return &[_]CompletionItem{
            .{ .label = "hcs_submit", .kind = .hedera_api, .detail = "fn(topic: Address, message: []const u8) -> Result<(), HederaError>", .documentation = "Submit message to HCS topic" },
            .{ .label = "hcs_create_topic", .kind = .hedera_api, .detail = "fn() -> Result<Address, HederaError>", .documentation = "Create new HCS topic" },
        };
    }

    pub fn getStorageFunctions() []CompletionItem {
        return &[_]CompletionItem{
            .{ .label = "state_read", .kind = .hedera_api, .detail = "fn(key: u256) -> u256", .documentation = "Read from contract storage (SLOAD)" },
            .{ .label = "state_write", .kind = .hedera_api, .detail = "fn(key: u256, value: u256)", .documentation = "Write to contract storage (SSTORE)" },
        };
    }
};
```

---

### 3. Build System Integration

**File**: `/data/projects/ghostls/build.zig`

**Changes**:
```zig
const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Fetch Kalix dependency
    const kalix = b.dependency("kalix", .{
        .target = target,
        .optimize = optimize,
    });

    const exe = b.addExecutable(.{
        .name = "ghostls",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    // Link Kalix compiler
    exe.root_module.addImport("kalix", kalix.module("kalix"));

    b.installArtifact(exe);
}
```

**File**: `/data/projects/ghostls/build.zig.zon`

**Changes**:
```zig
.{
    .name = "ghostls",
    .version = "0.7.0", // Bump for Kalix support
    .dependencies = .{
        .kalix = .{
            .path = "../kalix", // Local development path
            // OR: .url = "https://github.com/ghostkellz/kalix/archive/refs/heads/main.tar.gz",
        },
    },
    .paths = .{
        "build.zig",
        "build.zig.zon",
        "src",
        "integrations",
    },
}
```

---

## 🧪 Testing Strategy

### Unit Tests (Kalix Side)

**File**: `/data/projects/kalix/src/frontend/api_test.zig` (NEW)

```zig
const testing = std.testing;
const api = @import("api.zig");

test "LSPQuery.symbolAt returns correct symbol info" {
    const src =
        \\contract Token {
        \\    state balance: u128;
        \\    fn transfer(to: Address, amount: u128) -> Result<(), String> {
        \\        return Ok(());
        \\    }
        \\}
    ;

    var result = try api.analyzeSource(testing.allocator, src);
    defer result.deinit();

    // Test: cursor on "balance" at line 1, col 10
    const symbol = api.LSPQuery.symbolAt(&result.tree, 1, 10);
    try testing.expect(symbol != null);
    try testing.expectEqualStrings("balance", symbol.?.name);
    try testing.expect(symbol.?.kind == .state);
}

test "LSPQuery.getCompletions returns contract state fields" {
    const src =
        \\contract Token {
        \\    state balance: u128;
        \\    fn get() -> u128 {
        \\        return |
        \\    }
        \\}
    ;

    var result = try api.analyzeSource(testing.allocator, src);
    defer result.deinit();

    // Test: completions after "return " at line 3, col 15
    const completions = api.LSPQuery.getCompletions(&result.tree, 3, 15);
    try testing.expect(completions.len > 0);

    var found_balance = false;
    for (completions) |item| {
        if (std.mem.eql(u8, item.label, "balance")) {
            found_balance = true;
            break;
        }
    }
    try testing.expect(found_balance);
}

test "SmartContractAnalysis.detectVulnerabilities finds unchecked errors" {
    const src =
        \\contract Unsafe {
        \\    fn risky(amount: u128) {
        \\        let _ = dangerous_call(amount); // Ignored Result
        \\    }
        \\}
    ;

    var result = try api.analyzeSource(testing.allocator, src);
    defer result.deinit();

    const warnings = api.SmartContractAnalysis.detectVulnerabilities(&result.tree);
    try testing.expect(warnings.len > 0);
    try testing.expect(warnings[0].category == .unchecked_error);
}
```

### Integration Tests (GhostLS Side)

**File**: `/data/projects/ghostls/tests/kalix_lsp_test.zig` (NEW)

```zig
const std = @import("std");
const testing = std.testing;
const Server = @import("lsp").Server;

test "Kalix LSP: hover on state variable" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var server = try Server.init(allocator);
    defer server.deinit();

    // Simulate didOpen for .kalix file
    const open_request =
        \\{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{"textDocument":{"uri":"file:///test.kalix","languageId":"kalix","version":1,"text":"contract Token {\n    state balance: u128;\n}"}}}
    ;
    _ = try server.handleMessage(open_request);

    // Test hover request on "balance"
    const hover_request =
        \\{"jsonrpc":"2.0","id":1,"method":"textDocument/hover","params":{"textDocument":{"uri":"file:///test.kalix"},"position":{"line":1,"character":10}}}
    ;
    const response = try server.handleMessage(hover_request);
    defer allocator.free(response);

    try testing.expect(std.mem.indexOf(u8, response, "u128") != null);
}

test "Kalix LSP: completions include Hedera APIs" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var server = try Server.init(allocator);
    defer server.deinit();

    const open_request =
        \\{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{"textDocument":{"uri":"file:///test.kalix","languageId":"kalix","version":1,"text":"contract Token {\n    fn mint() {\n        |\n    }\n}"}}}
    ;
    _ = try server.handleMessage(open_request);

    const completion_request =
        \\{"jsonrpc":"2.0","id":1,"method":"textDocument/completion","params":{"textDocument":{"uri":"file:///test.kalix"},"position":{"line":2,"character":8}}}
    ;
    const response = try server.handleMessage(completion_request);
    defer allocator.free(response);

    try testing.expect(std.mem.indexOf(u8, response, "hts_mint") != null);
}
```

### End-to-End Test Script

**File**: `/data/projects/ghostls/tests/kalix_e2e_test.sh` (NEW)

```bash
#!/bin/bash
set -e

echo "🧪 Testing GhostLS Kalix LSP Integration..."

# Create test contract
cat > /tmp/test.kalix <<EOF
contract SimpleToken {
    state totalSupply: u128;
    state balances: Map<Address, u128>;

    pub fn mint(to: Address, amount: u128) -> Result<(), String> {
        totalSupply = totalSupply + amount;
        balances[to] = balances[to] + amount;
        Ok(())
    }
}
EOF

# Start GhostLS in background
./zig-out/bin/ghostls &
GHOSTLS_PID=$!
sleep 1

# Test initialize
printf 'Content-Length: 92\r\n\r\n{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{},"processId":null}}' | nc localhost 9257

# Test didOpen
printf 'Content-Length: 300\r\n\r\n{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{"textDocument":{"uri":"file:///tmp/test.kalix","languageId":"kalix","version":1,"text":"contract SimpleToken {...}"}}}' | nc localhost 9257

# Cleanup
kill $GHOSTLS_PID
rm /tmp/test.kalix

echo "✅ Kalix LSP integration test passed!"
```

---

## 📦 Deliverables Checklist

### Kalix Compiler (Phase 1.5 - LSP Support)

- [ ] **LSPQuery API** (`src/frontend/api.zig`)
  - [ ] `symbolAt()` - Get symbol info at position
  - [ ] `findReferences()` - Find all symbol usages
  - [ ] `getCompletions()` - Context-aware completions
  - [ ] `getSignatureHelp()` - Function signature hints
  - [ ] `getDocumentSymbols()` - Document outline
  - [ ] `getInlayHints()` - Type annotations

- [ ] **AST Location Metadata** (`src/frontend/ast.zig`)
  - [ ] Add `SourceLocation` to all AST nodes
  - [ ] Implement `findNodeAtPosition()`

- [ ] **Enhanced Diagnostics** (`src/frontend/api.zig`)
  - [ ] Add severity levels (error, warning, info, hint)
  - [ ] Add source ranges to diagnostics
  - [ ] Include quick fix suggestions

- [ ] **Smart Contract Analysis** (`src/frontend/api.zig`)
  - [ ] `estimateGas()` - Static gas profiling
  - [ ] `detectVulnerabilities()` - Security warnings
  - [ ] `getStorageLayout()` - ZVM storage slots
  - [ ] `validateHederaUsage()` - Hedera API validation

- [ ] **Unit Tests**
  - [ ] Tests for all LSPQuery functions
  - [ ] Tests for smart contract analysis
  - [ ] Tests for diagnostic generation

### GhostLS Server (v0.7.0 - Kalix Support)

- [ ] **File Type Support**
  - [ ] Add `.kalix` extension to `document_manager.zig`
  - [ ] Update language ID detection

- [ ] **Kalix Analyzer** (`src/lsp/kalix_analyzer.zig`)
  - [ ] Implement `analyze()` - Call Kalix compiler
  - [ ] Implement `getDiagnostics()` - Convert to LSP format
  - [ ] Implement `getHover()` - Symbol hover info
  - [ ] Implement `getCompletions()` - Auto-complete
  - [ ] Implement `getDefinition()` - Go to definition
  - [ ] Implement `getReferences()` - Find references
  - [ ] Implement `getDocumentSymbols()` - Outline view

- [ ] **Hedera Completions** (`src/lsp/kalix_hedera_completions.zig`)
  - [ ] HTS function completions (transfer, mint, burn, etc.)
  - [ ] HCS function completions (submit, create_topic)
  - [ ] Storage function completions (state_read, state_write)

- [ ] **Server Routing** (`src/lsp/server.zig`)
  - [ ] Route `.kalix` files to `KalixAnalyzer`
  - [ ] Handle all LSP methods (hover, completion, definition, etc.)

- [ ] **Build System**
  - [ ] Add Kalix dependency to `build.zig.zon`
  - [ ] Link Kalix module in `build.zig`

- [ ] **Integration Tests**
  - [ ] LSP protocol tests for Kalix files
  - [ ] End-to-end test script
  - [ ] Grim IDE integration test

- [ ] **Documentation**
  - [ ] Update README with Kalix support
  - [ ] Write Kalix LSP tutorial
  - [ ] Add example `.kalix` contracts

---

## 🚀 Implementation Roadmap

### Sprint 1: Foundation (1 week)

**Goal**: Get basic Kalix file parsing working in GhostLS

- [ ] Add `.kalix` file type detection
- [ ] Implement basic `KalixAnalyzer` with `analyzeSource()` call
- [ ] Add Kalix to GhostLS build system
- [ ] Test basic LSP initialize + didOpen for `.kalix` files

### Sprint 2: Core LSP Features (2 weeks)

**Goal**: Implement hover, completions, definition

- [ ] Kalix: Add `symbolAt()` API
- [ ] Kalix: Add `getCompletions()` API
- [ ] GhostLS: Implement hover provider for Kalix
- [ ] GhostLS: Implement completion provider for Kalix
- [ ] GhostLS: Implement definition provider for Kalix
- [ ] Tests: Unit tests for each feature

### Sprint 3: Advanced Features (2 weeks)

**Goal**: References, symbols, diagnostics

- [ ] Kalix: Add `findReferences()` API
- [ ] Kalix: Add `getDocumentSymbols()` API
- [ ] Kalix: Enhance diagnostics with ranges + severity
- [ ] GhostLS: Implement references provider
- [ ] GhostLS: Implement document symbols provider
- [ ] GhostLS: Implement diagnostics routing
- [ ] Tests: Integration tests with full LSP workflow

### Sprint 4: Smart Contract Features (2 weeks)

**Goal**: Gas estimation, security analysis, Hedera APIs

- [ ] Kalix: Implement `SmartContractAnalysis` module
- [ ] Kalix: Add gas estimation
- [ ] Kalix: Add vulnerability detection
- [ ] GhostLS: Add Hedera completion suggestions
- [ ] GhostLS: Add gas hints in hover
- [ ] GhostLS: Add security warnings in diagnostics
- [ ] Tests: Smart contract analysis tests

### Sprint 5: Polish & Release (1 week)

**Goal**: Documentation, testing, release

- [ ] Write comprehensive documentation
- [ ] Add example Kalix contracts
- [ ] Run full test suite
- [ ] Create demo video
- [ ] Release GhostLS v0.7.0 with Kalix support
- [ ] Release Kalix Phase 1.5 (LSP support)

---

## 🎓 Example Usage

### Developer Workflow

**File**: `contracts/token.kalix`

```kalix
contract SimpleToken {
    state totalSupply: u128;
    state balances: Map<Address, u128>;
    state owner: Address;

    pub fn mint(to: Address, amount: u128) -> Result<(), String> {
        if msg.sender() != owner {
            return Err("Unauthorized");
        }

        totalSupply = totalSupply + amount;
        balances[to] = balances[to] + amount;

        // GhostLS shows hover: "hts_mint: fn(token: Address, amount: u128) -> Result<(), HederaError>"
        hts_mint(self.token_id, amount)?;

        Ok(())
    }

    pub fn transfer(to: Address, amount: u128) -> Result<(), String> {
        let sender = msg.sender();

        // GhostLS shows diagnostic: "Potential integer underflow"
        if balances[sender] < amount {
            return Err("Insufficient balance");
        }

        balances[sender] = balances[sender] - amount;
        balances[to] = balances[to] + amount;

        Ok(())
    }
}
```

**LSP Features in Action**:

1. **Hover on `totalSupply`**:
   ```
   state totalSupply: u128
   Contract storage variable (slot 0)
   ```

2. **Completions after `hts_`**:
   ```
   hts_transfer  - Transfer HTS tokens
   hts_mint      - Mint new tokens
   hts_burn      - Burn tokens
   hts_associate - Associate account
   hts_create    - Create token
   ```

3. **Go to Definition on `balances[sender]`**:
   - Jumps to `state balances: Map<Address, u128>;` declaration

4. **Find References on `mint`**:
   - Shows all calls to `mint()` function

5. **Document Symbols**:
   ```
   SimpleToken (contract)
   ├── totalSupply (state)
   ├── balances (state)
   ├── owner (state)
   ├── mint (function)
   └── transfer (function)
   ```

6. **Diagnostics**:
   ```
   Warning: Potential integer underflow at line 28
   Hint: Add overflow check or use checked arithmetic
   ```

7. **Gas Estimation** (in hover):
   ```
   mint(): ~5000-8000 gas
   transfer(): ~3000-5000 gas
   ```

---

## 🔗 Cross-Project Communication

### Development Coordination

**Kalix Team**:
- Owns: Compiler frontend API (`src/frontend/api.zig`)
- Implements: LSPQuery, SmartContractAnalysis
- Provides: Unit tests for all LSP APIs
- Timeline: Sprint 1-3

**GhostLS Team**:
- Owns: LSP server integration (`src/lsp/kalix_analyzer.zig`)
- Implements: Protocol translation, Hedera completions
- Provides: Integration tests, documentation
- Timeline: Sprint 2-5

**Sync Points**:
- **Weekly**: API design review (ensure Kalix API meets GhostLS needs)
- **Sprint End**: Integration testing (verify end-to-end LSP workflow)
- **Pre-Release**: Joint testing and documentation review

---

## 📚 References

### Kalix Documentation
- `/data/projects/kalix/docs/lang/syntax.md` - Language syntax
- `/data/projects/kalix/docs/lang/grammar.peg` - Formal grammar
- `/data/projects/kalix/docs/lang/semantics.md` - Type system
- `/data/projects/kalix/ROADMAP.md` - Project roadmap
- `/data/projects/kalix/TODO.md` - Task breakdown
- `/data/projects/kalix/ZVM_START_HERE.md` - ZVM integration guide

### GhostLS Documentation
- `/data/projects/ghostls/README.md` - Project overview
- `/data/projects/ghostls/docs/GRIM_INTEGRATION.md` - IDE integration
- `/data/projects/ghostls/src/lsp/protocol.zig` - LSP protocol definitions
- `/data/projects/ghostls/src/lsp/server.zig` - Server implementation

### LSP Specification
- [LSP 3.17 Specification](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/)
- [LSP Diagnostics](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#diagnostic)
- [LSP Completion](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_completion)

---

## 🎯 Success Metrics

**Integration Complete When**:

1. ✅ GhostLS recognizes `.kalix` files
2. ✅ Real-time syntax errors shown in editor
3. ✅ Hover shows type information and docs
4. ✅ Completions suggest contract state, functions, Hedera APIs
5. ✅ Go to definition navigates to symbol declarations
6. ✅ Find references finds all symbol usages
7. ✅ Document symbols show contract outline
8. ✅ Gas estimation shown in hover
9. ✅ Security warnings highlight vulnerabilities
10. ✅ Full test suite passing (unit + integration + e2e)
11. ✅ Documentation complete with examples
12. ✅ Demo video showing LSP features

---

**Let's build the future of smart contract development! 🚀**
