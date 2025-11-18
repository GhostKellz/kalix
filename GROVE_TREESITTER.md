# Grove Tree-sitter Integration Guide for Kalix

**Integrating Kalix smart contract language support into Grove for LSP and editor tooling**

---

## Executive Summary

This document outlines how to integrate the **Kalix** smart contract language with **Grove**, a high-performance Tree-sitter wrapper for Zig. This integration will enable:

1. **Syntax highlighting** for `.kalix` files in editors (Grim, VSCode, etc.)
2. **LSP features** via GhostLS: hover, completion, diagnostics, go-to-definition
3. **Real-time parsing** with sub-5ms incremental updates
4. **Semantic analysis** integration for type-aware tooling

---

## Table of Contents

1. [Grove Architecture Overview](#grove-architecture-overview)
2. [What Kalix Needs to Provide](#what-kalix-needs-to-provide)
3. [Grove API Reference](#grove-api-reference)
4. [Integration Roadmap](#integration-roadmap)
5. [LSP Integration Pattern](#lsp-integration-pattern)
6. [Testing Strategy](#testing-strategy)

---

## Grove Architecture Overview

### What Grove Provides

Grove is a Zig wrapper around Tree-sitter that offers:

- **15 bundled grammars** (JSON, Zig, Rust, TypeScript, Python, etc.)
- **Zero-copy parsing** with incremental edit support
- **Query engine** for syntax highlighting, folding, symbols
- **LSP helpers** for position mapping, diagnostics, completions
- **Parser pooling** for multi-threaded performance
- **REPL/shell support** with sub-5ms highlighting

### Core Components

```
grove/
├── src/
│   ├── core/          # Parser, Tree, Node, Query, EditBuilder
│   ├── editor/        # Highlighting, features, language-specific
│   ├── lsp/           # LSP protocol helpers
│   ├── semantic/      # Semantic analysis (cursor, traversal)
│   └── languages.zig  # Grammar registry
└── vendor/
    └── grammars/      # Tree-sitter grammar C sources
```

### How Grove Works

1. **Parse**: `Parser.parseUtf8()` → `Tree` (AST)
2. **Query**: `Query.init()` + `.scm` files → captures
3. **Highlight**: `HighlightEngine` → styled spans
4. **LSP**: `LSP.helpers` → positions, symbols, diagnostics

---

## What Kalix Needs to Provide

### 1. Tree-sitter Grammar (Required)

Kalix must create a **Tree-sitter grammar** using the standard Tree-sitter grammar.js DSL.

**Location**: Create a new repo or directory:
```
tree-sitter-kalix/
├── grammar.js          # Grammar definition
├── src/
│   ├── parser.c        # Generated parser (commit to git)
│   └── scanner.c       # Optional external scanner (if needed)
├── queries/
│   ├── highlights.scm  # Syntax highlighting queries
│   ├── locals.scm      # Scope/variable tracking
│   └── injections.scm  # Embedded language support
└── test/
    └── corpus/         # Grammar tests
```

**Example grammar.js structure for Kalix**:

```javascript
module.exports = grammar({
  name: 'kalix',

  rules: {
    source_file: $ => repeat($._top_level_item),

    _top_level_item: $ => choice(
      $.contract_declaration,
      $.comment,
    ),

    contract_declaration: $ => seq(
      'contract',
      field('name', $.identifier),
      field('body', $.contract_body)
    ),

    contract_body: $ => seq(
      '{',
      repeat($._contract_item),
      '}'
    ),

    _contract_item: $ => choice(
      $.state_declaration,
      $.table_declaration,
      $.event_declaration,
      $.function_declaration,
    ),

    state_declaration: $ => seq(
      optional('pub'),
      'state',
      field('name', $.identifier),
      ':',
      field('type', $.type_expression),
      ';'
    ),

    table_declaration: $ => seq(
      optional('pub'),
      'table',
      field('name', $.identifier),
      ':',
      field('type', $.type_expression),
      ';'
    ),

    event_declaration: $ => seq(
      optional('pub'),
      'event',
      field('name', $.identifier),
      field('params', $.parameter_list),
      ';'
    ),

    function_declaration: $ => seq(
      optional('pub'),
      optional('view'),
      optional('payable'),
      'fn',
      field('name', $.identifier),
      field('params', $.parameter_list),
      optional(seq('->', field('return_type', $.type_expression))),
      field('body', $.block)
    ),

    parameter_list: $ => seq(
      '(',
      sepBy(',', $.parameter),
      ')'
    ),

    parameter: $ => seq(
      optional(choice('mut', 'const')),
      field('name', $.identifier),
      ':',
      field('type', $.type_expression)
    ),

    type_expression: $ => choice(
      $.primitive_type,
      $.generic_type,
      $.identifier
    ),

    primitive_type: $ => choice(
      'u8', 'u16', 'u32', 'u64', 'u128', 'u256',
      'i8', 'i16', 'i32', 'i64', 'i128',
      'bool', 'bytes', 'string', 'Address'
    ),

    generic_type: $ => seq(
      field('base', $.identifier),
      '<',
      field('args', sepBy(',', $.type_expression)),
      '>'
    ),

    block: $ => seq(
      '{',
      repeat($._statement),
      '}'
    ),

    _statement: $ => choice(
      $.let_statement,
      $.return_statement,
      $.expression_statement,
    ),

    let_statement: $ => seq(
      'let',
      optional(choice('mut', 'const')),
      field('name', $.identifier),
      '=',
      field('value', $._expression),
      ';'
    ),

    return_statement: $ => seq(
      'return',
      optional(field('value', $._expression)),
      ';'
    ),

    expression_statement: $ => seq($._expression, ';'),

    _expression: $ => choice(
      $.identifier,
      $.integer_literal,
      $.boolean_literal,
      $.string_literal,
      $.binary_expression,
      $.call_expression,
      $.member_expression,
      $.assignment_expression,
      $.parenthesized_expression,
    ),

    binary_expression: $ => choice(
      prec.left(1, seq($._expression, '+', $._expression)),
      prec.left(1, seq($._expression, '-', $._expression)),
      prec.left(2, seq($._expression, '*', $._expression)),
      prec.left(2, seq($._expression, '/', $._expression)),
      prec.left(0, seq($._expression, '==', $._expression)),
      prec.left(0, seq($._expression, '!=', $._expression)),
      prec.left(0, seq($._expression, '<', $._expression)),
      prec.left(0, seq($._expression, '>', $._expression)),
    ),

    call_expression: $ => seq(
      field('function', $._expression),
      field('arguments', $.argument_list)
    ),

    argument_list: $ => seq(
      '(',
      sepBy(',', $._expression),
      ')'
    ),

    member_expression: $ => seq(
      field('object', $._expression),
      '.',
      field('property', $.identifier)
    ),

    assignment_expression: $ => seq(
      field('left', $._expression),
      '=',
      field('right', $._expression)
    ),

    parenthesized_expression: $ => seq('(', $._expression, ')'),

    identifier: $ => /[a-zA-Z_][a-zA-Z0-9_]*/,
    integer_literal: $ => /[0-9]+/,
    boolean_literal: $ => choice('true', 'false'),
    string_literal: $ => /"([^"\\]|\\.)*"/,

    comment: $ => token(choice(
      seq('//', /.*/),
      seq('/*', /[^*]*\*+([^/*][^*]*\*+)*/, '/')
    )),
  }
});

function sepBy(sep, rule) {
  return optional(seq(rule, repeat(seq(sep, rule))));
}
```

### 2. Highlight Queries (Required)

**File**: `queries/highlights.scm`

This defines how Kalix syntax maps to editor highlight groups:

```scheme
; Keywords
[
  "contract"
  "state"
  "table"
  "event"
  "fn"
  "let"
  "return"
  "pub"
  "view"
  "payable"
  "mut"
  "const"
] @keyword

; Types
(primitive_type) @type.builtin
(type_expression (identifier) @type)

; Functions
(function_declaration
  name: (identifier) @function)

(call_expression
  function: (identifier) @function.call)

; Variables
(parameter name: (identifier) @variable.parameter)
(let_statement name: (identifier) @variable)

; State/Storage
(state_declaration name: (identifier) @property)
(table_declaration name: (identifier) @property)
(event_declaration name: (identifier) @type.enum)

; Literals
(integer_literal) @number
(boolean_literal) @boolean
(string_literal) @string

; Operators
[
  "+"
  "-"
  "*"
  "/"
  "=="
  "!="
  "<"
  ">"
  "="
] @operator

; Punctuation
["{" "}"] @punctuation.bracket
["(" ")"] @punctuation.bracket
["<" ">"] @punctuation.bracket
["," ";" ":"] @punctuation.delimiter

; Comments
(comment) @comment

; Contracts
(contract_declaration
  name: (identifier) @type.definition)
```

### 3. Locals Queries (Optional but Recommended)

**File**: `queries/locals.scm`

Defines scopes and variable bindings for smart completion:

```scheme
; Scopes
(contract_body) @local.scope
(block) @local.scope
(function_declaration) @local.scope

; Definitions
(function_declaration
  name: (identifier) @local.definition)

(parameter
  name: (identifier) @local.definition)

(let_statement
  name: (identifier) @local.definition)

(state_declaration
  name: (identifier) @local.definition)

; References
(identifier) @local.reference
```

### 4. Injections Queries (Optional)

**File**: `queries/injections.scm`

For embedding other languages (e.g., inline assembly, SQL):

```scheme
; Currently not needed for Kalix Phase 1
; Add later if you support inline ZVM bytecode or similar
```

### 5. Build the Grammar

```bash
cd tree-sitter-kalix
npm install
npx tree-sitter generate
npx tree-sitter test  # Run corpus tests
npx tree-sitter parse examples/token.kalix  # Test on real file
```

This generates `src/parser.c` (and optionally `src/scanner.c`).

### 6. Vendor into Grove

Once the grammar works, vendor it into Grove:

```bash
# In grove repository
mkdir -p vendor/tree-sitter-kalix
cp -r tree-sitter-kalix/src vendor/tree-sitter-kalix/
cp -r tree-sitter-kalix/queries vendor/tree-sitter-kalix/
```

Update `grove/build.zig`:

```zig
// Add kalix grammar source
const kalix_grammar_source = b.path("vendor/tree-sitter-kalix/src/parser.c");

// In mod.addCSourceFile section:
mod.addCSourceFile(.{ .file = kalix_grammar_source, .flags = &.{"-std=c99"} });
```

Add to `grove/src/languages.zig`:

```zig
extern fn tree_sitter_kalix() callconv(.c) *const c.TSLanguage;

pub const Bundled = enum {
    // ... existing languages
    kalix,

    pub fn raw(self: Bundled) *const c.TSLanguage {
        return switch (self) {
            // ... existing cases
            .kalix => tree_sitter_kalix(),
        };
    }
};
```

Create `grove/src/editor/kalix_lang.zig`:

```zig
const std = @import("std");
const grove = @import("../root.zig");

pub const highlights_scm = @embedFile("../../vendor/tree-sitter-kalix/queries/highlights.scm");

pub const KalixUtilities = struct {
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) KalixUtilities {
        return .{ .allocator = allocator };
    }

    pub fn getHighlights(self: KalixUtilities, root: grove.Node, source: []const u8) ![]grove.HighlightSpan {
        const language = try grove.Languages.kalix.get();
        var query = try grove.Query.init(self.allocator, language, highlights_scm);
        defer query.deinit();

        const rules = &[_]grove.HighlightRule{
            .{ .capture = "keyword", .class = "keyword" },
            .{ .capture = "type", .class = "type" },
            .{ .capture = "function", .class = "function" },
            .{ .capture = "variable", .class = "variable" },
            .{ .capture = "number", .class = "number" },
            .{ .capture = "string", .class = "string" },
            .{ .capture = "comment", .class = "comment" },
        };

        return try grove.Editor.getHighlights(self.allocator, &query, root, rules);
    }
};
```

---

## Grove API Reference

### Core Parsing API

#### Parser

```zig
// Create parser
var parser = try grove.Parser.init(allocator);
defer parser.deinit();

// Set language
const language = try grove.Languages.kalix.get();
try parser.setLanguage(language);

// Parse source
var tree = try parser.parseUtf8(null, source);
defer tree.deinit();

// Get root node
const root = tree.rootNode() orelse return error.EmptyTree;
```

#### Tree and Node Navigation

```zig
// Node properties
const kind = node.kind();              // "function_declaration"
const start_pos = node.startPosition(); // { .row = 5, .column = 4 }
const end_pos = node.endPosition();
const byte_range = node.byteRange();   // { .start = 100, .end = 250 }
const text = node.text(source);        // Extract source text

// Navigation
const parent = node.parent();
const child = node.child(0);
const child_count = node.childCount();
const named_child = node.namedChild(1); // Skip anonymous nodes
const field_child = node.childByFieldName("name");

// Tree walking
var cursor = try grove.TreeCursor.init(root);
defer cursor.deinit();
while (cursor.nextNode()) |node| {
    std.debug.print("{s}\n", .{node.kind()});
}
```

### Query API

```zig
// Create query from .scm file
const query_source = @embedFile("highlights.scm");
var query = try grove.Query.init(allocator, language, query_source);
defer query.deinit();

// Execute query
var cursor = try grove.QueryCursor.init();
defer cursor.deinit();
cursor.exec(&query, root);

// Iterate captures
while (cursor.nextCapture(&query)) |result| {
    const capture_name = result.capture.name;  // "keyword", "function", etc.
    const node = result.capture.node;
    const node_text = node.text(source);
}
```

### LSP Helpers

```zig
// Position conversion
const lsp_pos = grove.LSP.Position{ .line = 10, .character = 5 };
const grove_point = lsp_pos.toGrovePoint();

// Find node at cursor
const node = grove.LSP.findNodeAtPosition(root, lsp_pos);

// Convert node to LSP range
const range = grove.LSP.nodeToRange(node);

// Extract symbols (for outline)
const symbols = try grove.LSP.extractSymbols(allocator, root, source, language);
defer allocator.free(symbols);

// Collect diagnostics (syntax errors)
const diagnostics = try grove.LSP.collectDiagnostics(allocator, root, source);
defer allocator.free(diagnostics);
```

### Highlight Engine

```zig
var engine = try grove.Highlight.HighlightEngine.init(
    allocator,
    language,
    highlights_scm,
    rules
);
defer engine.deinit();

const spans = try engine.highlight(allocator, root, source);
defer allocator.free(spans);

// Each span: { .start, .end, .class }
```

### Incremental Parsing

```zig
// Parse initial document
var old_tree = try parser.parseUtf8(null, initial_source);

// User edits the document
var edit_builder = grove.EditBuilder.init();
try edit_builder.insertText(10, 5, "new code");  // line 10, col 5

const edit = edit_builder.finalize();
old_tree.edit(&edit);

// Reparse with previous tree for speed
var new_tree = try parser.parseUtf8(&old_tree, new_source);
old_tree.deinit();
```

### Semantic Analysis API

```zig
// Analyze position context
const analysis = try grove.Semantic.analyzePosition(
    allocator,
    root,
    line,
    column,
    language
);

// Returns: { .node, .context, .scope }
// context: .function_call, .identifier, .string_literal, etc.
```

### Parser Pool (Multi-threaded)

```zig
var pool = try grove.ParserPool.init(allocator, language, 4); // 4 parsers
defer pool.deinit();

// In worker thread:
var lease = try pool.acquire();
defer lease.release();

var tree = try lease.parserRef().parseUtf8(null, source);
defer tree.deinit();
```

---

## Integration Roadmap

### Phase 1: Grammar Development (Week 1-2)

**Kalix Team Tasks:**

1. Create `tree-sitter-kalix` repository
2. Write `grammar.js` covering all Kalix syntax (see example above)
3. Create `queries/highlights.scm` for syntax highlighting
4. Create `queries/locals.scm` for scope tracking
5. Write test corpus in `test/corpus/`
6. Generate and test: `npx tree-sitter generate && npx tree-sitter test`
7. Commit generated `src/parser.c` to git

**Deliverable**: Working Tree-sitter grammar with 100% test coverage

### Phase 2: Grove Integration (Week 3)

**Kalix Team Tasks:**

1. Vendor grammar into Grove: `vendor/tree-sitter-kalix/`
2. Update `build.zig` to compile Kalix parser
3. Add Kalix to `languages.zig` enum
4. Create `editor/kalix_lang.zig` with highlight helpers
5. Register in `editor/all_languages.zig`

**Testing:**

```zig
test "parse kalix contract" {
    var parser = try grove.Parser.init(testing.allocator);
    defer parser.deinit();

    const lang = try grove.Languages.kalix.get();
    try parser.setLanguage(lang);

    const source =
        \\contract Token {
        \\    state balance: u128;
        \\    fn mint(amount: u128) {
        \\        balance = balance + amount;
        \\    }
        \\}
    ;

    var tree = try parser.parseUtf8(null, source);
    defer tree.deinit();

    const root = tree.rootNode().?;
    try testing.expectEqualStrings("source_file", root.kind());
}
```

**Deliverable**: Kalix grammar compiles in Grove, basic parsing works

### Phase 3: LSP Integration with GhostLS (Week 4-5)

**GhostLS Tasks** (or Kalix team if building custom LSP):

1. Add Kalix language server using `grove.LSP.LanguageServer`
2. Implement LSP features using Grove helpers:
   - `textDocument/documentSymbol` → `extractSymbols()`
   - `textDocument/hover` → `findNodeAtPosition()` + semantic data
   - `textDocument/completion` → scope-aware suggestions
   - `textDocument/definition` → `findDefinition()`
   - `textDocument/foldingRange` → `extractFoldingRanges()`
   - `textDocument/semanticTokens` → `extractSemanticTokens()`

**Example LSP Server Skeleton**:

```zig
const grove = @import("grove");

pub const KalixLanguageServer = struct {
    grove_server: grove.LSP.LanguageServer,
    kalix_api: kalix.frontend.api,  // Your semantic analyzer

    pub fn init(allocator: std.mem.Allocator) !KalixLanguageServer {
        const language = try grove.Languages.kalix.get();
        return .{
            .grove_server = try grove.LSP.LanguageServer.init(allocator, language),
            .kalix_api = kalix.frontend.api.init(allocator),
        };
    }

    pub fn textDocumentHover(
        self: *KalixLanguageServer,
        params: lsp.HoverParams
    ) !?lsp.Hover {
        // Parse with Grove
        var tree = try self.grove_server.parseDocument(params.textDocument.text);
        defer tree.deinit();

        // Find node at cursor
        const node = grove.LSP.findNodeAtPosition(tree.rootNode().?, params.position);

        // Use Kalix semantic analyzer for type info
        var analysis = try self.kalix_api.analyzeSource(allocator, params.textDocument.text);
        defer analysis.deinit();

        // Combine Grove + Kalix data for rich hover
        if (node) |n| {
            const type_info = try self.kalix_api.getTypeAtPosition(...);
            return lsp.Hover{
                .contents = .{ .value = type_info.description },
                .range = grove.LSP.nodeToRange(n),
            };
        }

        return null;
    }

    pub fn textDocumentCompletion(
        self: *KalixLanguageServer,
        params: lsp.CompletionParams
    ) ![]lsp.CompletionItem {
        // Use Grove's generic completion + Kalix's symbol table
        const grove_completions = try self.grove_server.completion(
            params.textDocument.text,
            params.position
        );

        // Enhance with Kalix-specific context (HTS functions, etc.)
        var enhanced = std.ArrayList(lsp.CompletionItem).init(allocator);
        try enhanced.appendSlice(grove_completions);

        // Add Kalix stdlib completions
        if (in_function_body) {
            try enhanced.append(.{
                .label = "hts_transfer",
                .kind = .function,
                .detail = "fn(token: Address, from: Address, to: Address, amount: u128)",
            });
        }

        return enhanced.toOwnedSlice();
    }
};
```

**Deliverable**: Full LSP support for Kalix via GhostLS or custom server

### Phase 4: Semantic Integration (Week 6)

**Kalix Team Tasks:**

1. Export semantic analysis API from `src/frontend/api.zig`:
   ```zig
   pub const SymbolTable = struct {
       functions: std.StringHashMap(FunctionInfo),
       state_vars: std.StringHashMap(StateVarInfo),
       types: std.StringHashMap(TypeInfo),

       pub fn getTypeAtPosition(line: u32, col: u32) ?TypeInfo { ... }
       pub fn getFunctionSignature(name: []const u8) ?FunctionInfo { ... }
   };

   pub fn analyzeSourceForLSP(
       allocator: std.mem.Allocator,
       source: []const u8
   ) !SymbolTable {
       // Return queryable symbol table for LSP
   }
   ```

2. Integrate with Grove's `Semantic.SemanticAnalyzer` (optional advanced):
   ```zig
   // Grove can call Kalix analyzer for language-specific semantics
   pub fn registerKalixAnalyzer(analyzer: *grove.Semantic.SemanticAnalyzer) !void {
       analyzer.language_hooks.kalix = .{
           .resolve_type = kalixResolveType,
           .find_references = kalixFindReferences,
       };
   }
   ```

**Deliverable**: Type-aware hover, completions, diagnostics

### Phase 5: Editor Integration (Week 7-8)

**Tasks:**

1. Add Kalix to Grim editor grammar registry
2. VSCode extension using GhostLS/Kalix LSP
3. Tree-sitter playground demo
4. Documentation and examples

**Deliverable**: Working editor experience for Kalix developers

---

## LSP Integration Pattern

### Recommended Architecture

```
┌─────────────────────────────────────────┐
│   Editor (VSCode, Grim, Neovim)         │
└─────────────────┬───────────────────────┘
                  │ LSP Protocol
                  │
┌─────────────────▼───────────────────────┐
│   GhostLS / Kalix Language Server       │
│  ┌─────────────────────────────────┐   │
│  │  Grove LSP Helpers              │   │
│  │  - findNodeAtPosition()         │   │
│  │  - extractSymbols()             │   │
│  │  - collectDiagnostics()         │   │
│  └─────────────┬───────────────────┘   │
│                │                        │
│  ┌─────────────▼───────────────────┐   │
│  │  Grove Tree-sitter Wrapper      │   │
│  │  - Parser, Tree, Node           │   │
│  │  - Query, Cursor                │   │
│  │  - Incremental parsing          │   │
│  └─────────────┬───────────────────┘   │
│                │                        │
│  ┌─────────────▼───────────────────┐   │
│  │  Kalix Semantic Analyzer        │   │
│  │  - Type checking                │   │
│  │  - Symbol resolution            │   │
│  │  - Contract validation          │   │
│  └─────────────────────────────────┘   │
└─────────────────────────────────────────┘
```

### Key Integration Points

1. **Syntax Highlighting**: Grove Query Engine + `highlights.scm`
2. **Diagnostics**: Grove ERROR nodes + Kalix semantic errors
3. **Hover**: Grove position mapping + Kalix type inference
4. **Completion**: Grove scope extraction + Kalix symbol table
5. **Go-to-Definition**: Grove query + Kalix reference tracking
6. **Formatting**: Grove tree traversal + Kalix style rules

---

## Testing Strategy

### Grammar Testing

```bash
# In tree-sitter-kalix/
npx tree-sitter test

# Test specific file
npx tree-sitter parse examples/token.kalix --debug
```

### Grove Integration Testing

```zig
// grove/test/kalix_integration_test.zig
const std = @import("std");
const grove = @import("grove");
const testing = std.testing;

test "kalix: parse contract" {
    var parser = try grove.Parser.init(testing.allocator);
    defer parser.deinit();

    const lang = try grove.Languages.kalix.get();
    try parser.setLanguage(lang);

    const source = @embedFile("fixtures/simple_contract.kalix");
    var tree = try parser.parseUtf8(null, source);
    defer tree.deinit();

    const root = tree.rootNode().?;
    try testing.expect(root.childCount() > 0);
}

test "kalix: syntax highlighting" {
    const lang = try grove.Languages.kalix.get();
    const highlights = @embedFile("../vendor/tree-sitter-kalix/queries/highlights.scm");

    var query = try grove.Query.init(testing.allocator, lang, highlights);
    defer query.deinit();

    // Should compile without errors
    try testing.expect(query.captureCount() > 0);
}

test "kalix: LSP symbol extraction" {
    var parser = try grove.Parser.init(testing.allocator);
    defer parser.deinit();

    const lang = try grove.Languages.kalix.get();
    try parser.setLanguage(lang);

    const source =
        \\contract Token {
        \\    state balance: u128;
        \\    fn mint(amount: u128) {}
        \\}
    ;

    var tree = try parser.parseUtf8(null, source);
    defer tree.deinit();

    const symbols = try grove.LSP.extractSymbols(
        testing.allocator,
        tree.rootNode().?,
        source,
        lang
    );
    defer testing.allocator.free(symbols);

    try testing.expect(symbols.len > 0);
    // Should find: Token contract, balance state, mint function
}
```

### LSP Testing

```zig
test "kalix LSP: hover on function" {
    var server = try KalixLanguageServer.init(testing.allocator);
    defer server.deinit();

    const source =
        \\contract Token {
        \\    fn mint(amount: u128) {}
        \\}
    ;

    const hover = try server.textDocumentHover(.{
        .textDocument = .{ .text = source },
        .position = .{ .line = 1, .character = 7 }, // on "mint"
    });

    try testing.expect(hover != null);
    try testing.expect(std.mem.indexOf(u8, hover.?.contents.value, "mint") != null);
}
```

---

## Required Kalix Deliverables Summary

| Item | Description | Priority | Complexity |
|------|-------------|----------|------------|
| **grammar.js** | Tree-sitter grammar definition | **P0** | High |
| **src/parser.c** | Generated parser (from `tree-sitter generate`) | **P0** | Auto |
| **queries/highlights.scm** | Syntax highlighting queries | **P0** | Medium |
| **queries/locals.scm** | Scope/variable tracking | **P1** | Medium |
| **test/corpus/** | Grammar test cases | **P0** | Medium |
| **API: analyzeSourceForLSP()** | Export symbol table for LSP | **P1** | Medium |
| **API: getTypeAtPosition()** | Type inference for hover | **P1** | High |
| **LSP Server** | GhostLS integration or custom server | **P1** | High |

---

## Example: Complete Kalix Contract Highlighting

**Input** (`examples/token.kalix`):

```kalix
contract SimpleToken {
    state totalSupply: u128;
    state balances: Map<Address, u128>;

    pub fn mint(to: Address, amount: u128) -> Result<(), String> {
        totalSupply = totalSupply + amount;
        balances[to] = balances[to] + amount;
        Ok(())
    }

    pub fn transfer(to: Address, amount: u128) -> Result<(), String> {
        let sender = msg.sender();
        if balances[sender] < amount {
            return Err("Insufficient balance");
        }
        balances[sender] = balances[sender] - amount;
        balances[to] = balances[to] + amount;
        Ok(())
    }
}
```

**Grove Output** (after `HighlightEngine.highlight()`):

```json
[
  { "start": 0, "end": 8, "class": "keyword" },       // "contract"
  { "start": 9, "end": 20, "class": "type" },         // "SimpleToken"
  { "start": 26, "end": 31, "class": "keyword" },     // "state"
  { "start": 32, "end": 43, "class": "property" },    // "totalSupply"
  { "start": 45, "end": 49, "class": "type.builtin" },// "u128"
  // ... (full span list)
]
```

---

## Resources

### Grove Documentation

- **Main README**: `/data/projects/grove/README.md`
- **API Root**: `/data/projects/grove/src/root.zig`
- **LSP Module**: `/data/projects/grove/src/lsp.zig`
- **Examples**: `/data/projects/grove/examples/lsp_server.zig`

### Tree-sitter Documentation

- **Grammar Guide**: https://tree-sitter.github.io/tree-sitter/creating-parsers
- **Query Syntax**: https://tree-sitter.github.io/tree-sitter/using-parsers#pattern-matching-with-queries
- **Playground**: https://tree-sitter.github.io/tree-sitter/playground

### Reference Grammars

Study these for patterns:

- **Zig**: `grove/vendor/grammars/zig/`
- **Rust**: `grove/vendor/tree-sitter-rust/`
- **TypeScript**: `grove/vendor/grammars/typescript/`

### Kalix Documentation

- **Syntax Guide**: `/data/projects/kalix/docs/lang/syntax.md`
- **Grammar**: `/data/projects/kalix/docs/lang/grammar.peg`
- **Roadmap**: `/data/projects/kalix/ROADMAP.md`
- **ZVM Integration**: `/data/projects/kalix/ZVM_START_HERE.md`

---

## Next Steps

1. **Immediate** (Week 1): Create `tree-sitter-kalix` repo, write `grammar.js`
2. **Week 2**: Test grammar with corpus, refine until 100% passing
3. **Week 3**: Vendor into Grove, add to `languages.zig`
4. **Week 4-5**: Build LSP server using `grove.LSP` helpers
5. **Week 6**: Integrate Kalix semantic analyzer with Grove
6. **Week 7-8**: Editor plugins (Grim, VSCode), documentation

---

## Support Channels

- **Grove Issues**: https://github.com/ghostkellz/grove/issues
- **Tree-sitter Discussions**: https://github.com/tree-sitter/tree-sitter/discussions
- **Kalix Development**: `/data/projects/kalix/TODO.md`

---

**Questions or need clarification on any Grove API?** Refer to:

- `grove/src/root.zig` - Full public API surface
- `grove/src/lsp/helpers.zig` - LSP helper functions
- `grove/src/editor/features.zig` - Editor utilities
- `grove/examples/lsp_server.zig` - Working LSP example

**Ready to build the future of smart contract tooling!** 🚀
