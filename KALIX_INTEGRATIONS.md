● 🎯 THE CRYSTAL-CLEAR GAMEPLAN: Kalix LSP via GhostLS + Grove

  Overview: The Perfect Architecture Stack

  You have three perfectly aligned projects that form a complete LSP stack:

  ┌─────────────────────────────────────────────────┐
  │  GhostLS (LSP Server)                           │
  │  • 15+ LSP features already implemented         │
  │  • Blockchain analyzer (security + gas)         │
  │  • Multi-language support (Ghostlang + GShell)  │
  │  • Location: /data/projects/ghostls             │
  └────────────────┬────────────────────────────────┘
                   │ uses
                   ▼
  ┌─────────────────────────────────────────────────┐
  │  Grove (Tree-sitter Wrapper)                    │
  │  • 15 bundled languages (Zig wrapper)           │
  │  • 8 LSP helper functions (50% boilerplate cut) │
  │  • Pre-generated parser.c files (no compilation)│
  │  • Location: /data/projects/grove               │
  └────────────────┬────────────────────────────────┘
                   │ needs
                   ▼
  ┌─────────────────────────────────────────────────┐
  │  tree-sitter-kalix (Grammar)                    │
  │  • PEG spec ready: grammar.peg                  │
  │  • 88 production rules defined                  │
  │  • TO BE CREATED                                │
  └─────────────────────────────────────────────────┘
                   │ drives
                   ▼
  ┌─────────────────────────────────────────────────┐
  │  Kalix Compiler (Semantic Analysis)             │
  │  • Frontend API: analyzeSource()                │
  │  • AST + diagnostics + metadata                 │
  │  • Gas accounting, type checking                │
  │  • Location: /data/projects/kalix               │
  └─────────────────────────────────────────────────┘

  ---
  The Gameplan: 5-Phase Rollout

  ✅ PHASE 0: What We Already Have

  Grove (v0.2.0):
  - ✅ 15 languages bundled (Ghostlang, Zig, Rust, Python, TypeScript, etc.)
  - ✅ 8 LSP helpers (findNodeAtPosition, extractSymbols, collectDiagnostics, etc.)
  - ✅ Clean API for adding new languages (3 files to modify)
  - ✅ Pre-generated parser.c approach (no npm/JS needed at runtime)

  GhostLS (v0.5.0):
  - ✅ Full LSP server with 15+ features
  - ✅ Blockchain analyzer (blockchain_analyzer.zig - 656 lines)
    - Reentrancy detection
    - Gas optimization hints
    - Access control checks
    - Integer overflow warnings
    - Cryptographic best practices
  - ✅ Multi-language document manager
  - ✅ LSP 3.17 compliant

  Kalix (Phase 1 complete):
  - ✅ Full frontend with lexer, parser, AST, semantic analyzer
  - ✅ Public API: kalix.frontend.analyzeSource() returns AST + diagnostics + metadata
  - ✅ Complete grammar specification in docs/lang/grammar.peg (88 rules)
  - ✅ Type system, ownership analysis, gas accounting in backend

  ---
  🚀 PHASE 1: Create tree-sitter-kalix Grammar (1-2 weeks)

  Goal: Generate parser.c for Kalix language

  Why This is Easy:
  - ✅ Complete PEG grammar already exists (/data/projects/kalix/docs/lang/grammar.peg)
  - ✅ Grove shows pattern: 15 existing grammars as reference
  - ✅ Tree-sitter CLI does the heavy lifting

  Steps:

  1.1: Create Grammar Repository

  cd /data/projects
  mkdir tree-sitter-kalix
  cd tree-sitter-kalix

  npm init -y
  npm install --save-dev tree-sitter-cli

  1.2: Write grammar.js (JavaScript)

  File: tree-sitter-kalix/grammar.js

  Convert PEG spec to tree-sitter DSL:

  module.exports = grammar({
    name: 'kalix',

    extras: $ => [
      /\s/,           // whitespace
      $.comment,      // comments
    ],

    rules: {
      source_file: $ => repeat($._declaration),

      _declaration: $ => choice(
        $.contract_declaration,
        $.import_declaration,
      ),

      contract_declaration: $ => seq(
        'contract',
        field('name', $.identifier),
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
        field('type', $._type),
        ';'
      ),

      table_declaration: $ => seq(
        optional('pub'),
        'table',
        field('name', $.identifier),
        ':',
        field('type', $.map_type),
        ';'
      ),

      event_declaration: $ => seq(
        optional('pub'),
        'event',
        field('name', $.identifier),
        '(',
        optional($.parameter_list),
        ')',
        ';'
      ),

      function_declaration: $ => seq(
        optional('pub'),
        'fn',
        field('name', $.identifier),
        '(',
        optional($.parameter_list),
        ')',
        repeat($._function_modifier),
        field('body', $.block)
      ),

      _function_modifier: $ => choice(
        'view',
        'payable',
      ),

      block: $ => seq('{', repeat($._statement), '}'),

      _statement: $ => choice(
        $.let_statement,
        $.const_statement,
        $.return_statement,
        $.if_statement,
        $.expression_statement,
      ),

      let_statement: $ => seq(
        'let',
        optional('mut'),
        field('name', $.identifier),
        optional(seq(':', field('type', $._type))),
        '=',
        field('value', $._expression),
        ';'
      ),

      return_statement: $ => seq('return', optional($._expression), ';'),

      _expression: $ => choice(
        $.identifier,
        $.number_literal,
        $.string_literal,
        $.binary_expression,
        $.call_expression,
        $.member_expression,
        $.assignment_expression,
      ),

      binary_expression: $ => prec.left(choice(
        ['+', 1],
        ['-', 1],
        ['*', 2],
        ['/', 2],
        ['==', 0],
        ['!=', 0],
      ), seq(
        field('left', $._expression),
        field('operator', $.binary_operator),
        field('right', $._expression)
      )),

      member_expression: $ => seq(
        field('object', $._expression),
        '.',
        field('property', $.identifier)
      ),

      _type: $ => choice(
        $.primitive_type,
        $.map_type,
        $.identifier,  // custom types
      ),

      primitive_type: $ => choice(
        'u8', 'u16', 'u32', 'u64', 'u128',
        'i8', 'i16', 'i32', 'i64', 'i128',
        'bool', 'string', 'bytes',
        'Address', 'Hash'
      ),

      map_type: $ => seq(
        'Map',
        '<',
        field('key', $._type),
        ',',
        field('value', $._type),
        '>'
      ),

      identifier: $ => /[a-zA-Z_][a-zA-Z0-9_]*/,

      number_literal: $ => /\d+/,

      string_literal: $ => /"[^"]*"/,

      comment: $ => token(choice(
        seq('//', /.*/),
        seq('/*', /[^*]*\*+([^/*][^*]*\*+)*/, '/')
      )),
    }
  });

  Note: This is simplified. Full implementation needs all 88 rules from grammar.peg.

  1.3: Generate Parser

  cd tree-sitter-kalix
  npx tree-sitter generate

  Output: src/parser.c (the file Grove needs!)

  1.4: Write Highlight Queries

  File: tree-sitter-kalix/queries/highlights.scm

  ; Keywords
  ["contract" "state" "table" "event" "fn" "let" "const" "mut" "pub" "return" "if" "else" "view" "payable"]
  @keyword

  ; Types
  (primitive_type) @type.builtin
  ["Address" "Hash"] @type.builtin
  (identifier) @type

  ; Functions
  (function_declaration name: (identifier) @function)
  (call_expression function: (identifier) @function.call)

  ; Variables
  (state_declaration name: (identifier) @variable.state)
  (table_declaration name: (identifier) @variable.table)
  (event_declaration name: (identifier) @constant.event)
  (let_statement name: (identifier) @variable)

  ; Literals
  (number_literal) @number
  (string_literal) @string
  (comment) @comment

  ; Operators
  ["+", "-", "*", "/", "==", "!=", "="] @operator

  ; Punctuation
  ["(", ")", "{", "}", "[", "]", "<", ">", ";", ":", ",", "."] @punctuation

  1.5: Write Locals Query

  File: tree-sitter-kalix/queries/locals.scm

  ; Scopes
  (source_file) @local.scope
  (contract_declaration) @local.scope
  (function_declaration) @local.scope
  (block) @local.scope

  ; Definitions
  (contract_declaration name: (identifier) @local.definition.contract)
  (state_declaration name: (identifier) @local.definition.state)
  (table_declaration name: (identifier) @local.definition.table)
  (event_declaration name: (identifier) @local.definition.event)
  (function_declaration name: (identifier) @local.definition.function)
  (let_statement name: (identifier) @local.definition.variable)
  (const_statement name: (identifier) @local.definition.constant)

  ; References
  (identifier) @local.reference

  1.6: Test Grammar

  cd tree-sitter-kalix

  # Test parsing
  echo 'contract Token {
      pub state totalSupply: u64;
      pub fn mint(amount: u64) {
          state.totalSupply = state.totalSupply + amount;
      }
  }' > test.kalix

  npx tree-sitter parse test.kalix

  Expected: Clean AST output with no errors

  ---
  🔌 PHASE 2: Add Kalix to Grove (2-3 days)

  Goal: Make Kalix a bundled language in Grove

  Why This is Easy:
  - ✅ Pattern established by 15 existing languages
  - ✅ Only 4 files to modify
  - ✅ No external dependencies

  Steps:

  2.1: Copy Grammar to Grove

  cd /data/projects/grove

  # Copy parser
  mkdir -p vendor/grammars/kalix
  cp /data/projects/tree-sitter-kalix/src/parser.c vendor/grammars/kalix/

  # Copy queries
  mkdir -p vendor/grammars/kalix/queries
  cp /data/projects/tree-sitter-kalix/queries/*.scm vendor/grammars/kalix/queries/

  2.2: Update build.zig

  File: /data/projects/grove/build.zig

  // Around line 58 - Add source definition
  const kalix_grammar_source = b.path("vendor/grammars/kalix/parser.c");

  // Around line 105 - Add to module compilation
  mod.addCSourceFile(.{ .file = kalix_grammar_source, .flags = &.{"-std=c99"} });

  2.3: Update languages.zig

  File: /data/projects/grove/src/languages.zig

  // Line ~19 - Add extern function
  extern fn tree_sitter_kalix() callconv(.c) *const c.TSLanguage;

  // Line ~22 - Add to Bundled enum
  pub const Bundled = enum {
      json, zig, rust, ghostlang, typescript, tsx, bash,
      javascript, python, markdown, cmake, toml, yaml, c, gshell,
      kalix,  // ADD HERE

      // Line ~38 - Update raw()
      pub fn raw(self: Bundled) *const c.TSLanguage {
          return switch (self) {
              // ... existing
              .kalix => tree_sitter_kalix(),
          };
      }
  };

  // Line ~91 - Update ensureBundled()
  try self.register("kalix", try Bundled.kalix.get());

  2.4: Create Language Utilities

  File: /data/projects/grove/src/editor/kalix.zig

  const std = @import("std");
  const Features = @import("features.zig");
  const Query = @import("../core/query.zig").Query;
  const Languages = @import("../languages.zig").Bundled;
  const Node = @import("../core/node.zig").Node;

  const DocumentSymbol = Features.DocumentSymbol;
  const SymbolRule = Features.SymbolRule;
  const FoldingRange = Features.FoldingRange;

  const kalix_symbol_rules = [_]SymbolRule{
      .{ .symbol_capture = "local.definition.contract", .name_capture = "local.definition.contract", .kind = 
  .class },
      .{ .symbol_capture = "local.definition.function", .name_capture = "local.definition.function", .kind = 
  .function },
      .{ .symbol_capture = "local.definition.state", .name_capture = "local.definition.state", .kind = .field 
  },
      .{ .symbol_capture = "local.definition.table", .name_capture = "local.definition.table", .kind = .field 
  },
      .{ .symbol_capture = "local.definition.event", .name_capture = "local.definition.event", .kind = .event 
  },
  };

  pub const KalixUtilities = struct {
      allocator: std.mem.Allocator,
      locals_query: Query,

      pub fn init(allocator: std.mem.Allocator) !KalixUtilities {
          const language = try Languages.kalix.get();

          var locals_query = try Query.init(
              allocator,
              language,
              @embedFile("../../vendor/grammars/kalix/queries/locals.scm")
          );

          return .{
              .allocator = allocator,
              .locals_query = locals_query,
          };
      }

      pub fn deinit(self: *KalixUtilities) void {
          self.locals_query.deinit();
      }

      pub fn documentSymbols(
          self: *KalixUtilities,
          root: Node,
          source: []const u8,
      ) Features.SymbolError![]DocumentSymbol {
          return Features.collectDocumentSymbols(
              self.allocator,
              &self.locals_query,
              root,
              source,
              &kalix_symbol_rules,
          );
      }
  };

  2.5: Update all_languages.zig

  File: /data/projects/grove/src/editor/all_languages.zig

  // Add import
  const KalixUtilities = @import("kalix.zig").KalixUtilities;

  // Add to union
  pub const LanguageUtilities = union(enum) {
      // ... existing
      kalix: KalixUtilities,

      pub fn init(allocator: std.mem.Allocator, language: Languages) !LanguageUtilities {
          return switch (language) {
              // ... existing
              .kalix => .{ .kalix = try KalixUtilities.init(allocator) },
          };
      }

      pub fn deinit(self: *LanguageUtilities) void {
          switch (self.*) {
              // ... existing
              .kalix => |*utils| utils.deinit(),
          }
      }

      pub fn documentSymbols(/*...*/) {
          return switch (self.*) {
              // ... existing
              .kalix => |*utils| utils.documentSymbols(root, source),
          };
      }
  };

  2.6: Test Grove Integration

  cd /data/projects/grove

  # Build
  zig build

  # Run tests
  zig build test

  # Parse a kalix file
  zig build run -- /data/projects/kalix/examples/token.kalix

  ---
  🔗 PHASE 3: Integrate Kalix into GhostLS (1 week)

  Goal: Add .kalix file support to GhostLS with full LSP features

  3.1: Add Kalix as Dependency

  File: /data/projects/ghostls/build.zig.zon

  .dependencies = .{
      .grove = .{
          .url = "https://github.com/ghostkellz/grove/archive/main.tar.gz",
          .hash = "...",
      },
      .kalix = .{
          .url = "https://github.com/ghostkellz/kalix/archive/main.tar.gz",
          .hash = "...",
      },
  },

  File: /data/projects/ghostls/build.zig

  const kalix_dep = b.dependency("kalix", .{ .target = target, .optimize = optimize });
  const kalix_mod = kalix_dep.module("kalix");

  exe.root_module.addImport("kalix", kalix_mod);

  3.2: Update Document Manager

  File: /data/projects/ghostls/src/lsp/document_manager.zig

  // Around line 19
  pub const LanguageType = enum {
      Ghostlang,
      GShell,
      Kalix,  // ADD HERE
  };

  // Around line 280
  fn detectLanguageType(uri: []const u8) LanguageType {
      if (std.mem.endsWith(u8, uri, ".kalix")) return .Kalix;
      if (std.mem.endsWith(u8, uri, ".gsh") or
          std.mem.endsWith(u8, uri, ".gshrc") or
          std.mem.endsWith(u8, uri, ".gshrc.gza")) return .GShell;
      return .Ghostlang;
  }

  3.3: Create Kalix Diagnostics Provider

  File: /data/projects/ghostls/src/lsp/kalix_diagnostics.zig

  const std = @import("std");
  const protocol = @import("protocol.zig");
  const kalix = @import("kalix");
  const grove = @import("grove");

  pub fn getDiagnostics(
      allocator: std.mem.Allocator,
      source: []const u8,
  ) ![]protocol.Diagnostic {
      var diagnostics = std.ArrayList(protocol.Diagnostic).empty;

      // Use Grove for syntax errors
      var parser = try grove.Parser.init(allocator);
      defer parser.deinit();

      const language = try grove.Languages.kalix.get();
      try parser.setLanguage(language);

      var tree = try parser.parseUtf8(null, source);
      defer tree.deinit();

      const root = tree.rootNode() orelse return &[_]protocol.Diagnostic{};

      // Collect tree-sitter syntax errors
      var ts_diagnostics = try grove.LSP.collectDiagnostics(allocator, root, source);
      defer {
          for (ts_diagnostics.items) |*diag| diag.deinit(allocator);
          ts_diagnostics.deinit();
      }

      // Convert Grove diagnostics to LSP
      for (ts_diagnostics.items) |diag| {
          try diagnostics.append(allocator, .{
              .range = diag.range,
              .severity = .Error,
              .message = try allocator.dupe(u8, diag.message),
              .source = try allocator.dupe(u8, "kalix-syntax"),
          });
      }

      // Use Kalix frontend for semantic errors
      const analysis = kalix.frontend.analyzeSource(allocator, source) catch |err| {
          std.debug.print("Kalix analysis failed: {}\n", .{err});
          return try diagnostics.toOwnedSlice(allocator);
      };
      defer analysis.deinit(allocator);

      // Convert Kalix diagnostics to LSP
      for (analysis.diagnostics) |kdiag| {
          try diagnostics.append(allocator, .{
              .range = .{
                  .start = .{ .line = kdiag.line, .character = kdiag.column },
                  .end = .{ .line = kdiag.line, .character = kdiag.column + 1 },
              },
              .severity = .Error,
              .message = try allocator.dupe(u8, kdiag.message),
              .source = try allocator.dupe(u8, "kalix-semantic"),
          });
      }

      return try diagnostics.toOwnedSlice(allocator);
  }

  3.4: Create Kalix Hover Provider

  File: /data/projects/ghostls/src/lsp/kalix_hover_provider.zig

  const std = @import("std");
  const protocol = @import("protocol.zig");
  const kalix = @import("kalix");
  const grove = @import("grove");

  pub fn getHover(
      allocator: std.mem.Allocator,
      source: []const u8,
      position: protocol.Position,
  ) !?protocol.Hover {
      // Parse with Grove
      var parser = try grove.Parser.init(allocator);
      defer parser.deinit();

      const language = try grove.Languages.kalix.get();
      try parser.setLanguage(language);

      var tree = try parser.parseUtf8(null, source);
      defer tree.deinit();

      const root = tree.rootNode() orelse return null;

      // Find node at position
      const node = grove.LSP.findNodeAtPosition(root, .{
          .line = position.line,
          .character = position.character,
      }) orelse return null;

      // Get node text
      const start = node.startByte();
      const end = node.endByte();
      const text = source[start..end];

      // Use Kalix metadata for type info
      const analysis = kalix.frontend.analyzeSource(allocator, source) catch return null;
      defer analysis.deinit(allocator);

      // Search for symbol in metadata
      for (analysis.metadata.contracts) |contract| {
          // Check states
          for (contract.states) |state| {
              if (std.mem.eql(u8, state.name, text)) {
                  const markdown = try std.fmt.allocPrint(allocator,
                      "```kalix\nstate {s}: {s}\n```\n\n**Contract:** {s}",
                      .{ state.name, state.type_name, contract.name }
                  );

                  return .{
                      .contents = .{ .value = markdown },
                      .range = grove.LSP.nodeToRange(node),
                  };
              }
          }

          // Check functions
          for (contract.functions) |func| {
              if (std.mem.eql(u8, func.name, text)) {
                  const markdown = try std.fmt.allocPrint(allocator,
                      "```kalix\nfn {s}({s}) {s}\n```\n\n**Gas estimate:** ~{d} gas",
                      .{ func.name, func.params, func.modifiers, func.estimated_gas }
                  );

                  return .{
                      .contents = .{ .value = markdown },
                      .range = grove.LSP.nodeToRange(node),
                  };
              }
          }
      }

      return null;
  }

  3.5: Wire Up Server Handlers

  File: /data/projects/ghostls/src/lsp/server.zig

  const kalix_diagnostics = @import("kalix_diagnostics.zig");
  const kalix_hover = @import("kalix_hover_provider.zig");

  // In handleTextDocumentDidOpen/DidChange
  if (doc.language_type == .Kalix) {
      const diagnostics = try kalix_diagnostics.getDiagnostics(
          self.allocator,
          doc.content,
      );
      defer {
          for (diagnostics) |diag| {
              self.allocator.free(diag.message);
              self.allocator.free(diag.source);
          }
          self.allocator.free(diagnostics);
      }

      try self.sendDiagnostics(uri, diagnostics);
  }

  // In handleHover
  if (doc.language_type == .Kalix) {
      return try kalix_hover.getHover(
          self.allocator,
          doc.content,
          params.position,
      );
  }

  ---
  🧠 PHASE 4: Smart Contract Features (1 week)

  Goal: Leverage blockchain_analyzer.zig for Kalix-specific insights

  4.1: Adapt Blockchain Analyzer for Kalix

  File: /data/projects/ghostls/src/lsp/blockchain_analyzer.zig

  // Add Kalix-specific detection

  fn isExternalCall(self: *BlockchainAnalyzer, node: grove.Node, source: []const u8) bool {
      if (node.childByFieldName("function")) |func_node| {
          const func_text = self.getNodeText(func_node, source);
          
          // Kalix-specific external calls
          return std.mem.indexOf(u8, func_text, "web3.call") != null or
              std.mem.indexOf(u8, func_text, "web3.transfer") != null or
              std.mem.indexOf(u8, func_text, "HTS.transfer") != null or  // Hedera Token Service
              std.mem.indexOf(u8, func_text, "HCS.submit") != null;       // Hedera Consensus Service
      }
      return false;
  }

  // Add ZVM gas costs
  pub const ZVMGasCosts = struct {
      pub const SLOAD: u64 = 200;
      pub const SSTORE: u64 = 20000;
      pub const HTS_TRANSFER: u64 = 9000;
      pub const HCS_SUBMIT: u64 = 5000;
  };

  4.2: Add Kalix-Specific Checks

  // Detect missing access control in Kalix
  fn checkKalixAccessControl(self: *BlockchainAnalyzer, func_node: grove.Node, source: []const u8) !void {
      // Check for web3.require with owner checks
      if (!self.hasAccessControl(func_node, source)) {
          const range = self.nodeToRange(func_node);
          const message = try self.allocator.dupe(u8, 
              "Kalix function modifies state without access control. Add owner check with web3.require()."
          );
          
          try self.diagnostics.append(self.allocator, .{
              .range = range,
              .severity = .warning,
              .message = message,
              .code = try self.allocator.dupe(u8, "kalix-missing-access-control"),
          });
      }
  }

  ---
  🎨 PHASE 5: Editor Integration & Polish (3-5 days)

  5.1: Test with Grim IDE

  5.2: Test with Neovim

  5.3: Documentation

  5.4: Examples

  ---
  📊 Effort Estimation Summary

  | Phase | Task                     | Time      | Dependencies       |
  |-------|--------------------------|-----------|--------------------|
  | 1     | Create tree-sitter-kalix | 1-2 weeks | PEG grammar (done) |
  | 2     | Add Kalix to Grove       | 2-3 days  | Phase 1 complete   |
  | 3     | GhostLS integration      | 1 week    | Phase 2 complete   |
  | 4     | Smart contract features  | 1 week    | Phase 3 complete   |
  | 5     | Editor integration       | 3-5 days  | Phase 4 complete   |
  | Total | End-to-end               | 4-6 weeks |                    |

  ---
  🎯 Key Advantages of This Approach

  1. Maximum Code Reuse:
    - Grove's 8 LSP helpers eliminate ~50% of boilerplate
    - GhostLS's blockchain analyzer already has smart contract patterns
    - Kalix frontend API is ready-to-use
  2. Clean Separation of Concerns:
    - tree-sitter-kalix: Syntax only (grammar.js)
    - Grove: Tree-sitter wrapper + LSP helpers
    - GhostLS: LSP protocol + smart contract insights
    - Kalix frontend: Semantic analysis + type checking
  3. No Duplication:
    - One grammar (tree-sitter-kalix)
    - One LSP server (GhostLS with multi-language support)
    - One tree-sitter wrapper (Grove)
  4. Production Ready:
    - All components are mature and tested
    - 15+ LSP features out of the box
    - Blockchain security analysis included

  ---
  ✅ Next Immediate Action

  Start with Phase 1: Create the tree-sitter-kalix grammar.

  Would you like me to:
  1. Generate the complete grammar.js from the PEG spec?
  2. Set up the tree-sitter-kalix repository structure?
  3. Create the query files (highlights.scm, locals.scm)?

  This is the foundation everything else builds on!


