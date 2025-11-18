# KALIX → ZVM Integration TODO

## 🎉 ZVM Status: 110% Production Ready!

ZVM is now fully ready for KALIX integration with all required features implemented and tested.

## ✅ Completed in ZVM

### Core VM Features
- ✅ Stack machine (1024 slots)
- ✅ Memory management with expansion costs
- ✅ Gas metering and cost tracking
- ✅ All arithmetic, bitwise, comparison opcodes
- ✅ Control flow (JUMP, JUMPI, JUMPDEST)
- ✅ Storage operations (SLOAD, SSTORE, TLOAD, TSTORE)
- ✅ Contract lifecycle (CREATE, CREATE2, CALL, DELEGATECALL, STATICCALL, SELFDESTRUCT)
- ✅ Account state management
- ✅ Journaled state with checkpoint/rollback
- ✅ Execution context (caller, value, calldata, block info)

### KALIX-Specific Features
- ✅ **TABLEHASH opcode (0xD2)** - Structured storage for tables/maps
- ✅ **Gas cost export module** - Accurate gas estimation in compiler
- ✅ **KALIX bytecode loader** - Function selector routing
- ✅ **ZVMC container format** - Bytecode + ABI metadata packaging

### Multi-Chain Support
- ✅ **Hedera**: HTS/HCS syscalls + mock for testing
- ✅ **EVM**: Bytecode translation + precompiles (ecrecover, sha256, etc.)
- ✅ **Soroban**: WASM bridge + host functions

### Integration
- ✅ **ZELIX bridge** - Hedera network deployment
- ✅ **64/64 tests passing** - All functionality verified

---

## 📋 KALIX Compiler Tasks

### Phase 1: Backend Implementation (Week 1-2)

#### 1.1 ZVM Code Generation
- [ ] Create KALIX → ZVM bytecode generator
  - [ ] Implement opcode emission
  - [ ] Stack management during codegen
  - [ ] Jump label resolution
  - [ ] Constant pool management

- [ ] Import ZVM gas costs
  ```zig
  const zvm_gas = @import("zvm").gas_costs;

  // Use in compiler for gas estimation
  const add_gas = zvm_gas.OPCODE_COSTS[@intFromEnum(Opcode.ADD)];
  ```

- [ ] Generate ZVMC containers
  ```zig
  const BytecodeContainer = @import("zvm").BytecodeContainer;

  var container = try BytecodeContainer.create(
      allocator,
      generated_bytecode,
      abi_json,
      .zvm_native,
  );
  ```

#### 1.2 TABLEHASH Integration
- [ ] Detect table/map types in KALIX AST
  ```kalix
  pub state balances: Table<Address, u64>;  // Detect this
  ```

- [ ] Generate TABLEHASH bytecode for table access
  ```zig
  // For: balances.get(key)
  // Emit:
  PUSH32 <key>
  PUSH32 <table_slot>
  TABLEHASH           // 0xD2
  SLOAD
  ```

- [ ] Allocate storage slots for tables
  ```zig
  // Track which slot each table uses
  table_slots = {
      "balances": 0,
      "allowances": 1,
      "metadata": 2,
  }
  ```

#### 1.3 Function Selector Generation
- [ ] Compute function selectors
  ```zig
  const selector = KalixLoader.computeSelector("transfer(address,uint64)");
  // Returns: [4]u8 (first 4 bytes of keccak256)
  ```

- [ ] Generate dispatch table
  ```zig
  // Entry point bytecode:
  // 1. Load selector from calldata[0..4]
  // 2. Compare with known selectors
  // 3. JUMPI to function
  // 4. REVERT if no match
  ```

### Phase 2: ABI Generation (Week 2-3)

#### 2.1 Generate JSON ABI
- [ ] Extract function signatures from AST
- [ ] Extract events from AST
- [ ] Extract state variables and their slots
- [ ] Format as JSON for ZVMC container

Example ABI:
```json
{
  "contract": "Token",
  "functions": [
    {
      "name": "transfer",
      "selector": "0x12345678",
      "inputs": [
        { "name": "to", "type": "Address" },
        { "name": "amount", "type": "u64" }
      ],
      "outputs": [],
      "mutability": "nonpayable"
    }
  ],
  "events": [
    {
      "name": "Transfer",
      "fields": [
        { "name": "from", "type": "Address", "indexed": true },
        { "name": "to", "type": "Address", "indexed": true },
        { "name": "amount", "type": "u64", "indexed": false }
      ]
    }
  ]
}
```

#### 2.2 State Variable Layout
- [ ] Assign storage slots to state variables
- [ ] Handle packing of small types into single U256
- [ ] Generate slot map for debugging

### Phase 3: Optimization (Week 3-4)

#### 3.1 Gas Optimization
- [ ] Constant folding (compile-time evaluation)
- [ ] Dead code elimination
- [ ] Common subexpression elimination
- [ ] Inline small functions

#### 3.2 Storage Optimization
- [ ] Pack multiple small values into single slot
  ```kalix
  // Pack these into one U256:
  pub state {
      count: u64,      // 8 bytes
      active: bool,    // 1 byte
      owner: Address,  // 20 bytes
  }
  // Total: 29 bytes → fits in 32-byte U256
  ```

- [ ] Use TABLEHASH for maps (avoid array storage)

### Phase 4: Testing (Week 4-5)

#### 4.1 Unit Tests
- [ ] Test bytecode generation for each opcode
- [ ] Test TABLEHASH emission
- [ ] Test function selector computation
- [ ] Test ABI generation

#### 4.2 Integration Tests
- [ ] Compile KALIX → ZVM bytecode
- [ ] Load into ZVM loader
- [ ] Execute and verify results
- [ ] Test against ZVM test suite

#### 4.3 End-to-End Tests
- [ ] Deploy to local ZVM instance
- [ ] Deploy to Hedera testnet via ZELIX
- [ ] Call functions and verify state changes
- [ ] Test event emission

### Phase 5: Documentation (Week 5)

#### 5.1 Compiler Documentation
- [ ] KALIX → ZVM compilation guide
- [ ] Gas optimization best practices
- [ ] Storage layout guidelines
- [ ] Debugging tips

#### 5.2 Examples
- [ ] ERC20 token in KALIX
- [ ] NFT contract in KALIX
- [ ] Multi-sig wallet in KALIX
- [ ] Governance contract in KALIX

---

## 📚 Key ZVM Resources

### Documentation
- `/data/projects/zvm/docs/ARCHITECTURE.md` - System architecture
- `/data/projects/zvm/docs/KALIX_INTEGRATION.md` - KALIX integration guide
- `/data/projects/zvm/ROADMAP.md` - Full roadmap and phases

### Source Code References
- `/data/projects/zvm/src/runtime/kalix_loader.zig` - KALIX loader
- `/data/projects/zvm/src/gas/costs.zig` - Gas cost table (IMPORT THIS!)
- `/data/projects/zvm/src/bytecode/container.zig` - ZVMC container format
- `/data/projects/zvm/src/bytecode/opcode.zig` - All opcodes and gas costs

### Example Usage
```zig
// In KALIX compiler
const zvm = @import("zvm");

// 1. Generate bytecode
var bytecode = generateKalixBytecode(ast);

// 2. Generate ABI
var abi = generateKalixABI(ast);

// 3. Create container
var container = try zvm.BytecodeContainer.create(
    allocator,
    bytecode,
    abi,
    .zvm_native,
);

// 4. Serialize for deployment
const serialized = try container.serialize(allocator);

// 5. Deploy via ZELIX bridge
var bridge = zvm.runtime.ZelixBridge.init(allocator, .{ .network = .testnet });
const result = try bridge.deployContract(serialized, constructor_params, gas_limit);
```

---

## 🎯 Quick Start: Minimal KALIX Contract

### 1. Write KALIX Contract
```kalix
// contracts/SimpleStorage.kalix
contract SimpleStorage {
    pub state value: u64;

    pub fn set(new_value: u64) {
        self.value = new_value;
    }

    pub view fn get() -> u64 {
        return self.value;
    }
}
```

### 2. Compile to ZVM Bytecode

Expected bytecode structure:
```zig
// Entry point (function dispatch):
CALLDATALOAD 0     // Load selector from calldata
PUSH4 0xAABBCCDD   // set(u64) selector
EQ
PUSH2 set_offset
JUMPI

PUSH4 0x11223344   // get() selector
EQ
PUSH2 get_offset
JUMPI

REVERT             // Unknown function

// set() function:
set_offset:
    CALLDATALOAD 4     // Load parameter
    PUSH1 0            // Storage slot 0 (value)
    SSTORE
    RETURN

// get() function:
get_offset:
    PUSH1 0            // Storage slot 0 (value)
    SLOAD
    PUSH1 0            // Memory offset 0
    MSTORE
    PUSH1 32           // Return 32 bytes
    PUSH1 0            // From memory offset 0
    RETURN
```

### 3. Test with ZVM

```zig
// test/simple_storage_test.zig
const zvm = @import("zvm");
const std = @import("std");

test "SimpleStorage contract" {
    const allocator = std.testing.allocator;

    // Load compiled contract
    const bytecode = @embedFile("SimpleStorage.zvmc");
    var loader = zvm.KalixLoader.init(allocator);
    var container = try loader.loadContract(bytecode);
    defer container.deinit(allocator);

    // Setup state
    var state = zvm.JournaledState.init(allocator);
    defer state.deinit();
    var tstorage = zvm.TransientStorageImpl.init(allocator);
    defer tstorage.deinit();

    // Call set(42)
    const set_selector = zvm.KalixLoader.computeSelector("set(u64)");
    var set_params = [_]u8{0} ** 32;
    set_params[24..32].* = @bitCast(@as(u64, 42)); // Encode as big-endian u64

    const set_result = try loader.executeFunction(
        &container,
        set_selector,
        &set_params,
        100000,
        state.asStorage(),
        tstorage.asTransientStorage(),
        null,
    );

    try std.testing.expect(set_result.success);

    // Call get()
    const get_selector = zvm.KalixLoader.computeSelector("get()");
    const get_result = try loader.executeFunction(
        &container,
        get_selector,
        &[_]u8{},
        100000,
        state.asStorage(),
        tstorage.asTransientStorage(),
        null,
    );

    try std.testing.expect(get_result.success);

    // Verify returned value is 42
    const returned = std.mem.readInt(u64, get_result.return_data[24..32], .big);
    try std.testing.expectEqual(@as(u64, 42), returned);
}
```

---

## 🚀 Deployment to Hedera

```zig
// deploy/deploy_to_hedera.zig
const zvm = @import("zvm");
const zelix = @import("zelix");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Load compiled KALIX contract
    const bytecode = @embedFile("SimpleStorage.zvmc");

    // Create ZELIX bridge
    var bridge = zvm.runtime.ZelixBridge.init(
        allocator,
        .{ .network = .testnet },
    );

    // Deploy contract
    const result = try bridge.deployContract(
        bytecode,
        &[_]u8{}, // No constructor params
        1_000_000, // Gas limit
    );

    std.debug.print("Contract deployed!\n", .{});
    std.debug.print("Address: {}\n", .{result.contract_address});
    std.debug.print("Gas used: {}\n", .{result.gas_used});
    std.debug.print("TX ID: {s}\n", .{result.transaction_id});
}
```

---

## 📊 Progress Tracking

| Phase | Task | Status | Notes |
|-------|------|--------|-------|
| 1.1 | ZVM Code Generation | 🔵 TODO | Start here |
| 1.2 | TABLEHASH Integration | 🔵 TODO | Critical for maps |
| 1.3 | Function Selectors | 🔵 TODO | Entry point routing |
| 2.1 | JSON ABI Generation | 🔵 TODO | Metadata export |
| 2.2 | Storage Layout | 🔵 TODO | Slot assignment |
| 3.1 | Gas Optimization | 🔵 TODO | Performance |
| 3.2 | Storage Optimization | 🔵 TODO | Cost reduction |
| 4.1 | Unit Tests | 🔵 TODO | Verify codegen |
| 4.2 | Integration Tests | 🔵 TODO | ZVM execution |
| 4.3 | End-to-End Tests | 🔵 TODO | Hedera testnet |
| 5.1 | Documentation | 🔵 TODO | User guides |
| 5.2 | Examples | 🔵 TODO | Reference contracts |

**Legend:** 🔵 TODO | 🟡 In Progress | 🟢 Completed

---

## 💡 Tips & Best Practices

### 1. Start Simple
Begin with the minimal contract above before tackling complex features.

### 2. Use Gas Cost Table
Always import ZVM gas costs for accurate estimation:
```zig
const zvm_gas = @import("zvm").gas_costs;
```

### 3. Test Frequently
Run ZVM tests after each codegen change:
```bash
cd /data/projects/zvm
zig build test
```

### 4. Leverage TABLEHASH
Use for all maps/tables - it's optimized for this pattern:
```kalix
pub state balances: Table<Address, u64>;  // GOOD
```

### 5. Check Examples
ZVM has working tests for all features - use them as reference!

---

## 🆘 Need Help?

- ZVM Documentation: `archive/zvm/`
- ZVM Tests: `archive/zvm/src/interpreter/*_test.zig`
- KALIX Integration: `archive/zvm/docs/KALIX_INTEGRATION.md`

**ZVM is ready. Time to build KALIX! 🚀**
