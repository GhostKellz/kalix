const std = @import("std");

pub const U256 = std.meta.Int(.unsigned, 256);

pub const Register = union(enum) {
    stack: u16,
    temp: u16,
    constant: U256,
};

pub const BinaryOp = struct {
    dest: Register,
    left: Register,
    right: Register,
};

pub const LocalLoad = struct {
    dest: Register,
    slot: u16,
};

pub const LocalStore = struct {
    slot: u16,
    value: Register,
};

pub const StateLoad = struct {
    dest: Register,
    slot: u64,
};

pub const StateStore = struct {
    slot: u64,
    value: Register,
};

pub const TableLoad = struct {
    dest: Register,
    slot: u64,
    key: Register,
};

pub const TableStore = struct {
    slot: u64,
    key: Register,
    value: Register,
};

pub const Label = struct {
    name: u32,
};

pub const Jump = struct {
    target: u32,
};

pub const ConditionalJump = struct {
    target: u32,
    condition: Register,
};

pub const Call = struct {
    function: []const u8,
    arguments: []const Register,
};

pub const Return = struct {
    value: ?Register,
};

pub const Constant = struct {
    value: Register,
};

pub const HTSTransfer = struct {
    token_id: Register,
    from: Register,
    to: Register,
    amount: Register,
};

pub const HTSMint = struct {
    token_id: Register,
    amount: Register,
};

pub const HCSSubmit = struct {
    topic: Register,
    offset: Register,
    length: Register,
};

pub const Revert = struct {
    code: Register,
};

pub const IR = union(enum) {
    add: BinaryOp,
    sub: BinaryOp,
    mul: BinaryOp,
    div: BinaryOp,
    mod_: BinaryOp,

    eq: BinaryOp,
    lt: BinaryOp,
    gt: BinaryOp,
    ne: BinaryOp,
    lte: BinaryOp,
    gte: BinaryOp,

    load_local: LocalLoad,
    store_local: LocalStore,
    load_state: StateLoad,
    store_state: StateStore,
    load_table: TableLoad,
    store_table: TableStore,

    label: Label,
    jump: Jump,
    jump_if: ConditionalJump,
    call: Call,
    ret: Return,

    push: Constant,
    pop: void,
    dup: u8,

    hts_transfer: HTSTransfer,
    hts_mint: HTSMint,
    hcs_submit: HCSSubmit,

    revert: Revert,
    halt: void,
};

pub const IRList = std.ArrayList(IR);
