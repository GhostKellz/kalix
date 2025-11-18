/// Minimal subset of ZVM opcodes used by the Kalix backend.
pub const Opcode = enum(u8) {
    HALT = 0x00,

    PUSH1 = 0x11,
    PUSH2 = 0x12,
    PUSH4 = 0x13,
    PUSH8 = 0x14,
    PUSH16 = 0x15,
    PUSH32 = 0x16,

    ADD = 0x30,
    SUB = 0x31,
    MUL = 0x32,
    DIV = 0x33,
    MOD = 0x35,

    LT = 0x50,
    GT = 0x51,
    EQ = 0x54,
    AND = 0x56,
    XOR = 0x58,

    SLOAD = 0x90,
    SSTORE = 0x91,
    TLOAD = 0x92,
    TSTORE = 0x93,

    JUMP = 0xA0,
    JUMPI = 0xA1,
};
