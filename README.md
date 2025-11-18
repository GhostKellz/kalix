# kalix

Kalix is a Zig-based smart contract compiler that targets the Ghostkell ZVM. The
`kalix` executable consumes Kalix source files and produces `.zvmc` bytecode
containers bundling code, ABI metadata, and gas accounting in the format
expected by the upstream `ghostkellz/zvm` runtime.

## Building

```
zig build
```

## Compiling a Contract

```
zig build run -- contracts/Treasury.kalix
```

The command above writes one `.zvmc` artifact per contract next to the source
file. Specify an explicit output directory to collect the containers elsewhere:

```
zig build run -- contracts/Treasury.kalix out/
```

Run the unit test suite with:

```
zig build test
```
