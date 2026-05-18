# zlib interop checker
This helper verifies interoperability between:
- V module: `compress.zlib`
- C helper linked with `libz`
- Python stdlib: `zlib`
  It runs deterministic test vectors, compresses each vector with all three producers, then
  cross-decompresses every produced stream with all three consumers.
  A case passes only if every decompressed output is byte-identical to the original input.
## Run
```bash
./vnew run vlib/compress/zlib/interop/zlib_interop.vsh
```
## Requirements
- `python3` with the stdlib `zlib` module
- a C compiler (`cc`, `gcc`, or `clang`)
- `libz` development headers and linker support (`-lz`)
