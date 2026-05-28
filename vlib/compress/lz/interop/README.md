## LZ Interop Validation (V, C, Python)

This tool validates:

- V compress/decompress roundtrips for all formats: `lz77`, `lz78`, `lzw`,
  `lz4`, `lzss`, `lzma`, `lzjb`
- a C `lz77`-like reference implementation
- a Python `lz77`-like reference implementation

The C/Python references are intentionally simple so the benchmark is easy to run
without external dependencies.

### Run

```bash
cd /home/jalon/git/v
./vnew run vlib/compress/lz/interop/lz_interop.v 2>&1
```

Optional args:

1. validation rounds (default: `40`)
2. input size in bytes (default: `524288`)

Example:

```bash
cd /home/jalon/git/v
./vnew run vlib/compress/lz/interop/lz_interop.v 25 262144 2>&1
```

### Output

The tool prints validation status lines only (no timing output).

When helpers are available, it also cross-validates compression/expansion
interoperability with V (`lz77`) in both directions:

- V compress -> C decompress
- C compress -> V decompress
- V compress -> Python decompress
- Python compress -> V decompress

If `cc`/`gcc` or `python3` are missing, that row is skipped with a message.
