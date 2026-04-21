# bzip2 interop checker

This helper verifies interoperability between:

- V module: `x.compress.bzip2`
- System CLI: `bzip2`
- Python stdlib: `bz2`

It runs deterministic test vectors and compression levels, compresses each vector with all
three producers, then cross-decompresses every produced stream with all three decoders.

A case passes only if every decompressed output is byte-identical to the original input.

Current levels are `1`, `6`, and `9` (see `compression_levels` in
`vlib/x/compress/bzip2/interop/bzip2_interop_check.vsh`).

## Run

```bash
cd /home/jalon/git/v
v run vlib/x/compress/bzip2/interop/bzip2_interop_check.vsh
```

## Requirements

- `bzip2` available in `PATH`
- `python3` with `bz2`

