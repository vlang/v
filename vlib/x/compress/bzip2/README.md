# x.compress.bzip2

Pure V bzip2 encoder/decoder.

## Features

- Pure V implementation (no C wrappers)
- bzip2 stream compression (`BZh1`..`BZh9`)
- bzip2 stream decompression
- Block and stream CRC validation
- Deterministic output for identical input/params

## API

```v
import x.compress.bzip2

compressed := bzip2.compress('hello'.bytes())!
plain := bzip2.decompress(compressed)!
assert plain.bytestr() == 'hello'
```

With params:

```v
import x.compress.bzip2

data := 'hello'.bytes()
compressed := bzip2.compress(data, block_size: 1)!
plain := bzip2.decompress(compressed, verify_crc: true)!
assert plain.bytestr() == 'hello'
```

## Notes

- Randomized legacy bzip2 blocks are intentionally rejected.
- API currently works on full byte slices (no streaming interface yet).

## Test

```bash
v -silent test vlib/x/compress/bzip2/
```

## Proof

- tested against the official bzip2 C library using a variety of inputs and parameters.
- deterministic and produces identical output for identical input and parameters.
- includes CRC validation to ensure data integrity during compression and decompression.
- rejects randomized legacy bzip2 blocks, ensuring only valid bzip2 streams are processed.
- tested with a variety of inputs, including edge cases, to ensure robustness and reliability.
