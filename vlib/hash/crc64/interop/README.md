# CRC64 Interoperability Fixture

This directory contains interoperability tests for the V `hash.crc64` module, cross-validating
against reference implementations in C and Python.

## Overview

The fixture verifies that `hash.crc64` produces identical CRC-64-ECMA-182 checksums when:
- Computing the same data in V, C, and Python
- Operating on various input patterns (empty, binary, text, large payloads)

This helps catch:
- Bit-ordering errors
- Polynomial table generation bugs
- Byte-order issues
- Implementation divergences

## Files

- `crc64_interop.vsh` - Main V interop checker script
- `crc64_ref.c` - C reference implementation (auto-compiled)
- `crc64_ref.py` - Python reference implementation

## Running the fixture

From the V repository root:

```bash
v run vlib/hash/crc64/interop/crc64_interop.vsh
```

## Test vectors

The fixture tests 10 vector categories:

1. **empty** - Zero-length input
2. **single_a** - Single character 'a'
3. **single_null** - Single null byte
4. **text_123456789** - Standard test vector
5. **text_hello_world** - Common greeting
6. **all_zeros_16** - 16 zero bytes
7. **all_ones_16** - 16 0xFF bytes
8. **repeating_pattern** - 150 bytes of "abc" repeated
9. **all_bytes** - All 256 byte values in sequence
10. **large_payload** - 10000 bytes of "test data " repeated

Each vector is checksummed by:
- V: `hash.crc64.sum(data)`
- C: compiled from `crc64_ref.c`
- Python: `crc64_ref.py`

## Polynomial

All implementations use CRC-64-ECMA-182:
- Polynomial: `0x42F0E1EBA9EA3693`
- Initial value: `0x0000000000000000`
- Final XOR: `0x0000000000000000`
- Check value for `"123456789"`: `0x6c40df5f0b497347`

## Prerequisites

- V compiler (tested with current repo version)
- GCC (to compile C reference)
- Python 3 (for Python reference)

## Expected output

```
Compiling C reference helper...
Running cross-validation tests...
OK: empty => 0x0000000000000000
OK: single_a => 0x926a79e87a919f5d
OK: single_null => 0xbd4a6ec89c7eaafb
...
=== Results ===
Passed: 10
Failed: 0
Total:  10
```

## Troubleshooting

If the fixture fails:
1. Ensure `gcc` and `python3` are in PATH
2. Check that `hash.crc64` module can be imported (`v -silent test vlib/hash/crc64/`)
3. Manual check: `python3 crc64_ref.py checksum 313233343536373839` (should match C)
