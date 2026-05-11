#!/usr/bin/env python3
import sys
import time

MIN_MATCH = 3
MAX_LITERAL = 128
STREAM_MAGIC = b'VLZ1'
FORMAT_LZ77 = 0


def encode_uvarint(value: int) -> bytes:
    out = bytearray()
    v = value
    while v >= 0x80:
        out.append((v & 0x7F) | 0x80)
        v >>= 7
    out.append(v)
    return bytes(out)


def decode_uvarint(data: bytes, pos: int) -> tuple[int, int]:
    value = 0
    shift = 0
    i = pos
    while i < len(data) and shift <= 63:
        b = data[i]
        i += 1
        value |= (b & 0x7F) << shift
        if (b & 0x80) == 0:
            return value, i
        shift += 7
    raise ValueError('bad length varint')


def compress_lz77(data: bytes) -> bytes:
    out = bytearray()
    out.extend(STREAM_MAGIC)
    out.append(FORMAT_LZ77)
    out.extend(encode_uvarint(len(data)))
    i = 0
    while i < len(data):
        lit_len = min(MAX_LITERAL, len(data) - i)
        out.append(lit_len - 1)
        out.extend(data[i : i + lit_len])
        i += lit_len
    return bytes(out)


def decompress_lz77(data: bytes) -> bytes:
    if len(data) < 6 or data[:4] != STREAM_MAGIC:
        raise ValueError('bad magic')
    if data[4] != FORMAT_LZ77:
        raise ValueError('format mismatch')

    expected_len, pos = decode_uvarint(data, 5)
    out = bytearray()
    while pos < len(data):
        control = data[pos]
        pos += 1
        if (control & 0x80) == 0:
            literal_len = (control & 0x7F) + 1
            if pos + literal_len > len(data):
                raise ValueError('truncated literal')
            out.extend(data[pos : pos + literal_len])
            pos += literal_len
        else:
            length = (control & 0x7F) + MIN_MATCH
            off, pos = decode_uvarint(data, pos)
            if off == 0 or off > len(out):
                raise ValueError('bad offset')
            base = len(out) - off
            for k in range(length):
                out.append(out[base + k])
    if len(out) != expected_len:
        raise ValueError('length mismatch')
    return bytes(out)


def main() -> int:
    if len(sys.argv) < 2:
        print(
            f'usage:\n'
            f'  {sys.argv[0]} bench <input.bin> <iterations>\n'
            f'  {sys.argv[0]} compress <input.bin> <output.bin>\n'
            f'  {sys.argv[0]} decompress <input.bin> <output.bin>',
            file=sys.stderr,
        )
        return 1

    mode = sys.argv[1]
    if mode == 'bench':
        if len(sys.argv) < 4:
            print(f'usage: {sys.argv[0]} bench <input.bin> <iterations>', file=sys.stderr)
            return 1
        input_path = sys.argv[2]
        iterations = int(sys.argv[3])
        if iterations <= 0:
            print('iterations must be > 0', file=sys.stderr)
            return 1
        with open(input_path, 'rb') as f:
            data = f.read()
        start = time.perf_counter()
        for _ in range(iterations):
            enc = compress_lz77(data)
            dec = decompress_lz77(enc)
            if dec != data:
                print('roundtrip mismatch', file=sys.stderr)
                return 1
        elapsed_ms = int((time.perf_counter() - start) * 1000)
        print(f'ms={elapsed_ms}')
        return 0

    if mode == 'compress':
        if len(sys.argv) < 4:
            print(f'usage: {sys.argv[0]} compress <input.bin> <output.bin>', file=sys.stderr)
            return 1
        with open(sys.argv[2], 'rb') as f:
            data = f.read()
        out = compress_lz77(data)
        with open(sys.argv[3], 'wb') as f:
            f.write(out)
        return 0

    if mode == 'decompress':
        if len(sys.argv) < 4:
            print(f'usage: {sys.argv[0]} decompress <input.bin> <output.bin>', file=sys.stderr)
            return 1
        with open(sys.argv[2], 'rb') as f:
            data = f.read()
        out = decompress_lz77(data)
        with open(sys.argv[3], 'wb') as f:
            f.write(out)
        return 0

    print(f'unknown mode: {mode}', file=sys.stderr)
    return 1


if __name__ == '__main__':
    raise SystemExit(main())

