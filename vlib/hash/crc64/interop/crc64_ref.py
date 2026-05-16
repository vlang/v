#!/usr/bin/env python3
"""CRC64-ECMA reference implementation in Python."""

import sys
import binascii

CRC64_ECMA = 0x42F0E1EBA9EA3693

class CRC64:
    def __init__(self):
        self.table = self._make_table(CRC64_ECMA)

    @staticmethod
    def _make_table(poly):
        table = []
        for i in range(256):
            crc = i
            for _ in range(8):
                if crc & 1:
                    crc = (crc >> 1) ^ poly
                else:
                    crc >>= 1
            table.append(crc & 0xFFFFFFFFFFFFFFFF)
        return table

    def checksum(self, data):
        """Compute CRC64 checksum of data."""
        crc = 0xFFFFFFFFFFFFFFFF
        for byte in data:
            crc = self.table[(crc ^ byte) & 0xFF] ^ (crc >> 8)
        return (~crc) & 0xFFFFFFFFFFFFFFFF


def main():
    crc = CRC64()

    if len(sys.argv) < 2:
        print("Usage: crc64_ref.py <action> [data...]", file=sys.stderr)
        print("  checksum <hexstring>  - compute CRC64 of hex data", file=sys.stderr)
        print("  table                 - print first 16 table entries", file=sys.stderr)
        sys.exit(1)

    action = sys.argv[1]

    if action == "table":
        print("CRC64_ECMA table (first 16):")
        for i in range(16):
            print(f"  [{i:2d}] = 0x{crc.table[i]:016x}")
        return 0

    if action == "checksum":
        hexstr = sys.argv[2] if len(sys.argv) > 2 else ""
        try:
            data = binascii.unhexlify(hexstr) if hexstr else b""
            result = crc.checksum(data)
            print(f"{result:016x}")
            return 0
        except (binascii.Error, ValueError) as e:
            print(f"Error: invalid hex input: {e}", file=sys.stderr)
            sys.exit(1)

    print(f"Error: unknown action '{action}'", file=sys.stderr)
    sys.exit(1)


if __name__ == "__main__":
    main()

