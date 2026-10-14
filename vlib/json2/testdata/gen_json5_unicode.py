"""Generate private JSON5 category tables from Unicode 15.0.0 UnicodeData.txt.

Download https://www.unicode.org/Public/15.0.0/ucd/UnicodeData.txt, then run:
python3 vlib/json2/testdata/gen_json5_unicode.py UnicodeData.txt > vlib/json2/json5_unicode.v
./v fmt -w vlib/json2/json5_unicode.v
"""

from pathlib import Path
import sys


def read_categories(path):
    categories = {}
    first = None
    for line in path.read_text().splitlines():
        fields = line.split(";")
        code, name, category = int(fields[0], 16), fields[1], fields[2]
        if name.endswith(", First>"):
            first = code
        elif name.endswith(", Last>"):
            for point in range(first, code + 1):
                categories[point] = category
            first = None
        else:
            categories[code] = category
    return categories


def emit_ranges(name, categories, allowed):
    ranges = []
    for code, category in sorted(categories.items()):
        if category not in allowed:
            continue
        if ranges and ranges[-1][1] + 1 == code:
            ranges[-1][1] = code
        else:
            ranges.append([code, code])
    print(f"const {name} = [")
    for start, end in ranges:
        print(f"\t[0x{start:04x}, 0x{end:04x}]!,")
    print("]\n")


def print_oracle(name, categories, allowed, extras):
    count = 0
    checksum = 0xCBF29CE484222325
    for code in range(0x110000):
        bit = int(categories.get(code) in allowed or code in extras)
        count += bit
        checksum = ((checksum ^ bit) * 0x100000001B3) & ((1 << 64) - 1)
    print(f"{name}: count={count}, FNV-1a={checksum}", file=sys.stderr)


if __name__ == "__main__":
    categories = read_categories(Path(sys.argv[1]))
    print("// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.")
    print("// Use of this source code is governed by an MIT license found in the LICENSE file.")
    print("// Generated from Unicode 15.0.0, matching encoding.utf8's letter tables.")
    print("// Source: https://www.unicode.org/Public/15.0.0/ucd/UnicodeData.txt")
    print("// Regenerate with testdata/gen_json5_unicode.py; see its usage instructions.")
    print("module json2\n")
    print("// Nl completes ECMAScript UnicodeLetter alongside utf8.is_letter's L categories.")
    emit_ranges("json5_letter_number_ranges", categories, {"Nl"})
    print("// IdentifierPart adds Mn, Mc, Nd and Pc; No and Me are excluded.")
    emit_ranges("json5_identifier_part_ranges", categories, {"Mn", "Mc", "Nd", "Pc"})
    letters = {"Lu", "Ll", "Lt", "Lm", "Lo", "Nl"}
    print_oracle("IdentifierStart", categories, letters, {0x24, 0x5F})
    print_oracle("IdentifierPart", categories, letters | {"Mn", "Mc", "Nd", "Pc"},
                 {0x24, 0x5F, 0x200C, 0x200D})
