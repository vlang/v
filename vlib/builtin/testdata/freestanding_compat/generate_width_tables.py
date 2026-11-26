"""Generate native code-point column widths from Unicode 16.0.0 data.

Usage: python3 generate_width_tables.py <ucd-directory>
Inputs: https://www.unicode.org/Public/16.0.0/ucd/{DerivedCoreProperties,
EastAsianWidth,HangulSyllableType}.txt and the bundled Unicode category table.
The generated data are covered by linux_bare/COPYRIGHT.unicode.
"""
from pathlib import Path
import hashlib
import re
import struct
import sys

root = Path(__file__).resolve().parents[4]
ucd = Path(sys.argv[1])
source = (root / 'vlib/unicode/tables.v').read_text()
assert "unicode_data_version = '16.0.0'" in source
zero = set()
wide = set()
# Nonspacing and enclosing marks consume no additional terminal column.
section = source.split('const cat_ranges = [', 1)[1].split(']', 1)[0]
for first, last, category in re.findall(
        r'CatRange\{lo: (0x[0-9a-f]+), hi: (0x[0-9a-f]+), cat: (\d+)\}', section):
    if int(category) in (5, 7):  # Mn, Me
        zero.update(range(int(first, 16), int(last, 16) + 1))
inputs = [('DerivedCoreProperties.txt', {'Default_Ignorable_Code_Point'}, zero),
          ('HangulSyllableType.txt', {'V', 'T'}, zero),
          ('EastAsianWidth.txt', {'W', 'F'}, wide)]
lines = ['// Generated Unicode 16.0.0 column-width data; COPYRIGHT.unicode.',
         '// Regenerate with testdata/freestanding_compat/generate_width_tables.py.',
         'module builtin', '', '$if linux && (amd64 || arm64) && !v_bare_no_startup ? {']
for filename, properties, points in inputs:
    data = (ucd / filename).read_bytes()
    assert '16.0.0' in data.decode().splitlines()[0]
    lines.append(f'\t// {filename} sha256: {hashlib.sha256(data).hexdigest()}')
    for line in data.decode().splitlines():
        fields = line.split('#', 1)[0].split(';')
        if len(fields) < 2 or fields[1].strip() not in properties:
            continue
        bounds = fields[0].strip().split('..')
        points.update(range(int(bounds[0], 16), int(bounds[-1], 16) + 1))
# A soft hyphen is a visible column. The leading Hangul filler represents
# a complete omitted consonant, unlike the zero-column medial/trailing jamo.
zero.discard(0x00ad)
zero.discard(0x115f)
wide.add(0x115f)


def packed_ranges(points):
    ranges = []
    for point in sorted(points):
        if ranges and point == ranges[-1][1] + 1:
            ranges[-1][1] = point
        else:
            ranges.append([point, point])
    return ''.join('\\x%02x' % byte for first, last in ranges
                   for byte in struct.pack('<II', first, last))


for name, points in [('zero', zero), ('double', wide)]:
    lines += ['', f"\tconst bare_width_{name}_ranges = '{packed_ranges(points)}'"]
lines += ['}']
(root / 'vlib/builtin/linux_bare/width_tables_d_freestanding.v').write_text('\n'.join(lines) + '\n')
