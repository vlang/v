"""Capture the SVG arc fixture from an independent native Skia build.

Install the pinned binding with `python3 -m pip install skia-python==144.1`,
then run this file. It is a fixture generator, not a canvas test dependency.
The Linux fixture was captured on x86_64 with glibc 2.41. Native libm rounding
can differ from the original macOS fixture; do not replace one with the other.
"""

import json
import platform
import struct

import skia


def f32_bits(value):
    return f"0x{struct.unpack('<I', struct.pack('<f', value))[0]:08x}"


assert skia.__version__ == "144.1", "use the pinned Skia reference build"
builder = skia.PathBuilder()
builder.moveTo(11.581105139828319, 50.0693373303035)
builder.arcTo(
    skia.Point(11.64792707484055, 4.2227373311956375),
    -99.92622184274818,
    skia.PathBuilder.kLarge_ArcSize,
    skia.PathDirection.kCCW,
    skia.Point(27.78401298036616, 4.989945955549562),
)
path = builder.detach()
points = []
for index in range(path.countPoints()):
    point = path.getPoint(index)
    points.extend([f32_bits(point.x()), f32_bits(point.y())])

weights = []
iterator = skia.Path.RawIter(path)
while True:
    verb, _ = iterator.next()
    if verb == skia.Path.kDone_Verb:
        break
    if verb == skia.Path.kConic_Verb:
        weights.append(f32_bits(iterator.conicWeight()))

print(json.dumps({
    "skia_python": skia.__version__,
    "system": platform.system(),
    "machine": platform.machine(),
    "libc": platform.libc_ver(),
    "points": points,
    "verbs": [int(verb) for verb in path.getVerbs()],
    "weights": weights,
}, indent=2))
