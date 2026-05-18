import sys, zlib

def rf(p):
    with open(p, "rb") as f: return f.read()

def wf(p, b):
    with open(p, "wb") as f: f.write(b)

if len(sys.argv) != 4:
    print("usage: py_ref compress|decompress|gzip|gunzip in out", file=sys.stderr)
    sys.exit(2)

mode, inp, outp = sys.argv[1], sys.argv[2], sys.argv[3]
data = rf(inp)
if mode == "compress":
    wf(outp, zlib.compress(data))
elif mode == "decompress":
    wf(outp, zlib.decompress(data))
elif mode == "gzip":
    co = zlib.compressobj(level=6, wbits=16 + zlib.MAX_WBITS)
    wf(outp, co.compress(data) + co.flush())
elif mode == "gunzip":
    wf(outp, zlib.decompress(data, 16 + zlib.MAX_WBITS))
else:
    print("unknown mode", file=sys.stderr)
    sys.exit(2)

