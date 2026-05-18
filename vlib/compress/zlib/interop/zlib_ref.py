import sys, zlib
def rf(path):
    with open(path, "rb") as f:
        return f.read()
def wf(path, data):
    with open(path, "wb") as f:
        f.write(data)
if len(sys.argv) != 4:
    print("usage: zlib_ref.py compress|decompress in out", file=sys.stderr)
    sys.exit(2)
mode, inp, outp = sys.argv[1], sys.argv[2], sys.argv[3]
data = rf(inp)
if mode == "compress":
    wf(outp, zlib.compress(data))
elif mode == "decompress":
    wf(outp, zlib.decompress(data))
else:
    print("unknown mode", file=sys.stderr)
    sys.exit(2)
