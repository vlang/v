#!/bin/sh
# Check make's native program/test warmup with a fresh, private module cache.
set -eu
repo=$(CDPATH= cd "$(dirname "$0")/.." && pwd -P)
vexe=${VEXE:-"$repo/v"}
work=$(mktemp -d "${TMPDIR:-/tmp}/v-make-cache.XXXXXX")
trap 'rm -rf "$work"' EXIT
export V3CACHE="$work/cache" V3_CACHE_TRACE=1 V_MACOS_V3_NO_FALLBACK=1
export V3_DISABLE_UNCHANGED_BUILD_CACHE=1 V3_CACHE_DISABLE_PROGRAM_EXECUTABLE=1
unset V3_CACHE_FORCE_SOURCE V3_INTERNAL_CACHE_FORCE_SOURCE

# The requested warmup compiler overrides the production compiler flags, including
# VFLAGS inherited by make's child processes. Both native cache modes are warmed.
make -C "$repo" --no-print-directory cache-basic-modules VEXE="$vexe" \
    VFLAGS='-prod -cc unavailable-compiler' CACHE_VFLAGS='-cc cc' > "$work/warm.log" 2>&1 || {
    cat "$work/warm.log"
    exit 1
}
python3 - "$V3CACHE" "$work/objects.json" <<'PY'
import hashlib,json,pathlib,sys
objects=sorted(pathlib.Path(sys.argv[1]).rglob('builtin_*.o'))
assert objects, 'make did not create any builtin objects'
json.dump({str(p):[p.stat().st_mtime_ns,hashlib.sha256(p.read_bytes()).hexdigest()] for p in objects},open(sys.argv[2],'w'))
PY
cat > "$work/program.v" <<'V'
import strconv
import strings
fn main() {
    assert strconv.atoi('3')! == 3
    assert strings.repeat(120, 3) == 'xxx'
}
V
cat > "$work/consumer_test.v" <<'V'
import strconv
import strings
fn test_cached_modules() {
    assert strconv.atoi('3')! == 3
    assert strings.repeat(120, 3) == 'xxx'
}
V
export VFLAGS='-cc cc'
"$vexe" -showcc -o "$work/program" "$work/program.v" > "$work/program.log" 2>&1 || {
    cat "$work/program.log"
    exit 1
}
"$work/program"
"$vexe" -showcc "$work/consumer_test.v" > "$work/test.log" 2>&1 || {
    cat "$work/test.log"
    exit 1
}
python3 - "$work" <<'PY'
import hashlib,json,pathlib,sys
work=pathlib.Path(sys.argv[1])
for name in ['program','test']:
    log=(work/(name+'.log')).read_text()
    assert 'module cache miss: module=builtin' not in log, log
    assert 'module cache object miss: module=builtin' not in log, log
objects=json.loads((work/'objects.json').read_text())
for name,before in objects.items():
    path=pathlib.Path(name)
    assert [path.stat().st_mtime_ns,hashlib.sha256(path.read_bytes()).hexdigest()]==before, name
assert set(objects)=={str(p) for p in (work/'cache').rglob('builtin_*.o')}, 'consumer rebuilt builtin'
print('make program/test builtin cache reuse passed')
PY
