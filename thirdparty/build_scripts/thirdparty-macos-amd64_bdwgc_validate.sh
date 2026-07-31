#!/usr/bin/env bash
# Usage: thirdparty-macos-amd64_bdwgc_validate.sh <TCC_FOLDER>
#
# Validates a staged tcc.exe + libgc.{a,dylib} pair in place, using
# thirdparty/tccbin_tests/run.sh (present in the same vlang/v checkout
# this script lives in). Ports the hardened checks already proven in
# vlang/tccbin's thirdparty-macos-amd64 build-and-test.yml (PR #74),
# with two deliberate departures called out below rather than copied
# blind:
#
#   - The GC-linked flag set here also includes -DGC_THREADS=1 and
#     -DTHREAD_LOCAL_ALLOC=1, which tccbin's own existing CI omits but
#     vlib/builtin/builtin_d_gcboehm.c.v applies unconditionally
#     (lines 3-21) before ever reaching the macOS-specific block. A
#     validation that doesn't pass these flags isn't actually testing
#     what V will use.
#   - The static-lane summary matcher is adapted, not copied verbatim:
#     tccbin's pinned run.sh (checked out at a fixed vlang/v commit
#     that predates vlang/v#27935's stderr-capture fix) only ever
#     prints a bare "FAIL name (compile error)"; the current run.sh in
#     this checkout prints "FAIL name (compile error: <detail>)". The
#     direct per-test symbol/error-text checks (is_known_gc_init) stay
#     the authority either way; only the loose aggregate-shape match
#     was adjusted to tolerate the added detail.
#
# Exits 0 only if the dylib lane, the no-GC fallback lane, and the
# static lane's signature match (or the static lane unexpectedly fully
# passes - also fine, just logged) all hold. Any other shape exits 1.
set -euo pipefail

TCC_FOLDER="${1:?usage: $0 <TCC_FOLDER>}"

# TCC_FOLDER is passed both as a relative path (the in-tree build
# directory, "thirdparty/tcc") and as an already-absolute path (the
# fresh-checkout clone under /tmp) by the two call sites in
# update_tccbin.yml - blindly prefixing $PWD onto it (as an earlier
# version of this script did) is only correct for the relative case;
# for the absolute case "$PWD/$TCC_FOLDER" collapses the leading slash
# into "$PWD/tmp/..." instead of "/tmp/...", pointing the dylib/rpath
# args below at a path that doesn't exist.
case "$TCC_FOLDER" in
  /*) TCC_FOLDER_ABS="$TCC_FOLDER" ;;
  *) TCC_FOLDER_ABS="$PWD/$TCC_FOLDER" ;;
esac

echo "== independently re-verifying the dylib itself (build script's own check is not trusted blindly) =="
if [ ! -L "$TCC_FOLDER/lib/libgc.dylib" ]; then
  echo "::error::$TCC_FOLDER/lib/libgc.dylib is not a symlink - expected a symlink to a versioned file (e.g. libgc.1.dylib)."
  exit 1
fi
libgc_dylib_target=$(readlink "$TCC_FOLDER/lib/libgc.dylib")
if [ ! -f "$TCC_FOLDER/lib/$libgc_dylib_target" ]; then
  echo "::error::$TCC_FOLDER/lib/libgc.dylib points at '$libgc_dylib_target', which does not exist alongside it."
  exit 1
fi
libgc_install_name=$(otool -D "$TCC_FOLDER/lib/$libgc_dylib_target" | tail -n 1)
case "$libgc_install_name" in
  @rpath/*) echo "libgc.dylib -> $libgc_dylib_target, install name $libgc_install_name (both confirmed)" ;;
  *) echo "::error::install name of $libgc_dylib_target is '$libgc_install_name', not @rpath/-relative."; exit 1 ;;
esac

echo "== validating the static libgc.a archive directly (ported verbatim from vlang/tccbin PR #74) =="
# If the rebuild produced a structurally valid but empty or incomplete
# libgc.a (a real, different regression from a broken bdwgc build),
# the static lane's probes below would still show only "unresolved
# reference to '_GC_init'" text - the exact same shape as tcc's own
# known archive-parsing limitation. Validate the archive directly via
# ar/nm before treating any link failure against it as the known,
# harmless tcc limitation.
if ! ar t "$TCC_FOLDER/lib/libgc.a" >/dev/null 2>&1; then
  echo "::error::$TCC_FOLDER/lib/libgc.a is not a well-formed archive (ar t failed) - a real regression in the bdwgc build, not the known tcc archive-parsing limitation."
  exit 1
fi
# `grep -q` stops reading as soon as it finds a match, which can make
# `nm` receive SIGPIPE while still writing the rest of a large
# archive's symbol list - under `set -e -o pipefail`, that SIGPIPE-
# killed `nm` makes the whole pipeline report failure even though grep
# itself found the match. Capture the full output first, grep the
# captured text without -q.
rebuilt_gc_symbols=$(nm -g "$TCC_FOLDER/lib/libgc.a" 2>&1) || true
rebuilt_gc_init_defined=$(printf '%s\n' "$rebuilt_gc_symbols" | grep -E '(^| )[Tt] _?GC_init$') || true
if [ -z "$rebuilt_gc_init_defined" ]; then
  echo "::error::$TCC_FOLDER/lib/libgc.a does not define GC_init - a real regression (missing/incomplete symbols), not the known tcc archive-parsing limitation."
  exit 1
fi

# Full flag set V actually uses (builtin_d_gcboehm.c.v lines 3-21, 74-90
# combined) - not just the subset tccbin's own CI currently tests.
GC_CFLAGS=(-DGC_THREADS=1 -DTHREAD_LOCAL_ALLOC=1 -DGC_BUILTIN_ATOMIC=1 -DMPROTECT_VDB=1)

echo "== dylib lane (blocking) =="
# run.sh/libgc/include are resolved relative to the vlang/v checkout
# root (this script is invoked with cwd at that root, matching every
# other build_scripts/*.sh convention), not relative to $TCC_FOLDER.
run_sh="thirdparty/tccbin_tests/run.sh"
chmod +x "$run_sh"
"$run_sh" "$TCC_FOLDER/tcc.exe" macos -- \
    "${GC_CFLAGS[@]}" \
    -I thirdparty/libgc/include \
    "$TCC_FOLDER_ABS/lib/libgc.dylib" \
    -Wl,-rpath,"$TCC_FOLDER_ABS/lib" \
    -ldl -lpthread

echo "== no-GC fallback lane (ported verbatim) =="
"$TCC_FOLDER/tcc.exe" thirdparty/tccbin_tests/shared/crash.c -o /tmp/crash_nogc_validate.exe
if [ ! -x /tmp/crash_nogc_validate.exe ]; then
  echo "::error::expected tcc to produce a runnable /tmp/crash_nogc_validate.exe, but it's missing or not executable."
  exit 1
fi
set +e
/tmp/crash_nogc_validate.exe
nogc_code=$?
set -e
if [ "$nogc_code" -ne 139 ]; then
  echo "::error::expected exit 139 (killed by SIGSEGV) from an unguarded null-pointer dereference, got $nogc_code"
  exit 1
fi
cat > /tmp/nogc_trivial_validate.c <<'TRIVIALEOF'
int main(void) { return 0; }
TRIVIALEOF
"$TCC_FOLDER/tcc.exe" /tmp/nogc_trivial_validate.c -o /tmp/nogc_trivial_validate.exe
if [ ! -x /tmp/nogc_trivial_validate.exe ]; then
  echo "::error::expected tcc to produce a runnable trivial no-GC binary, but it's missing or not executable."
  exit 1
fi
set +e
/tmp/nogc_trivial_validate.exe
trivial_code=$?
set -e
if [ "$trivial_code" -ne 0 ]; then
  echo "::error::expected the trivial no-GC program to exit 0, got $trivial_code."
  exit 1
fi
echo "no-GC fallback lane passed (crash.c SIGSEGV as expected, trivial program exits 0)"

echo "== static libgc.a lane (signature-checked XFAIL only, never blocking) =="
set +e
static_output=$("$run_sh" "$TCC_FOLDER/tcc.exe" macos -- \
    "${GC_CFLAGS[@]}" \
    -I thirdparty/libgc/include \
    "$TCC_FOLDER/lib/libgc.a" \
    -ldl -lpthread 2>&1)
static_code=$?
set -e
echo "$static_output"

if [ "$static_code" -eq 0 ]; then
  echo "Static libgc.a linking now succeeds on macOS amd64 - not treated as a failure here (this validator's job is to confirm the pair works; if static linking has genuinely started working, that's good news, not a regression). Flag this for a human to consider folding the static lane back into a single blocking lane."
else
  # Direct per-test probes, ported verbatim - these, not the aggregate
  # summary line, are the actual authority on whether this is the known
  # archive-parsing limitation.
  set +e
  direct_err_gc_alloc=$("$TCC_FOLDER/tcc.exe" thirdparty/tccbin_tests/shared/gc_alloc.c \
      "${GC_CFLAGS[@]}" \
      -I thirdparty/libgc/include \
      "$TCC_FOLDER/lib/libgc.a" \
      -ldl -lpthread \
      -o /tmp/gc_alloc_xfail_validate 2>&1)
  direct_code_gc_alloc=$?
  direct_err_hello=$("$TCC_FOLDER/tcc.exe" thirdparty/tccbin_tests/shared/hello.c \
      "${GC_CFLAGS[@]}" \
      -I thirdparty/libgc/include \
      "$TCC_FOLDER/lib/libgc.a" \
      -ldl -lpthread \
      -o /tmp/hello_xfail_validate 2>&1)
  direct_code_hello=$?
  set -e

  is_known_gc_init() {
    # $1 = captured stderr text, $2 = the probe's own exit code.
    if [ "$2" -eq 0 ] || [ "$2" -gt 128 ]; then
      return 1
    fi
    case "$1" in
      *"unresolved reference to '_GC_init'"*|*"unresolved reference to 'GC_init'"*) ;;
      *) return 1 ;;
    esac
    other=$(printf '%s\n' "$1" | grep -E '^tcc: error:' | grep -v -E "unresolved reference to '_?GC_[A-Za-z0-9_]+'")
    [ -z "$other" ]
  }

  # Adapted from tccbin's `*"PASS crash"*"FAIL gc_alloc (compile error)"*"FAIL hello (compile error)"*`:
  # the current run.sh appends detail after "(compile error: ...)", so
  # an exact trailing-paren match would never fire. Match only the
  # stable prefix of each line; the direct checks above remain the
  # actual authority regardless of what the aggregate shape says.
  case "$static_output" in
    *"PASS crash"*"FAIL gc_alloc"*"FAIL hello"*)
      if is_known_gc_init "$direct_err_gc_alloc" "$direct_code_gc_alloc" && is_known_gc_init "$direct_err_hello" "$direct_code_hello"; then
        echo "known XFAIL: static libgc.a still can't link gc_alloc.c/hello.c (unresolved GC_init, the known tcc archive-parsing bug), as expected - not blocking"
      else
        echo "::error::static lane failed with the known aggregate shape, but at least one of gc_alloc.c/hello.c's direct compile errors does NOT mention an unresolved GC_init reference (or exited abnormally) - this looks like a different, unexpected regression:"
        echo "gc_alloc.c: exit=$direct_code_gc_alloc: $direct_err_gc_alloc"
        echo "hello.c: exit=$direct_code_hello: $direct_err_hello"
        exit 1
      fi
      ;;
    *)
      echo "::error::static libgc.a lane failed, but NOT with the known/expected shape (PASS crash, FAIL gc_alloc/hello) - this looks like a different, unexpected problem, not the known XFAIL."
      exit 1
      ;;
  esac
fi

echo "All gates passed: dylib lane blocking-green, no-GC fallback green, static lane is either green or the known signature-checked XFAIL."
