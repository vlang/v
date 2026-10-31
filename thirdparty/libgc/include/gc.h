/* This file is installed for backward compatibility. */
#include "gc/gc.h"

/* Fix for vlang/v#27179: the original version of this file cast the
 * pointer argument to V's `u64` type, which makes the header illegal
 * to include from standalone C (any C shim doing `#include <gc.h>`
 * via V's -I@VEXEROOT/thirdparty/libgc/include path would fail with
 * "unknown type: u64"). `GC_word` is the libgc-defined type that
 * `GC_noop1` actually takes (gc/gc.h, typedef'd to uintptr_t on most
 * platforms) — using it makes this header valid both from V code and
 * from plain C shims. */
#ifndef _MSC_VER
/* The fallback must stay weak, so that a libgc which defines GC_noop1_ptr
 * wins at link time. It is spelled `__attribute`, not `__attribute__`:
 * TinyCC does not define __GNUC__ on Linux, so glibc's <sys/cdefs.h>
 * defines `__attribute__(xyz)` to nothing, and after any libc header the
 * fallback became a strong definition. TinyCC then reported
 * "'GC_noop1_ptr' defined twice" against such a libgc.a, and since TinyCC
 * mob e23dabed that is a link error. GCC, clang and TinyCC all accept the
 * `__attribute` spelling, which glibc leaves alone. */
__attribute ((weak)) GC_API void GC_CALL GC_noop1_ptr(volatile void *p) {
   GC_noop1((GC_word)p);
}
#endif
