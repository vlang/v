#ifndef V_ALLOC_FASTPATHS_H
#define V_ALLOC_FASTPATHS_H

#include <stddef.h>
#include <stdint.h>
#include <string.h>

// ============================================================
// Small allocation fast paths shared by the prealloc arena and VGC.
// They are pure C (no V dependency) so both allocators can use them.
// ============================================================

// Zero only the part of [ptr, ptr + n) beyond `zero_until`, the end of the
// region already known to be zero (fresh mmap pages). A caller with no
// guaranteed-zero region passes zero_until == ptr, which zeroes it all.
static inline void v_zero_if_needed(void* ptr, size_t n, const void* zero_until) {
	uintptr_t p = (uintptr_t)ptr;
	uintptr_t z = (uintptr_t)zero_until;
	if (p >= z) {
		memset(ptr, 0, n);
		return;
	}
	uintptr_t end = p + n;
	if (end > z) {
		memset((void*)z, 0, (size_t)(end - z));
	}
}

// Advisory huge-page hint for large OS-backed arenas. No-op elsewhere.
#if defined(__linux__) && !defined(__ANDROID__)
// Declared here so TCC's headers, which need feature macros for madvise and
// for MADV_HUGEPAGE, do not have to be pulled in. A repeated compatible
// declaration from <sys/mman.h> is harmless in C.
extern int madvise(void* addr, size_t length, int advice);
static inline void v_os_hugepage_hint(void* ptr, size_t size) {
	// MADV_HUGEPAGE is 14 in Linux's madvise.h. The literal keeps TinyCC happy.
	(void)madvise(ptr, size, 14);
}
#else
static inline void v_os_hugepage_hint(void* ptr, size_t size) {
	(void)ptr;
	(void)size;
}
#endif

#endif // V_ALLOC_FASTPATHS_H
