#ifndef V_PREALLOC_ATOMICS_H
#define V_PREALLOC_ATOMICS_H

// All cross-compiler atomics live in v_atomics.h. This header only adapts
// them to the signed int / long long interface that V calls through
// `C.v_prealloc_atomic_*`; the function names and signatures are unchanged.
#include "v_atomics.h"

static inline int v_prealloc_atomic_add_i32(int *ptr, int delta) {
	return (int)v_atomic_add_u32((volatile uint32_t*)ptr, (uint32_t)delta);
}
static inline int v_prealloc_atomic_load_i32(int *ptr) {
	return (int)v_atomic_load_u32((volatile uint32_t*)ptr);
}
static inline int v_prealloc_atomic_store_i32(int *ptr, int val) {
	v_atomic_store_u32((volatile uint32_t*)ptr, (uint32_t)val);
	return val;
}
static inline int v_prealloc_atomic_cas_i32(int *ptr, int expected, int desired) {
	uint32_t observed = (uint32_t)expected;
	return v_atomic_cas_u32((volatile uint32_t*)ptr, &observed, (uint32_t)desired) ? 1 : 0;
}
static inline long long v_prealloc_atomic_add_i64(long long *ptr, long long delta) {
	return (long long)v_atomic_add_u64((volatile uint64_t*)ptr, (uint64_t)delta);
}
static inline long long v_prealloc_atomic_load_i64(long long *ptr) {
	return (long long)v_atomic_load_u64((volatile uint64_t*)ptr);
}

#endif
