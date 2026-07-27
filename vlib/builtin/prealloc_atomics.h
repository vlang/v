#ifndef V_PREALLOC_ATOMICS_H
#define V_PREALLOC_ATOMICS_H

#if defined(_MSC_VER)
#include <intrin.h>
static inline int v_prealloc_atomic_add_i32(int *ptr, int delta) {
	return (int)_InterlockedExchangeAdd((volatile long*)ptr, (long)delta) + delta;
}
static inline int v_prealloc_atomic_load_i32(int *ptr) {
	return (int)_InterlockedCompareExchange((volatile long*)ptr, 0, 0);
}
static inline int v_prealloc_atomic_store_i32(int *ptr, int val) {
	_InterlockedExchange((volatile long*)ptr, (long)val);
	return val;
}
static inline int v_prealloc_atomic_cas_i32(int *ptr, int expected, int desired) {
	return _InterlockedCompareExchange((volatile long*)ptr, (long)desired, (long)expected) == expected;
}
static inline long long v_prealloc_atomic_add_i64(long long *ptr, long long delta) {
	return _InterlockedExchangeAdd64((volatile long long*)ptr, delta) + delta;
}
static inline long long v_prealloc_atomic_load_i64(long long *ptr) {
	return _InterlockedCompareExchange64((volatile long long*)ptr, 0, 0);
}
#else
static inline int v_prealloc_atomic_add_i32(int *ptr, int delta) {
	return __sync_add_and_fetch(ptr, delta);
}
static inline int v_prealloc_atomic_load_i32(int *ptr) {
	return __sync_add_and_fetch(ptr, 0);
}
static inline int v_prealloc_atomic_store_i32(int *ptr, int val) {
	return __sync_lock_test_and_set(ptr, val);
}
static inline int v_prealloc_atomic_cas_i32(int *ptr, int expected, int desired) {
	return __sync_bool_compare_and_swap(ptr, expected, desired);
}
static inline long long v_prealloc_atomic_add_i64(long long *ptr, long long delta) {
	return __sync_add_and_fetch(ptr, delta);
}
static inline long long v_prealloc_atomic_load_i64(long long *ptr) {
	return __sync_add_and_fetch(ptr, 0);
}
#endif

#endif
