#ifndef V_ATOMICS_H
#define V_ATOMICS_H

#include <stdint.h>

// ============================================================
// Canonical cross-compiler atomics for V's C runtime.
//
// This is the single implementation shared by the prealloc atomics
// (prealloc_atomics.h), the V garbage collector (thirdparty/vgc/
// vgc_platform.h) and the Boehm helpers (gc_boehm_helpers.c). No other file
// may redeclare the TinyCC libtcc1 helpers below: a conflicting
// redeclaration (notably a six-argument __atomic_compare_exchange_4) breaks
// the TinyCC build of every translation unit that includes two of V's
// atomic headers, and the V driver then silently falls back to another C
// compiler.
//
// Normalized semantics on every target:
//   * atomic load/store are acquire/release;
//   * add/sub return the NEW value;
//   * cas is a strong compare-and-swap that writes the observed value into
//     *expected on failure (C11 semantics);
//   * fence is sequentially consistent.
// ============================================================

#if defined(__TINYC__)
// Exactly the canonical five-argument libtcc1 declarations from
// thirdparty/stdatomic/nix/atomic.h:66-78. Types are spelled exactly as
// there: unsigned int* for the 4-byte helpers, unsigned long long* for the
// 8-byte helpers. Keep this one and only one declaration site.
extern unsigned int __atomic_load_4(unsigned int* ptr, int order);
extern unsigned long long __atomic_load_8(unsigned long long* ptr, int order);
extern void __atomic_store_4(unsigned int* ptr, unsigned int val, int order);
extern void __atomic_store_8(unsigned long long* ptr, unsigned long long val, int order);
extern unsigned int __atomic_exchange_4(unsigned int* ptr, unsigned int val, int order);
extern _Bool __atomic_compare_exchange_4(unsigned int* ptr, unsigned int* expected,
	unsigned int desired, int success_order, int failure_order);

static inline uint32_t v_atomic_load_u32(volatile uint32_t* p) {
	return (uint32_t)__atomic_load_4((unsigned int*)p, 5);
}
static inline void v_atomic_store_u32(volatile uint32_t* p, uint32_t v) {
	__atomic_store_4((unsigned int*)p, (unsigned int)v, 5);
}
static inline uint64_t v_atomic_load_u64(volatile uint64_t* p) {
	return (uint64_t)__atomic_load_8((unsigned long long*)p, 5);
}
static inline void v_atomic_store_u64(volatile uint64_t* p, uint64_t v) {
	__atomic_store_8((unsigned long long*)p, (unsigned long long)v, 5);
}
static inline uint32_t v_atomic_add_u32(volatile uint32_t* p, uint32_t v) {
	return (uint32_t)__atomic_add_fetch((volatile unsigned int*)p, (unsigned int)v, 5);
}
static inline uint32_t v_atomic_sub_u32(volatile uint32_t* p, uint32_t v) {
	return (uint32_t)__atomic_sub_fetch((volatile unsigned int*)p, (unsigned int)v, 5);
}
static inline uint64_t v_atomic_add_u64(volatile uint64_t* p, uint64_t v) {
	return (uint64_t)__atomic_add_fetch(p, v, 5);
}
static inline uint64_t v_atomic_sub_u64(volatile uint64_t* p, uint64_t v) {
	return (uint64_t)__atomic_sub_fetch(p, v, 5);
}
static inline _Bool v_atomic_cas_u32(volatile uint32_t* p, uint32_t* expected, uint32_t desired) {
	return __atomic_compare_exchange_4((unsigned int*)p, (unsigned int*)expected,
		(unsigned int)desired, 5, 5);
}
static inline uint32_t v_atomic_exchange_u32(volatile uint32_t* p, uint32_t v) {
	return (uint32_t)__atomic_exchange_4((unsigned int*)p, (unsigned int)v, 5);
}
#if defined(__x86_64__) || defined(__i386__)
static inline void v_atomic_fence(void) { __asm__ __volatile__("mfence" ::: "memory"); }
#elif defined(__aarch64__) || defined(__arm__)
static inline void v_atomic_fence(void) { __asm__ __volatile__("dmb ish" ::: "memory"); }
#elif defined(__riscv)
static inline void v_atomic_fence(void) { __asm__ __volatile__("fence rw,rw" ::: "memory"); }
#else
static inline void v_atomic_fence(void) { __asm__ __volatile__("" ::: "memory"); }
#endif

#elif defined(_MSC_VER)
#include <intrin.h>
static inline uint32_t v_atomic_load_u32(volatile uint32_t* p) {
	uint32_t v = *p;
	_ReadBarrier();
	return v;
}
static inline void v_atomic_store_u32(volatile uint32_t* p, uint32_t v) {
	_WriteBarrier();
	*p = v;
}
static inline uint64_t v_atomic_load_u64(volatile uint64_t* p) {
	uint64_t v = *p;
	_ReadBarrier();
	return v;
}
static inline void v_atomic_store_u64(volatile uint64_t* p, uint64_t v) {
	_WriteBarrier();
	*p = v;
}
static inline uint32_t v_atomic_add_u32(volatile uint32_t* p, uint32_t v) {
	uint32_t old = (uint32_t)_InterlockedExchangeAdd((volatile long*)p, (long)v);
	return old + v;
}
static inline uint32_t v_atomic_sub_u32(volatile uint32_t* p, uint32_t v) {
	uint32_t old = (uint32_t)_InterlockedExchangeAdd((volatile long*)p, -(long)v);
	return old - v;
}
static inline uint64_t v_atomic_add_u64(volatile uint64_t* p, uint64_t v) {
	uint64_t old = (uint64_t)_InterlockedExchangeAdd64((volatile long long*)p, (long long)v);
	return old + v;
}
static inline uint64_t v_atomic_sub_u64(volatile uint64_t* p, uint64_t v) {
	uint64_t old = (uint64_t)_InterlockedExchangeAdd64((volatile long long*)p, -(long long)v);
	return old - v;
}
static inline _Bool v_atomic_cas_u32(volatile uint32_t* p, uint32_t* expected, uint32_t desired) {
	uint32_t observed = (uint32_t)_InterlockedCompareExchange((volatile long*)p,
		(long)desired, (long)*expected);
	if (observed == *expected) return 1;
	*expected = observed; // C11 semantics: report the value that was seen.
	return 0;
}
static inline uint32_t v_atomic_exchange_u32(volatile uint32_t* p, uint32_t v) {
	return (uint32_t)_InterlockedExchange((volatile long*)p, (long)v);
}
// A full sequentially-consistent fence (the old vgc path used only a compiler
// barrier here). Use the hardware fence on the architectures we support.
#if defined(_M_X64) || defined(_M_IX86)
static inline void v_atomic_fence(void) { _mm_mfence(); }
#elif defined(_M_ARM64)
static inline void v_atomic_fence(void) { __dmb(_ARM64_BARRIER_ISH); }
#else
static inline void v_atomic_fence(void) { _ReadWriteBarrier(); }
#endif

#else
// GCC / Clang.
static inline uint32_t v_atomic_load_u32(volatile uint32_t* p) {
	return __atomic_load_n(p, __ATOMIC_ACQUIRE);
}
static inline void v_atomic_store_u32(volatile uint32_t* p, uint32_t v) {
	__atomic_store_n(p, v, __ATOMIC_RELEASE);
}
static inline uint64_t v_atomic_load_u64(volatile uint64_t* p) {
	return __atomic_load_n(p, __ATOMIC_ACQUIRE);
}
static inline void v_atomic_store_u64(volatile uint64_t* p, uint64_t v) {
	__atomic_store_n(p, v, __ATOMIC_RELEASE);
}
static inline uint32_t v_atomic_add_u32(volatile uint32_t* p, uint32_t v) {
	return __atomic_add_fetch(p, v, __ATOMIC_ACQ_REL);
}
static inline uint32_t v_atomic_sub_u32(volatile uint32_t* p, uint32_t v) {
	return __atomic_sub_fetch(p, v, __ATOMIC_ACQ_REL);
}
static inline uint64_t v_atomic_add_u64(volatile uint64_t* p, uint64_t v) {
	return __atomic_add_fetch(p, v, __ATOMIC_ACQ_REL);
}
static inline uint64_t v_atomic_sub_u64(volatile uint64_t* p, uint64_t v) {
	return __atomic_sub_fetch(p, v, __ATOMIC_ACQ_REL);
}
static inline _Bool v_atomic_cas_u32(volatile uint32_t* p, uint32_t* expected, uint32_t desired) {
	return __atomic_compare_exchange_n(p, expected, desired, 0,
		__ATOMIC_ACQ_REL, __ATOMIC_ACQUIRE);
}
static inline uint32_t v_atomic_exchange_u32(volatile uint32_t* p, uint32_t v) {
	return __atomic_exchange_n(p, v, __ATOMIC_ACQ_REL);
}
static inline void v_atomic_fence(void) { __atomic_thread_fence(__ATOMIC_SEQ_CST); }
#endif

// ============================================================
// Counters built on the atomics above.
//
// v_counter_add_shared / v_counter_load_shared are always atomic
// read-modify-writes: they are used by the Boehm growth counters and the
// prealloc counters, which several threads touch at once.
//
// v_counter_add_owner / v_counter_load_owner mirror the old vgc_counter_*
// semantics: only the owning thread adds, any thread reads. On a 64-bit
// target a volatile access to an aligned word does not tear, so the add is a
// plain load/store and costs no call even without optimization; on 32-bit it
// falls back to a real atomic add. They stay macros for that reason.
// ============================================================
static inline void v_counter_add_shared(volatile uint64_t* p, uint64_t v) {
	(void)v_atomic_add_u64(p, v);
}
static inline uint64_t v_counter_load_shared(volatile uint64_t* p) {
	return v_atomic_load_u64(p);
}

#if UINTPTR_MAX > 0xFFFFFFFFu
#define v_counter_load_owner(ptr) (*(volatile uint64_t*)(ptr))
#define v_counter_add_owner(ptr, val) ((void)(*(volatile uint64_t*)(ptr) += (uint64_t)(val)))
#else
#define v_counter_load_owner(ptr) v_atomic_load_u64(ptr)
#define v_counter_add_owner(ptr, val) ((void)v_atomic_add_u64(ptr, val))
#endif

#endif // V_ATOMICS_H
