/*
    Compatibility header for stdatomic.h that works for all compilers supported by V.
    For TCC, we use libatomic from the OS.
*/
#ifndef __ATOMIC_H
#define __ATOMIC_H

#ifndef __cplusplus
// If C just use stdatomic.h
#ifndef __TINYC__
#include <stdatomic.h>
#endif
#else
// CPP wrapper for atomic operations that are compatible with C
#include "atomic_cpp.h"
#endif

#ifdef __TINYC__

typedef volatile long long atomic_llong;
typedef volatile unsigned long long atomic_ullong;
typedef volatile uintptr_t atomic_uintptr_t;

// use functions for 64, 32 and 8 bit from libatomic directly
// since tcc is not capible to use "generic" C functions
// there is no header file for libatomic so we provide function declarations here

extern unsigned long long __atomic_load_8(unsigned long long* x, int mo);
extern void __atomic_store_8(unsigned long long* x, unsigned long long y, int mo);
extern _Bool __atomic_compare_exchange_8(unsigned long long* x, unsigned long long* expected, unsigned long long y, int mo, int mo2);
extern _Bool __atomic_compare_exchange_8(unsigned long long* x, unsigned long long* expected, unsigned long long y, int mo, int mo2);
extern unsigned long long __atomic_exchange_8(unsigned long long* x, unsigned long long y, int mo);
extern unsigned long long __atomic_fetch_add_8(unsigned long long* x, unsigned long long y, int mo);
extern unsigned long long __atomic_fetch_sub_8(unsigned long long* x, unsigned long long y, int mo);
extern unsigned long long __atomic_fetch_and_8(unsigned long long* x, unsigned long long y, int mo);
extern unsigned long long __atomic_fetch_or_8(unsigned long long* x, unsigned long long y, int mo);
extern unsigned long long __atomic_fetch_xor_8(unsigned long long* x, unsigned long long y, int mo);

extern unsigned int __atomic_load_4(unsigned int* x, int mo);
extern void __atomic_store_4(unsigned int* x, unsigned int y, int mo);
extern _Bool __atomic_compare_exchange_4(unsigned int* x, unsigned int* expected, unsigned int y, int mo, int mo2);
extern _Bool __atomic_compare_exchange_4(unsigned int* x, unsigned int* expected, unsigned int y, int mo, int mo2);
extern unsigned int __atomic_exchange_4(unsigned int* x, unsigned int y, int mo);
extern unsigned int __atomic_fetch_add_4(unsigned int* x, unsigned int y, int mo);
extern unsigned int __atomic_fetch_sub_4(unsigned int* x, unsigned int y, int mo);
extern unsigned int __atomic_fetch_and_4(unsigned int* x, unsigned int y, int mo);
extern unsigned int __atomic_fetch_or_4(unsigned int* x, unsigned int y, int mo);
extern unsigned int __atomic_fetch_xor_4(unsigned int* x, unsigned int y, int mo);

extern unsigned short __atomic_load_2(unsigned short* x, int mo);
extern void __atomic_store_2(unsigned short* x, unsigned short y, int mo);
extern _Bool __atomic_compare_exchange_2(unsigned short* x, unsigned short* expected, unsigned short y, int mo, int mo2);
extern _Bool __atomic_compare_exchange_2(unsigned short* x, unsigned short* expected, unsigned short y, int mo, int mo2);
extern unsigned short __atomic_exchange_2(unsigned short* x, unsigned short y, int mo);
extern unsigned short __atomic_fetch_add_2(unsigned short* x, unsigned short y, int mo);
extern unsigned short __atomic_fetch_sub_2(unsigned short* x, unsigned short y, int mo);
extern unsigned short __atomic_fetch_and_2(unsigned short* x, unsigned short y, int mo);
extern unsigned short __atomic_fetch_or_2(unsigned short* x, unsigned short y, int mo);
extern unsigned short __atomic_fetch_xor_2(unsigned short* x, unsigned short y, int mo);

extern unsigned char __atomic_load_1(unsigned char* x, int mo);
extern void __atomic_store_1(unsigned char* x, unsigned char y, int mo);
extern _Bool __atomic_compare_exchange_1(unsigned char* x, unsigned char* expected, unsigned char y, int mo, int mo2);
extern _Bool __atomic_compare_exchange_1(unsigned char* x, unsigned char* expected, unsigned char y, int mo, int mo2);
extern unsigned char __atomic_exchange_1(unsigned char* x, unsigned char y, int mo);
extern unsigned char __atomic_fetch_add_1(unsigned char* x, unsigned char y, int mo);
extern unsigned char __atomic_fetch_sub_1(unsigned char* x, unsigned char y, int mo);
extern unsigned char __atomic_fetch_and_1(unsigned char* x, unsigned char y, int mo);
extern unsigned char __atomic_fetch_or_1(unsigned char* x, unsigned char y, int mo);
extern unsigned char __atomic_fetch_xor_1(unsigned char* x, unsigned char y, int mo);

// The default functions should work with pointers so we have to decide based on pointer size
#if UINTPTR_MAX == 0xFFFFFFFF

#define atomic_load_explicit __atomic_load_4
#define atomic_store_explicit __atomic_store_4
#define atomic_compare_exchange_weak_explicit __atomic_compare_exchange_4
#define atomic_compare_exchange_strong_explicit __atomic_compare_exchange_4
#define atomic_exchange_explicit __atomic_exchange_4
#define atomic_fetch_add_explicit __atomic_fetch_add_4
#define atomic_fetch_sub_explicit __atomic_sub_fetch_4

#else

#define atomic_load_explicit __atomic_load_8
#define atomic_store_explicit __atomic_store_8
#define atomic_compare_exchange_weak_explicit __atomic_compare_exchange_8
#define atomic_compare_exchange_strong_explicit __atomic_compare_exchange_8
#define atomic_exchange_explicit __atomic_exchange_8
#define atomic_fetch_add_explicit __atomic_fetch_add_8
#define atomic_fetch_sub_explicit __atomic_sub_fetch_8

#endif

// memory order policies - we use "sequentially consistent" by default

#define memory_order_relaxed 0
#define memory_order_consume 1
#define memory_order_acquire 2
#define memory_order_release 3
#define memory_order_acq_rel 4
#define memory_order_seq_cst 5

static inline void** atomic_load(void** x) {
	return (void**)atomic_load_explicit((unsigned long long*)x, memory_order_seq_cst);
}
static inline void atomic_store(void** x, void* y) {
	atomic_store_explicit((unsigned long long*)x, (uintptr_t)y, memory_order_seq_cst);
}
static inline int atomic_compare_exchange_weak(void** x, void** expected, void* y) {
	return (int)atomic_compare_exchange_weak_explicit((unsigned long long*)x, (unsigned long long*)expected, (uintptr_t)y, memory_order_seq_cst, memory_order_seq_cst);
}
static inline int atomic_compare_exchange_strong(void** x,  void** expected, void* y) {
	return (int)atomic_compare_exchange_strong_explicit((unsigned long long*)x, (unsigned long long*)expected, (uintptr_t)y, memory_order_seq_cst, memory_order_seq_cst);
}
static inline uintptr_t atomic_exchange(void** x, void* y) {
	return atomic_exchange_explicit((unsigned long long*)x, (uintptr_t)y, memory_order_seq_cst);
}
static inline uintptr_t atomic_fetch_add(uintptr_t* x, uintptr_t y) {
	return atomic_fetch_add_explicit(x, y, memory_order_seq_cst);
}
static inline uintptr_t atomic_fetch_sub(uintptr_t* x, uintptr_t y) {
	return atomic_fetch_sub_explicit(x, y, memory_order_seq_cst);
}
static inline uintptr_t atomic_fetch_and(uintptr_t* x, uintptr_t y) {
	return atomic_fetch_and_explicit(x, y, memory_order_seq_cst);
}
static inline uintptr_t atomic_fetch_or(uintptr_t* x, uintptr_t y) {
	return atomic_fetch_or_explicit(x, y, memory_order_seq_cst);
}
static inline uintptr_t atomic_fetch_xor(uintptr_t* x, uintptr_t y) {
	return atomic_fetch_xor_explicit(x, y, memory_order_seq_cst);
}

#define atomic_load_ptr atomic_load
#define atomic_store_ptr atomic_store
#define atomic_compare_exchange_weak_ptr atomic_compare_exchange_weak
#define atomic_compare_exchange_strong_ptr atomic_compare_exchange_strong
#define atomic_exchange_ptr atomic_exchange
#define atomic_fetch_add_ptr atomic_fetch_add
#define atomic_fetch_sub_ptr atomic_fetch_sub
#define atomic_fetch_and_ptr atomic_fetch_and
#define atomic_fetch_or_ptr atomic_fetch_or
#define atomic_fetch_xor_ptr atomic_fetch_xor

// specialized versions for 64 bit

static inline unsigned long long atomic_load_u64(unsigned long long* x) {
	return __atomic_load_8(x, memory_order_seq_cst);
}
static inline void atomic_store_u64(unsigned long long* x, unsigned long long y) {
	__atomic_store_8(x, y, memory_order_seq_cst);
}
static inline int atomic_compare_exchange_weak_u64(unsigned long long* x, unsigned long long* expected, unsigned long long y) {
	return (int)__atomic_compare_exchange_8(x, expected, y, memory_order_seq_cst, memory_order_seq_cst);
}
static inline int atomic_compare_exchange_strong_u64(unsigned long long* x, unsigned long long* expected, unsigned long long y) {
	return (int)__atomic_compare_exchange_8(x, expected, y, memory_order_seq_cst, memory_order_seq_cst);
}
static inline unsigned long long atomic_exchange_u64(unsigned long long* x, unsigned long long y) {
	return __atomic_exchange_8(x, y, memory_order_seq_cst);
}
static inline unsigned long long atomic_fetch_add_u64(unsigned long long* x, unsigned long long y) {
	return __atomic_fetch_add_8(x, y, memory_order_seq_cst);
}
static inline unsigned long long atomic_fetch_sub_u64(unsigned long long* x, unsigned long long y) {
	return __atomic_fetch_sub_8(x, y, memory_order_seq_cst);
}
static inline unsigned long long atomic_fetch_and_u64(unsigned long long* x, unsigned long long y) {
	return __atomic_fetch_and_8(x, y, memory_order_seq_cst);
}
static inline unsigned long long atomic_fetch_or_u64(unsigned long long* x, unsigned long long y) {
	return __atomic_fetch_or_8(x, y, memory_order_seq_cst);
}
static inline unsigned long long atomic_fetch_xor_u64(unsigned long long* x, unsigned long long y) {
	return __atomic_fetch_xor_8(x, y, memory_order_seq_cst);
}

static inline unsigned atomic_load_u32(unsigned* x) {
	return __atomic_load_4(x, memory_order_seq_cst);
}
static inline void atomic_store_u32(unsigned* x, unsigned y) {
	__atomic_store_4(x, y, memory_order_seq_cst);
}
static inline int atomic_compare_exchange_weak_u32(unsigned* x, unsigned* expected, unsigned y) {
	return (int)__atomic_compare_exchange_4(x, expected, y, memory_order_seq_cst, memory_order_seq_cst);
}
static inline int atomic_compare_exchange_strong_u32(unsigned* x, unsigned* expected, unsigned y) {
	return (int)__atomic_compare_exchange_4(x, expected, y, memory_order_seq_cst, memory_order_seq_cst);
}
static inline unsigned atomic_exchange_u32(unsigned* x, unsigned y) {
	return __atomic_exchange_4(x, y, memory_order_seq_cst);
}
static inline unsigned atomic_fetch_add_u32(unsigned* x, unsigned y) {
	return __atomic_fetch_add_4(x, y, memory_order_seq_cst);
}
static inline unsigned atomic_fetch_sub_u32(unsigned* x, unsigned y) {
	return __atomic_fetch_sub_4(x, y, memory_order_seq_cst);
}
static inline unsigned atomic_fetch_and_u32(unsigned* x, unsigned y) {
	return __atomic_fetch_and_4(x, y, memory_order_seq_cst);
}
static inline unsigned atomic_fetch_or_u32(unsigned* x, unsigned y) {
	return __atomic_fetch_or_4(x, y, memory_order_seq_cst);
}
static inline unsigned atomic_fetch_xor_u32(unsigned* x, unsigned y) {
	return __atomic_fetch_xor_4(x, y, memory_order_seq_cst);
}

static inline unsigned short atomic_load_u16(unsigned short* x) {
	return __atomic_load_2(x, memory_order_seq_cst);
}
static inline void atomic_store_u16(void* x, unsigned short y) {
	__atomic_store_2(x, y, memory_order_seq_cst);
}
static inline int atomic_compare_exchange_weak_u16(void* x, unsigned short* expected, unsigned short y) {
	return (int)__atomic_compare_exchange_2(x, expected, y, memory_order_seq_cst, memory_order_seq_cst);
}
static inline int atomic_compare_exchange_strong_u16(unsigned short* x, unsigned short* expected, unsigned short y) {
	return (int)__atomic_compare_exchange_2(x, expected, y, memory_order_seq_cst, memory_order_seq_cst);
}
static inline unsigned short atomic_exchange_u16(unsigned short* x, unsigned short y) {
	return __atomic_exchange_2(x, y, memory_order_seq_cst);
}
static inline unsigned short atomic_fetch_add_u16(unsigned short* x, unsigned short y) {
	return __atomic_fetch_add_2(x, y, memory_order_seq_cst);
}
static inline unsigned short atomic_fetch_sub_u16(unsigned short* x, unsigned short y) {
	return __atomic_fetch_sub_2(x, y, memory_order_seq_cst);
}
static inline unsigned short atomic_fetch_and_u16(unsigned short* x, unsigned short y) {
	return __atomic_fetch_and_2(x, y, memory_order_seq_cst);
}
static inline unsigned short atomic_fetch_or_u16(unsigned short* x, unsigned short y) {
	return __atomic_fetch_or_2(x, y, memory_order_seq_cst);
}
static inline unsigned short atomic_fetch_xor_u16(unsigned short* x, unsigned short y) {
	return __atomic_fetch_xor_2(x, y, memory_order_seq_cst);
}

static inline unsigned char atomic_load_byte(unsigned char* x) {
	return __atomic_load_1(x, memory_order_seq_cst);
}
static inline void atomic_store_byte(unsigned char* x, unsigned char y) {
	__atomic_store_1(x, y, memory_order_seq_cst);
}
static inline int atomic_compare_exchange_weak_byte(unsigned char* x, unsigned char* expected, unsigned char y) {
	return __atomic_compare_exchange_1(x, expected, y, memory_order_seq_cst, memory_order_seq_cst);
}
static inline int atomic_compare_exchange_strong_byte(unsigned char* x, unsigned char* expected, unsigned char y) {
	return __atomic_compare_exchange_1(x, expected, y, memory_order_seq_cst, memory_order_seq_cst);
}
static inline unsigned char atomic_exchange_byte(unsigned char* x, unsigned char y) {
	return __atomic_exchange_1(x, y, memory_order_seq_cst);
}
static inline unsigned char atomic_fetch_add_byte(unsigned char* x, unsigned char y) {
	return __atomic_fetch_add_1(x, y, memory_order_seq_cst);
}
static inline unsigned char atomic_fetch_sub_byte(unsigned char* x, unsigned char y) {
	return __atomic_fetch_sub_1(x, y, memory_order_seq_cst);
}
static inline unsigned char atomic_fetch_and_byte(unsigned char* x, unsigned char y) {
	return __atomic_fetch_and_1(x, y, memory_order_seq_cst);
}
static inline unsigned char atomic_fetch_or_byte(unsigned char* x, unsigned char y) {
	return __atomic_fetch_or_1(x, y, memory_order_seq_cst);
}
static inline unsigned char atomic_fetch_xor_byte(unsigned char* x, unsigned char y) {
	return __atomic_fetch_xor_1(x, y, memory_order_seq_cst);
}

#else

// Since V might be confused with "generic" C functions either we provide special versions
// for gcc/clang, too
static inline unsigned long long atomic_load_u64(unsigned long long* x) {
	return atomic_load_explicit((_Atomic (unsigned long long)*)x, memory_order_seq_cst);
}
static inline void atomic_store_u64(unsigned long long* x, unsigned long long y) {
	atomic_store_explicit((_Atomic(unsigned long long)*)x, y, memory_order_seq_cst);
}
static inline int atomic_compare_exchange_weak_u64(unsigned long long* x, unsigned long long* expected, unsigned long long y) {
	return (int)atomic_compare_exchange_weak_explicit((_Atomic(unsigned long long)*)x, expected, y, memory_order_seq_cst, memory_order_seq_cst);
}
static inline int atomic_compare_exchange_strong_u64(unsigned long long* x, unsigned long long* expected, unsigned long long y) {
	return (int)atomic_compare_exchange_strong_explicit((_Atomic(unsigned long long)*)x, expected, y, memory_order_seq_cst, memory_order_seq_cst);
}
static inline unsigned long long atomic_exchange_u64(unsigned long long* x, unsigned long long y) {
	return atomic_exchange_explicit((_Atomic(unsigned long long)*)x, y, memory_order_seq_cst);
}
static inline unsigned long long atomic_fetch_add_u64(unsigned long long* x, unsigned long long y) {
	return atomic_fetch_add_explicit((_Atomic(unsigned long long)*)x, y, memory_order_seq_cst);
}
static inline unsigned long long atomic_fetch_sub_u64(unsigned long long* x, unsigned long long y) {
	return atomic_fetch_sub_explicit((_Atomic(unsigned long long)*)x, y, memory_order_seq_cst);
}
static inline unsigned long long atomic_fetch_and_u64(unsigned long long* x, unsigned long long y) {
	return atomic_fetch_and_explicit((_Atomic(unsigned long long)*)x, y, memory_order_seq_cst);
}
static inline unsigned long long atomic_fetch_or_u64(unsigned long long* x, unsigned long long y) {
	return atomic_fetch_or_explicit((_Atomic(unsigned long long)*)x, y, memory_order_seq_cst);
}
static inline unsigned long long atomic_fetch_xor_u64(unsigned long long* x, unsigned long long y) {
	return atomic_fetch_xor_explicit((_Atomic(unsigned long long)*)x, y, memory_order_seq_cst);
}


static inline void* atomic_load_ptr(void** x) {
	return (void*)atomic_load_explicit((_Atomic(uintptr_t)*)x, memory_order_seq_cst);
}
static inline void atomic_store_ptr(void** x, void* y) {
	atomic_store_explicit((_Atomic(uintptr_t)*)x, (uintptr_t)y, memory_order_seq_cst);
}
static inline int atomic_compare_exchange_weak_ptr(void** x, void** expected, void* y) {
	return (int)atomic_compare_exchange_weak_explicit((_Atomic(uintptr_t)*)x, (unsigned long *)expected, (uintptr_t)y, memory_order_seq_cst, memory_order_seq_cst);
}
static inline int atomic_compare_exchange_strong_ptr(void** x, void** expected, void* y) {
	return (int)atomic_compare_exchange_strong_explicit((_Atomic(uintptr_t)*)x, (unsigned long *)expected, (uintptr_t)y, memory_order_seq_cst, memory_order_seq_cst);
}
static inline void* atomic_exchange_ptr(void** x, void* y) {
	return (void*)atomic_exchange_explicit((_Atomic(uintptr_t)*)x, (uintptr_t)y, memory_order_seq_cst);
}
static inline void* atomic_fetch_add_ptr(void** x, void* y) {
	return (void*)atomic_fetch_add_explicit((_Atomic(uintptr_t)*)x, (uintptr_t)y, memory_order_seq_cst);
}
static inline void* atomic_fetch_sub_ptr(void** x, void* y) {
	return (void*)atomic_fetch_sub_explicit((_Atomic(uintptr_t)*)x, (uintptr_t)y, memory_order_seq_cst);
}
static inline void* atomic_fetch_and_ptr(void** x, void* y) {
	return (void*)atomic_fetch_and_explicit((_Atomic(uintptr_t)*)x, (uintptr_t)y, memory_order_seq_cst);
}
static inline void* atomic_fetch_or_ptr(void** x, void* y) {
	return (void*)atomic_fetch_or_explicit((_Atomic(uintptr_t)*)x, (uintptr_t)y, memory_order_seq_cst);
}
static inline void* atomic_fetch_xor_ptr(void** x, void* y) {
	return (void*)atomic_fetch_xor_explicit((_Atomic(uintptr_t)*)x, (uintptr_t)y, memory_order_seq_cst);
}


static inline unsigned atomic_load_u32(unsigned* x) {
	return atomic_load_explicit((_Atomic(unsigned)*)x, memory_order_seq_cst);
}
static inline void atomic_store_u32(unsigned* x, unsigned y) {
	atomic_store_explicit((_Atomic(unsigned)*)x, y, memory_order_seq_cst);
}
static inline int atomic_compare_exchange_weak_u32(unsigned* x, unsigned* expected, unsigned y) {
	return (int)atomic_compare_exchange_weak_explicit((_Atomic(unsigned)*)x, expected, y, memory_order_seq_cst, memory_order_seq_cst);
}
static inline int atomic_compare_exchange_strong_u32(unsigned* x, unsigned* expected, unsigned y) {
	return (int)atomic_compare_exchange_strong_explicit((_Atomic(unsigned)*)x, expected, y, memory_order_seq_cst, memory_order_seq_cst);
}
static inline unsigned atomic_exchange_u32(unsigned* x, unsigned y) {
	return atomic_exchange_explicit((_Atomic(unsigned)*)x, y, memory_order_seq_cst);
}
static inline unsigned atomic_fetch_add_u32(unsigned* x, unsigned y) {
	return atomic_fetch_add_explicit((_Atomic(unsigned)*)x, y, memory_order_seq_cst);
}
static inline unsigned atomic_fetch_sub_u32(unsigned* x, unsigned y) {
	return atomic_fetch_sub_explicit((_Atomic(unsigned)*)x, y, memory_order_seq_cst);
}
static inline unsigned atomic_fetch_and_u32(unsigned* x, unsigned y) {
	return atomic_fetch_and_explicit((_Atomic(unsigned)*)x, y, memory_order_seq_cst);
}
static inline unsigned atomic_fetch_or_u32(unsigned* x, unsigned y) {
	return atomic_fetch_or_explicit((_Atomic(unsigned)*)x, y, memory_order_seq_cst);
}
static inline unsigned atomic_fetch_xor_u32(unsigned* x, unsigned y) {
	return atomic_fetch_xor_explicit((_Atomic(unsigned)*)x, y, memory_order_seq_cst);
}

static inline unsigned short atomic_load_u16(unsigned short* x) {
	return atomic_load_explicit((_Atomic(unsigned short)*)x, memory_order_seq_cst);
}
static inline void atomic_store_u16(void* x, unsigned short y) {
	atomic_store_explicit((_Atomic(unsigned short)*)x, y, memory_order_seq_cst);
}
static inline int atomic_compare_exchange_weak_u16(void* x, unsigned short* expected, unsigned short y) {
	return (int)atomic_compare_exchange_weak_explicit((_Atomic(unsigned short)*)x, expected, y, memory_order_seq_cst, memory_order_seq_cst);
}
static inline int atomic_compare_exchange_strong_u16(unsigned short* x, unsigned short* expected, unsigned short y) {
	return (int)atomic_compare_exchange_strong_explicit((_Atomic(unsigned short)*)x, expected, y, memory_order_seq_cst, memory_order_seq_cst);
}
static inline unsigned short atomic_exchange_u16(unsigned short* x, unsigned short y) {
	return atomic_exchange_explicit((_Atomic(unsigned short)*)x, y, memory_order_seq_cst);
}
static inline unsigned short atomic_fetch_add_u16(unsigned short* x, unsigned short y) {
	return atomic_fetch_add_explicit((_Atomic(unsigned short)*)x, y, memory_order_seq_cst);
}
static inline unsigned short atomic_fetch_sub_u16(unsigned short* x, unsigned short y) {
	return atomic_fetch_sub_explicit((_Atomic(unsigned short)*)x, y, memory_order_seq_cst);
}
static inline unsigned short atomic_fetch_and_u16(unsigned short* x, unsigned short y) {
	return atomic_fetch_and_explicit((_Atomic(unsigned short)*)x, y, memory_order_seq_cst);
}
static inline unsigned short atomic_fetch_or_u16(unsigned short* x, unsigned short y) {
	return atomic_fetch_or_explicit((_Atomic(unsigned short)*)x, y, memory_order_seq_cst);
}
static inline unsigned short atomic_fetch_xor_u16(unsigned short* x, unsigned short y) {
	return atomic_fetch_xor_explicit((_Atomic(unsigned short)*)x, y, memory_order_seq_cst);
}

static inline unsigned char atomic_load_byte(unsigned char* x) {
	return atomic_load_explicit((_Atomic(unsigned char)*)x, memory_order_seq_cst);
}
static inline void atomic_store_byte(unsigned char* x, unsigned char y) {
	atomic_store_explicit((_Atomic(unsigned char)*)x, y, memory_order_seq_cst);
}
static inline int atomic_compare_exchange_weak_byte(unsigned char* x, unsigned char* expected, unsigned char y) {
	return (int)atomic_compare_exchange_weak_explicit((_Atomic(unsigned char)*)x, expected, y, memory_order_seq_cst, memory_order_seq_cst);
}
static inline int atomic_compare_exchange_strong_byte(unsigned char* x, unsigned char* expected, unsigned char y) {
	return (int)atomic_compare_exchange_strong_explicit((_Atomic(unsigned char)*)x, expected, y, memory_order_seq_cst, memory_order_seq_cst);
}
static inline unsigned char atomic_exchange_byte(unsigned char* x, unsigned char y) {
	return atomic_exchange_explicit((_Atomic(unsigned char)*)x, y, memory_order_seq_cst);
}
static inline unsigned char atomic_fetch_add_byte(unsigned char* x, unsigned char y) {
	return atomic_fetch_add_explicit((_Atomic(unsigned char)*)x, y, memory_order_seq_cst);
}
static inline unsigned char atomic_fetch_sub_byte(unsigned char* x, unsigned char y) {
	return atomic_fetch_sub_explicit((_Atomic(unsigned char)*)x, y, memory_order_seq_cst);
}
static inline unsigned char atomic_fetch_and_byte(unsigned char* x, unsigned char y) {
	return atomic_fetch_and_explicit((_Atomic(unsigned char)*)x, y, memory_order_seq_cst);
}
static inline unsigned char atomic_fetch_or_byte(unsigned char* x, unsigned char y) {
	return atomic_fetch_or_explicit((_Atomic(unsigned char)*)x, y, memory_order_seq_cst);
}
static inline unsigned char atomic_fetch_xor_byte(unsigned char* x, unsigned char y) {
	return atomic_fetch_xor_explicit((_Atomic(unsigned char)*)x, y, memory_order_seq_cst);
}

#endif
#endif
