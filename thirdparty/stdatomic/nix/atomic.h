/*
    Compatibility header for stdatomic.h that works for all compilers supported by V.
    For TCC, we use libatomic from the OS.
*/
#ifndef __ATOMIC_H
#define __ATOMIC_H

#ifndef __cplusplus
	// If C just use stdatomic.h
	#include <stdatomic.h>
#else
	// CPP wrapper for atomic operations that are compatible with C
	#include "atomic_cpp.h"
#endif

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
	atomic_thread_fence(memory_order_acquire);
	return atomic_load_explicit((_Atomic(unsigned char)*)x, memory_order_seq_cst);
}
static inline void atomic_store_byte(unsigned char* x, unsigned char y) {
	atomic_store_explicit((_Atomic(unsigned char)*)x, y, memory_order_seq_cst);
	atomic_thread_fence(memory_order_release);
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
