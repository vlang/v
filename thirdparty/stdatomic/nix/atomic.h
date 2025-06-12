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

#if defined(__x86_64__) || defined(_M_X64) || defined(__i386__) || defined(_M_IX86)
    /* x86 architecture: uses PAUSE instruction for efficient spinning */
    #define cpu_relax() __asm__ __volatile__ ("pause")
#elif defined(__aarch64__) || defined(_M_ARM64) || defined(__arm__) || defined(_M_ARM)
    #if defined(__TINYC__)
        /* TCC compiler limitation: assembly not supported on ARM */
        #define cpu_relax()
    #else
        /* ARM architecture: uses YIELD instruction for power-efficient spinning */
        #define cpu_relax() __asm__ __volatile__ ("yield" ::: "memory")
    #endif
#elif defined(__riscv) && __riscv_xlen == 64
    /* RISC-V 64-bit: no dedicated pause instruction, using alternative sequence */
    #define cpu_relax() __asm__ __volatile__ ( \
        "fence rw, rw\n\t"   /* Full memory barrier (read-write ordering) */ \
        "andi a0, a0, 0\n\t" /* Dummy arithmetic instruction (always sets a0 = 0) */ \
        ::: "memory", "a0")  /* Clobbers memory and a0 register to prevent optimizations */
#elif defined(__powerpc64__) || defined(__ppc64__)
    /* PowerPC 64-bit: use OR instruction for synchronization */
    #define cpu_relax() __asm__ __volatile__ ("or 1,1,1\n\t" ::: "memory")
#elif defined(__mips64)
    /* MIPS 64-bit: use series of super-scalar NOPs */
    #define cpu_relax() __asm__ __volatile__ ("ssnop\n\tssnop\n\tssnop\n\t" ::: "memory")
#else
    /* Fallback implementation for unsupported architectures */
    #define cpu_relax() __asm__ __volatile__ ( \
        "nop\n\t" "nop\n\t" "nop\n\t" "nop\n\t" /* Series of no-operation instructions */ \
        ::: "memory") /* Memory clobber to prevent instruction reordering */
#endif

#ifdef __TINYC__

typedef volatile long long atomic_llong;
typedef volatile unsigned long long atomic_ullong;
typedef volatile uintptr_t atomic_uintptr_t;

extern void atomic_thread_fence (int memory_order);
extern void __atomic_thread_fence (int memory_order);
#define atomic_thread_fence(order) __atomic_thread_fence (order)

// use functions for 64, 32 and 8 bit from libatomic directly
// since tcc is not capible to use "generic" C functions
// there is no header file for libatomic so we provide function declarations here

extern unsigned long long __atomic_load_8(unsigned long long* x, int mo);
extern void __atomic_store_8(unsigned long long* x, unsigned long long y, int mo);
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
extern unsigned int __atomic_exchange_4(unsigned int* x, unsigned int y, int mo);
extern unsigned int __atomic_fetch_add_4(unsigned int* x, unsigned int y, int mo);
extern unsigned int __atomic_fetch_sub_4(unsigned int* x, unsigned int y, int mo);
extern unsigned int __atomic_fetch_and_4(unsigned int* x, unsigned int y, int mo);
extern unsigned int __atomic_fetch_or_4(unsigned int* x, unsigned int y, int mo);
extern unsigned int __atomic_fetch_xor_4(unsigned int* x, unsigned int y, int mo);

extern unsigned short __atomic_load_2(unsigned short* x, int mo);
extern void __atomic_store_2(unsigned short* x, unsigned short y, int mo);
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
static inline int atomic_compare_exchange_weak(void** x, void** expected, intptr_t y) {
	return (int)atomic_compare_exchange_weak_explicit((unsigned long long*)x, (unsigned long long*)expected, (uintptr_t)y, memory_order_seq_cst, memory_order_seq_cst);
}
static inline int atomic_compare_exchange_strong(void** x,  void** expected, intptr_t y) {
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

#ifdef __aarch64__
// must has an `extern` to link with libatomic.a

// acq_rel version
extern inline _Bool __aarch64_cas1_acq_rel(unsigned char*ptr, unsigned char*expected, unsigned char desired) {
    return __atomic_compare_exchange_1(
        ptr,
        expected,
        desired,
		memory_order_acq_rel,
		memory_order_acquire
    );
}

extern inline _Bool __aarch64_cas2_acq_rel(unsigned short*ptr, unsigned short*expected, unsigned short desired) {
    return __atomic_compare_exchange_2(
        ptr,
        expected,
        desired,
		memory_order_acq_rel,
		memory_order_acquire
    );
}

extern inline _Bool __aarch64_cas4_acq_rel(unsigned int*ptr, unsigned int*expected, unsigned int desired) {
    return __atomic_compare_exchange_4(
        ptr,
        expected,
        desired,
        memory_order_acq_rel,
        memory_order_acquire
    );
}

extern inline _Bool __aarch64_cas8_acq_rel(unsigned long long*ptr, unsigned long long*expected, unsigned long long desired) {
    return __atomic_compare_exchange_8(
        ptr,
        expected,
        desired,
        memory_order_acq_rel,
        memory_order_acquire
    );
}

extern inline char __aarch64_ldadd1_acq_rel(char*ptr, char value) {
    return __atomic_fetch_add_1(
        (unsigned char*)ptr,
        (unsigned char)value,
        memory_order_acq_rel
    );
}

extern inline short __aarch64_ldadd2_acq_rel(short*ptr, short value) {
    return __atomic_fetch_add_2(
        (unsigned short*)ptr,
        (unsigned short)value,
        memory_order_acq_rel
    );
}

extern inline int __aarch64_ldadd4_acq_rel(int*ptr, int value) {
    return __atomic_fetch_add_4(
        (unsigned int*)ptr,
        (unsigned int)value,
        memory_order_acq_rel
    );
}

extern inline long long __aarch64_ldadd8_acq_rel(long long*ptr, long long value) {
    return __atomic_fetch_add_8(
        (unsigned long long*)ptr,
        (unsigned long long)value,
        memory_order_acq_rel
    );
}

extern inline unsigned char __aarch64_swp1_acq_rel(unsigned char*ptr, unsigned char newval) {
    return __atomic_exchange_1(
        ptr,
        newval,
        memory_order_acq_rel
    );
}

extern inline unsigned short __aarch64_swp2_acq_rel(unsigned short*ptr, unsigned short newval) {
    return __atomic_exchange_2(
        ptr,
        newval,
        memory_order_acq_rel
    );
}

extern inline unsigned int __aarch64_swp4_acq_rel(unsigned int*ptr, unsigned int newval) {
    return __atomic_exchange_4(
        ptr,
        newval,
        memory_order_acq_rel
    );
}

extern inline unsigned long long __aarch64_swp8_acq_rel(unsigned long long*ptr, unsigned long long newval) {
    return __atomic_exchange_8(
        ptr,
        newval,
        memory_order_acq_rel
    );
}

extern inline unsigned char __aarch64_ldclr1_acq_rel(unsigned char*ptr, unsigned char mask) {
    return __atomic_fetch_and_1(
        ptr,
        ~mask,
        memory_order_acq_rel
    );
}

extern inline unsigned short __aarch64_ldclr2_acq_rel(unsigned short*ptr, unsigned short mask) {
    return __atomic_fetch_and_2(
        ptr,
        ~mask,
        memory_order_acq_rel
    );
}

extern inline unsigned int __aarch64_ldclr4_acq_rel(unsigned int*ptr, unsigned int mask) {
    return __atomic_fetch_and_4(
        ptr,
        ~mask,
        memory_order_acq_rel
    );
}

extern inline unsigned long long __aarch64_ldclr8_acq_rel(unsigned long long*ptr, unsigned long long mask) {
    return __atomic_fetch_and_8(
        ptr,
        ~mask,
        memory_order_acq_rel
    );
}

extern inline unsigned char __aarch64_ldset1_acq_rel(unsigned char*ptr, unsigned char mask) {
    return __atomic_fetch_or_1(
        ptr,
        mask,
        memory_order_acq_rel
    );
}

extern inline unsigned short __aarch64_ldset2_acq_rel(unsigned short*ptr, unsigned short mask) {
    return __atomic_fetch_or_2(
        ptr,
        mask,
        memory_order_acq_rel
    );
}

extern inline unsigned int __aarch64_ldset4_acq_rel(unsigned int*ptr, unsigned int mask) {
    return __atomic_fetch_or_4(
        ptr,
        mask,
        memory_order_acq_rel
    );
}

extern inline unsigned long long __aarch64_ldset8_acq_rel(unsigned long long*ptr, unsigned long long mask) {
    return __atomic_fetch_or_8(
        ptr,
        mask,
        memory_order_acq_rel
    );
}

extern inline unsigned char __aarch64_ldeor1_acq_rel(unsigned char*ptr, unsigned char mask) {
    return __atomic_fetch_xor_1(
        ptr,
        mask,
        memory_order_acq_rel
    );
}

extern inline unsigned short __aarch64_ldeor2_acq_rel(unsigned short*ptr, unsigned short mask) {
    return __atomic_fetch_xor_2(
        ptr,
        mask,
        memory_order_acq_rel
    );
}

extern inline unsigned int __aarch64_ldeor4_acq_rel(unsigned int*ptr, unsigned int mask) {
    return __atomic_fetch_xor_4(
        ptr,
        mask,
        memory_order_acq_rel
    );
}

extern inline unsigned long long __aarch64_ldeor8_acq_rel(unsigned long long*ptr, unsigned long long mask) {
    return __atomic_fetch_xor_8(
        ptr,
        mask,
        memory_order_acq_rel
    );
}

#define aarch64_cas_acq_rel(ptr, expected, desired)      \
    _Generic((ptr),                                      \
        char*:         __aarch64_cas1_acq_rel,  \
        short*:        __aarch64_cas2_acq_rel,  \
        int*:          __aarch64_cas4_acq_rel,  \
        long long*:    __aarch64_cas8_acq_rel   \
    )(ptr, expected, desired)

// relax version
extern inline _Bool __aarch64_cas1_relax(unsigned char*ptr, unsigned char*expected, unsigned char desired) {
    return __atomic_compare_exchange_1(
        ptr,
        expected,
        desired,
		memory_order_relaxed,
		memory_order_relaxed
    );
}

extern inline _Bool __aarch64_cas2_relax(unsigned short*ptr, unsigned short*expected, unsigned short desired) {
    return __atomic_compare_exchange_2(
        ptr,
        expected,
        desired,
		memory_order_relaxed,
		memory_order_relaxed
    );
}

extern inline _Bool __aarch64_cas4_relax(unsigned int*ptr, unsigned int*expected, unsigned int desired) {
    return __atomic_compare_exchange_4(
        ptr,
        expected,
        desired,
        memory_order_relaxed,
        memory_order_relaxed
    );
}

extern inline _Bool __aarch64_cas8_relax(unsigned long long*ptr, unsigned long long*expected, unsigned long long desired) {
    return __atomic_compare_exchange_8(
        ptr,
        expected,
        desired,
        memory_order_relaxed,
        memory_order_relaxed
    );
}

extern inline char __aarch64_ldadd1_relax(char*ptr, char value) {
    return __atomic_fetch_add_1(
        (unsigned char*)ptr,
        (unsigned char)value,
        memory_order_relaxed
    );
}

extern inline short __aarch64_ldadd2_relax(short*ptr, short value) {
    return __atomic_fetch_add_2(
        (unsigned short*)ptr,
        (unsigned short)value,
        memory_order_relaxed
    );
}

extern inline int __aarch64_ldadd4_relax(int*ptr, int value) {
    return __atomic_fetch_add_4(
        (unsigned int*)ptr,
        (unsigned int)value,
        memory_order_relaxed
    );
}

extern inline long long __aarch64_ldadd8_relax(long long*ptr, long long value) {
    return __atomic_fetch_add_8(
        (unsigned long long*)ptr,
        (unsigned long long)value,
        memory_order_relaxed
    );
}

extern inline unsigned char __aarch64_swp1_relax(unsigned char*ptr, unsigned char newval) {
    return __atomic_exchange_1(
        ptr,
        newval,
        memory_order_relaxed
    );
}

extern inline unsigned short __aarch64_swp2_relax(unsigned short*ptr, unsigned short newval) {
    return __atomic_exchange_2(
        ptr,
        newval,
        memory_order_relaxed
    );
}

extern inline unsigned int __aarch64_swp4_relax(unsigned int*ptr, unsigned int newval) {
    return __atomic_exchange_4(
        ptr,
        newval,
        memory_order_relaxed
    );
}

extern inline unsigned long long __aarch64_swp8_relax(unsigned long long*ptr, unsigned long long newval) {
    return __atomic_exchange_8(
        ptr,
        newval,
        memory_order_relaxed
    );
}

extern inline unsigned char __aarch64_ldclr1_relax(unsigned char*ptr, unsigned char mask) {
    return __atomic_fetch_and_1(
        ptr,
        ~mask,
        memory_order_relaxed
    );
}

extern inline unsigned short __aarch64_ldclr2_relax(unsigned short*ptr, unsigned short mask) {
    return __atomic_fetch_and_2(
        ptr,
        ~mask,
        memory_order_relaxed
    );
}

extern inline unsigned int __aarch64_ldclr4_relax(unsigned int*ptr, unsigned int mask) {
    return __atomic_fetch_and_4(
        ptr,
        ~mask,
        memory_order_relaxed
    );
}

extern inline unsigned long long __aarch64_ldclr8_relax(unsigned long long*ptr, unsigned long long mask) {
    return __atomic_fetch_and_8(
        ptr,
        ~mask,
        memory_order_relaxed
    );
}

extern inline unsigned char __aarch64_ldset1_relax(unsigned char*ptr, unsigned char mask) {
    return __atomic_fetch_or_1(
        ptr,
        mask,
        memory_order_relaxed
    );
}

extern inline unsigned short __aarch64_ldset2_relax(unsigned short*ptr, unsigned short mask) {
    return __atomic_fetch_or_2(
        ptr,
        mask,
        memory_order_relaxed
    );
}

extern inline unsigned int __aarch64_ldset4_relax(unsigned int*ptr, unsigned int mask) {
    return __atomic_fetch_or_4(
        ptr,
        mask,
        memory_order_relaxed
    );
}

extern inline unsigned long long __aarch64_ldset8_relax(unsigned long long*ptr, unsigned long long mask) {
    return __atomic_fetch_or_8(
        ptr,
        mask,
        memory_order_relaxed
    );
}

extern inline unsigned char __aarch64_ldeor1_relax(unsigned char*ptr, unsigned char mask) {
    return __atomic_fetch_xor_1(
        ptr,
        mask,
        memory_order_relaxed
    );
}

extern inline unsigned short __aarch64_ldeor2_relax(unsigned short*ptr, unsigned short mask) {
    return __atomic_fetch_xor_2(
        ptr,
        mask,
        memory_order_relaxed
    );
}

extern inline unsigned int __aarch64_ldeor4_relax(unsigned int*ptr, unsigned int mask) {
    return __atomic_fetch_xor_4(
        ptr,
        mask,
        memory_order_relaxed
    );
}

extern inline unsigned long long __aarch64_ldeor8_relax(unsigned long long*ptr, unsigned long long mask) {
    return __atomic_fetch_xor_8(
        ptr,
        mask,
        memory_order_relaxed
    );
}

#define aarch64_cas_relax(ptr, expected, desired)      \
    _Generic((ptr),                                      \
        char*:         __aarch64_cas1_relax,  \
        short*:        __aarch64_cas2_relax,  \
        int*:          __aarch64_cas4_relax,  \
        long long*:    __aarch64_cas8_relax   \
    )(ptr, expected, desired)

#endif // __aarch64__

#else

// Since V might be confused with "generic" C functions either we provide special versions
// for gcc/clang, too
static inline unsigned long long atomic_load_u64(uint64_t* x) {
	return atomic_load_explicit((_Atomic (uint64_t)*)x, memory_order_seq_cst);
}
static inline void atomic_store_u64(uint64_t* x, uint64_t y) {
	atomic_store_explicit((_Atomic(uint64_t)*)x, y, memory_order_seq_cst);
}
static inline int atomic_compare_exchange_weak_u64(uint64_t* x, uint64_t* expected, uint64_t y) {
	return (int)atomic_compare_exchange_weak_explicit((_Atomic(uint64_t)*)x, expected, y, memory_order_seq_cst, memory_order_seq_cst);
}
static inline int atomic_compare_exchange_strong_u64(uint64_t* x, uint64_t* expected, uint64_t y) {
	return (int)atomic_compare_exchange_strong_explicit((_Atomic(uint64_t)*)x, expected, y, memory_order_seq_cst, memory_order_seq_cst);
}
static inline unsigned long long atomic_exchange_u64(uint64_t* x, uint64_t y) {
	return atomic_exchange_explicit((_Atomic(uint64_t)*)x, y, memory_order_seq_cst);
}
static inline unsigned long long atomic_fetch_add_u64(uint64_t* x, uint64_t y) {
	return atomic_fetch_add_explicit((_Atomic(uint64_t)*)x, y, memory_order_seq_cst);
}
static inline unsigned long long atomic_fetch_sub_u64(uint64_t* x, uint64_t y) {
	return atomic_fetch_sub_explicit((_Atomic(uint64_t)*)x, y, memory_order_seq_cst);
}
static inline unsigned long long atomic_fetch_and_u64(uint64_t* x, uint64_t y) {
	return atomic_fetch_and_explicit((_Atomic(uint64_t)*)x, y, memory_order_seq_cst);
}
static inline unsigned long long atomic_fetch_or_u64(uint64_t* x, uint64_t y) {
	return atomic_fetch_or_explicit((_Atomic(uint64_t)*)x, y, memory_order_seq_cst);
}
static inline unsigned long long atomic_fetch_xor_u64(uint64_t* x, uint64_t y) {
	return atomic_fetch_xor_explicit((_Atomic(uint64_t)*)x, y, memory_order_seq_cst);
}


static inline void* atomic_load_ptr(void** x) {
	return (void*)atomic_load_explicit((_Atomic(uintptr_t)*)x, memory_order_seq_cst);
}
static inline void atomic_store_ptr(void** x, void* y) {
	atomic_store_explicit((_Atomic(uintptr_t)*)x, (uintptr_t)y, memory_order_seq_cst);
}
static inline int atomic_compare_exchange_weak_ptr(void** x, void** expected, intptr_t y) {
	return (int)atomic_compare_exchange_weak_explicit((_Atomic(uintptr_t)*)x, (unsigned long *)expected, (uintptr_t)y, memory_order_seq_cst, memory_order_seq_cst);
}
static inline int atomic_compare_exchange_strong_ptr(void** x, void** expected, intptr_t y) {
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
