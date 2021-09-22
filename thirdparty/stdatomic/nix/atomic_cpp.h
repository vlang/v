// Source: https://android.googlesource.com/platform/bionic/+/lollipop-release/libc/include/stdatomic.h
/*-
 * Copyright (c) 2011 Ed Schouten <ed@FreeBSD.org>
 *                    David Chisnall <theraven@FreeBSD.org>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 * $FreeBSD$
 */
#ifndef _STDATOMIC_H_
#define _STDATOMIC_H_
#include <sys/cdefs.h>
#if defined(__cplusplus) && defined(_USING_LIBCXX)
#ifdef __clang__
#if __has_feature(cxx_atomic)
#define _STDATOMIC_HAVE_ATOMIC
#endif
#else /* gcc */
#if __GNUC_PREREQ(4, 7)
#define _STDATOMIC_HAVE_ATOMIC
#endif
#endif
#endif
#ifdef _STDATOMIC_HAVE_ATOMIC
/* We have a usable C++ <atomic>; use it instead.  */
#include <atomic>
#undef _Atomic
/* Also defined by <atomic> for gcc.  But not used in macros. */
/* Also a clang intrinsic.                                    */
/* Should not be used by client code before this file is      */
/* included.  The definitions in <atomic> themselves see      */
/* the old definition, as they should.                        */
/* Client code sees the following definition.                 */
#define _Atomic(t) std::atomic<t>
using std::atomic_bool;
using std::atomic_char;
using std::atomic_char16_t;
using std::atomic_char32_t;
using std::atomic_compare_exchange_strong;
using std::atomic_compare_exchange_strong_explicit;
using std::atomic_compare_exchange_weak;
using std::atomic_compare_exchange_weak_explicit;
using std::atomic_exchange;
using std::atomic_exchange_explicit;
using std::atomic_fetch_add;
using std::atomic_fetch_add_explicit;
using std::atomic_fetch_and;
using std::atomic_fetch_and_explicit;
using std::atomic_fetch_or;
using std::atomic_fetch_or_explicit;
using std::atomic_fetch_sub;
using std::atomic_fetch_sub_explicit;
using std::atomic_fetch_xor;
using std::atomic_fetch_xor_explicit;
using std::atomic_init;
using std::atomic_int;
using std::atomic_int_fast16_t;
using std::atomic_int_fast32_t;
using std::atomic_int_fast64_t;
using std::atomic_int_fast8_t;
using std::atomic_int_least16_t;
using std::atomic_int_least32_t;
using std::atomic_int_least64_t;
using std::atomic_int_least8_t;
using std::atomic_intmax_t;
using std::atomic_intptr_t;
using std::atomic_is_lock_free;
using std::atomic_llong;
using std::atomic_load;
using std::atomic_load_explicit;
using std::atomic_long;
using std::atomic_ptrdiff_t;
using std::atomic_schar;
using std::atomic_short;
using std::atomic_signal_fence;
using std::atomic_size_t;
using std::atomic_store;
using std::atomic_store_explicit;
using std::atomic_thread_fence;
using std::atomic_uchar;
using std::atomic_uint;
using std::atomic_uint_fast16_t;
using std::atomic_uint_fast32_t;
using std::atomic_uint_fast64_t;
using std::atomic_uint_fast8_t;
using std::atomic_uint_least16_t;
using std::atomic_uint_least32_t;
using std::atomic_uint_least64_t;
using std::atomic_uint_least8_t;
using std::atomic_uintmax_t;
using std::atomic_uintptr_t;
using std::atomic_ullong;
using std::atomic_ulong;
using std::atomic_ushort;
using std::atomic_wchar_t;
using std::memory_order;
using std::memory_order_acq_rel;
using std::memory_order_consume;
using std::memory_order_relaxed;
using std::memory_order_release;
using std::memory_order_seq_cst;
#else /* <atomic> unavailable, possibly because this is C, not C++ */
#include <sys/types.h>
#include <stdbool.h>
/*
 * C: Do it ourselves.
 * Note that the runtime representation defined here should be compatible
 * with the C++ one, i.e. an _Atomic(T) needs to contain the same
 * bits as a T.
 */
#include <stddef.h> /* For ptrdiff_t.                          */
#include <stdint.h> /* TODO: Should pollute namespace less.    */
#if __STDC_VERSION__ >= 201112L
#include <uchar.h> /* For char16_t and char32_t.              */
#endif
#ifdef __clang__
#if __has_extension(c_atomic) || __has_extension(cxx_atomic)
#define __CLANG_ATOMICS
#else
#error "stdatomic.h does not support your compiler"
#endif
#if __has_builtin(__sync_swap)
#define __HAS_BUILTIN_SYNC_SWAP
#endif
#else
#if __GNUC_PREREQ(4, 7)
#define __GNUC_ATOMICS
#else
#define __SYNC_ATOMICS
#ifdef __cplusplus
#define __ATOMICS_AVOID_DOT_INIT
#endif
#endif
#endif
/*
 * 7.17.1 Atomic lock-free macros.
 */
#ifdef __GCC_ATOMIC_BOOL_LOCK_FREE
#define ATOMIC_BOOL_LOCK_FREE __GCC_ATOMIC_BOOL_LOCK_FREE
#elif defined(__SYNC_ATOMICS)
#define ATOMIC_BOOL_LOCK_FREE 2 /* For all modern platforms */
#endif
#ifdef __GCC_ATOMIC_CHAR_LOCK_FREE
#define ATOMIC_CHAR_LOCK_FREE __GCC_ATOMIC_CHAR_LOCK_FREE
#elif defined(__SYNC_ATOMICS)
#define ATOMIC_CHAR_LOCK_FREE 2
#endif
#ifdef __GCC_ATOMIC_CHAR16_T_LOCK_FREE
#define ATOMIC_CHAR16_T_LOCK_FREE __GCC_ATOMIC_CHAR16_T_LOCK_FREE
#elif defined(__SYNC_ATOMICS)
#define ATOMIC_CHAR16_T_LOCK_FREE 2
#endif
#ifdef __GCC_ATOMIC_CHAR32_T_LOCK_FREE
#define ATOMIC_CHAR32_T_LOCK_FREE __GCC_ATOMIC_CHAR32_T_LOCK_FREE
#elif defined(__SYNC_ATOMICS)
#define ATOMIC_CHAR32_T_LOCK_FREE 2
#endif
#ifdef __GCC_ATOMIC_WCHAR_T_LOCK_FREE
#define ATOMIC_WCHAR_T_LOCK_FREE __GCC_ATOMIC_WCHAR_T_LOCK_FREE
#elif defined(__SYNC_ATOMICS)
#define ATOMIC_WCHAR_T_LOCK_FREE 2
#endif
#ifdef __GCC_ATOMIC_SHORT_LOCK_FREE
#define ATOMIC_SHORT_LOCK_FREE __GCC_ATOMIC_SHORT_LOCK_FREE
#elif defined(__SYNC_ATOMICS)
#define ATOMIC_SHORT_LOCK_FREE 2
#endif
#ifdef __GCC_ATOMIC_INT_LOCK_FREE
#define ATOMIC_INT_LOCK_FREE __GCC_ATOMIC_INT_LOCK_FREE
#elif defined(__SYNC_ATOMICS)
#define ATOMIC_INT_LOCK_FREE 2
#endif
#ifdef __GCC_ATOMIC_LONG_LOCK_FREE
#define ATOMIC_LONG_LOCK_FREE __GCC_ATOMIC_LONG_LOCK_FREE
#elif defined(__SYNC_ATOMICS)
#define ATOMIC_LONG_LOCK_FREE 2
#endif
#ifdef __GCC_ATOMIC_LLONG_LOCK_FREE
#define ATOMIC_LLONG_LOCK_FREE __GCC_ATOMIC_LLONG_LOCK_FREE
#elif defined(__SYNC_ATOMICS)
#define ATOMIC_LLONG_LOCK_FREE 1 /* maybe */
#endif
#ifdef __GCC_ATOMIC_POINTER_LOCK_FREE
#define ATOMIC_POINTER_LOCK_FREE __GCC_ATOMIC_POINTER_LOCK_FREE
#elif defined(__SYNC_ATOMICS)
#define ATOMIC_POINTER_LOCK_FREE 2
#endif
/*
 * 7.17.2 Initialization.
 */
#if defined(__CLANG_ATOMICS)
#define ATOMIC_VAR_INIT(value) (value)
#define atomic_init(obj, value) __c11_atomic_init(obj, value)
#else
#ifdef __ATOMICS_AVOID_DOT_INIT
#define ATOMIC_VAR_INIT(value) \
    {                          \
        value                  \
    }
#else
#define ATOMIC_VAR_INIT(value) \
    {                          \
        .__val = (value)       \
    }
#endif
#define atomic_init(obj, value) ((void)((obj)->__val = (value)))
#endif
/*
 * Clang and recent GCC both provide predefined macros for the memory
 * orderings.  If we are using a compiler that doesn't define them, use the
 * clang values - these will be ignored in the fallback path.
 */
#ifndef __ATOMIC_RELAXED
#define __ATOMIC_RELAXED 0
#endif
#ifndef __ATOMIC_CONSUME
#define __ATOMIC_CONSUME 1
#endif
#ifndef __ATOMIC_ACQUIRE
#define __ATOMIC_ACQUIRE 2
#endif
#ifndef __ATOMIC_RELEASE
#define __ATOMIC_RELEASE 3
#endif
#ifndef __ATOMIC_ACQ_REL
#define __ATOMIC_ACQ_REL 4
#endif
#ifndef __ATOMIC_SEQ_CST
#define __ATOMIC_SEQ_CST 5
#endif
/*
 * 7.17.3 Order and consistency.
 *
 * The memory_order_* constants that denote the barrier behaviour of the
 * atomic operations.
 * The enum values must be identical to those used by the
 * C++ <atomic> header.
 */
typedef enum
{
    memory_order_relaxed = __ATOMIC_RELAXED,
    memory_order_consume = __ATOMIC_CONSUME,
    memory_order_acquire = __ATOMIC_ACQUIRE,
    memory_order_release = __ATOMIC_RELEASE,
    memory_order_acq_rel = __ATOMIC_ACQ_REL,
    memory_order_seq_cst = __ATOMIC_SEQ_CST
} memory_order;
/*
 * 7.17.4 Fences.
 */
static __inline void
atomic_thread_fence(memory_order __order __attribute__((unused)))
{
#ifdef __CLANG_ATOMICS
    __c11_atomic_thread_fence(__order);
#elif defined(__GNUC_ATOMICS)
    __atomic_thread_fence(__order);
#else
    __sync_synchronize();
#endif
}
static __inline void
atomic_signal_fence(memory_order __order __attribute__((unused)))
{
#ifdef __CLANG_ATOMICS
    __c11_atomic_signal_fence(__order);
#elif defined(__GNUC_ATOMICS)
    __atomic_signal_fence(__order);
#else
    __asm volatile("" ::
                       : "memory");
#endif
}
/*
 * 7.17.5 Lock-free property.
 */
#if defined(_KERNEL)
/* Atomics in kernelspace are always lock-free. */
#define atomic_is_lock_free(obj) \
    ((void)(obj), (_Bool)1)
#elif defined(__CLANG_ATOMICS)
#define atomic_is_lock_free(obj) \
    __c11_atomic_is_lock_free(sizeof(*(obj)))
#elif defined(__GNUC_ATOMICS)
#define atomic_is_lock_free(obj) \
    __atomic_is_lock_free(sizeof((obj)->__val), &(obj)->__val)
#else
#define atomic_is_lock_free(obj) \
    ((void)(obj), sizeof((obj)->__val) <= sizeof(void *))
#endif
/*
 * 7.17.6 Atomic integer types.
 */
#ifndef __CLANG_ATOMICS
/*
 * No native support for _Atomic(). Place object in structure to prevent
 * most forms of direct non-atomic access.
 */
#define _Atomic(T)        \
    struct                \
    {                     \
        T volatile __val; \
    }
#endif
typedef _Atomic(bool) atomic_bool;
typedef _Atomic(char) atomic_char;
typedef _Atomic(signed char) atomic_schar;
typedef _Atomic(unsigned char) atomic_uchar;
typedef _Atomic(short) atomic_short;
typedef _Atomic(unsigned short) atomic_ushort;
typedef _Atomic(int) atomic_int;
typedef _Atomic(unsigned int) atomic_uint;
typedef _Atomic(long) atomic_long;
typedef _Atomic(unsigned long) atomic_ulong;
typedef _Atomic(long long) atomic_llong;
typedef _Atomic(unsigned long long) atomic_ullong;
#if __STDC_VERSION__ >= 201112L || __cplusplus >= 201103L
typedef _Atomic(char16_t) atomic_char16_t;
typedef _Atomic(char32_t) atomic_char32_t;
#endif
typedef _Atomic(wchar_t) atomic_wchar_t;
typedef _Atomic(int_least8_t) atomic_int_least8_t;
typedef _Atomic(uint_least8_t) atomic_uint_least8_t;
typedef _Atomic(int_least16_t) atomic_int_least16_t;
typedef _Atomic(uint_least16_t) atomic_uint_least16_t;
typedef _Atomic(int_least32_t) atomic_int_least32_t;
typedef _Atomic(uint_least32_t) atomic_uint_least32_t;
typedef _Atomic(int_least64_t) atomic_int_least64_t;
typedef _Atomic(uint_least64_t) atomic_uint_least64_t;
typedef _Atomic(int_fast8_t) atomic_int_fast8_t;
typedef _Atomic(uint_fast8_t) atomic_uint_fast8_t;
typedef _Atomic(int_fast16_t) atomic_int_fast16_t;
typedef _Atomic(uint_fast16_t) atomic_uint_fast16_t;
typedef _Atomic(int_fast32_t) atomic_int_fast32_t;
typedef _Atomic(uint_fast32_t) atomic_uint_fast32_t;
typedef _Atomic(int_fast64_t) atomic_int_fast64_t;
typedef _Atomic(uint_fast64_t) atomic_uint_fast64_t;
typedef _Atomic(intptr_t) atomic_intptr_t;
typedef _Atomic(uintptr_t) atomic_uintptr_t;
typedef _Atomic(size_t) atomic_size_t;
typedef _Atomic(ptrdiff_t) atomic_ptrdiff_t;
typedef _Atomic(intmax_t) atomic_intmax_t;
typedef _Atomic(uintmax_t) atomic_uintmax_t;
/*
 * 7.17.7 Operations on atomic types.
 */
/*
 * Compiler-specific operations.
 */
#if defined(__CLANG_ATOMICS)
#define atomic_compare_exchange_strong_explicit(object, expected,          \
                                                desired, success, failure) \
    __c11_atomic_compare_exchange_strong(object, expected, desired,        \
                                         success, failure)
#define atomic_compare_exchange_weak_explicit(object, expected,          \
                                              desired, success, failure) \
    __c11_atomic_compare_exchange_weak(object, expected, desired,        \
                                       success, failure)
#define atomic_exchange_explicit(object, desired, order) \
    __c11_atomic_exchange(object, desired, order)
#define atomic_fetch_add_explicit(object, operand, order) \
    __c11_atomic_fetch_add(object, operand, order)
#define atomic_fetch_and_explicit(object, operand, order) \
    __c11_atomic_fetch_and(object, operand, order)
#define atomic_fetch_or_explicit(object, operand, order) \
    __c11_atomic_fetch_or(object, operand, order)
#define atomic_fetch_sub_explicit(object, operand, order) \
    __c11_atomic_fetch_sub(object, operand, order)
#define atomic_fetch_xor_explicit(object, operand, order) \
    __c11_atomic_fetch_xor(object, operand, order)
#define atomic_load_explicit(object, order) \
    __c11_atomic_load(object, order)
#define atomic_store_explicit(object, desired, order) \
    __c11_atomic_store(object, desired, order)
#elif defined(__GNUC_ATOMICS)
#define atomic_compare_exchange_strong_explicit(object, expected,          \
                                                desired, success, failure) \
    __atomic_compare_exchange_n(&(object)->__val, expected,                \
                                desired, 0, success, failure)
#define atomic_compare_exchange_weak_explicit(object, expected,          \
                                              desired, success, failure) \
    __atomic_compare_exchange_n(&(object)->__val, expected,              \
                                desired, 1, success, failure)
#define atomic_exchange_explicit(object, desired, order) \
    __atomic_exchange_n(&(object)->__val, desired, order)
#define atomic_fetch_add_explicit(object, operand, order) \
    __atomic_fetch_add(&(object)->__val, operand, order)
#define atomic_fetch_and_explicit(object, operand, order) \
    __atomic_fetch_and(&(object)->__val, operand, order)
#define atomic_fetch_or_explicit(object, operand, order) \
    __atomic_fetch_or(&(object)->__val, operand, order)
#define atomic_fetch_sub_explicit(object, operand, order) \
    __atomic_fetch_sub(&(object)->__val, operand, order)
#define atomic_fetch_xor_explicit(object, operand, order) \
    __atomic_fetch_xor(&(object)->__val, operand, order)
#define atomic_load_explicit(object, order) \
    __atomic_load_n(&(object)->__val, order)
#define atomic_store_explicit(object, desired, order) \
    __atomic_store_n(&(object)->__val, desired, order)
#else
#define __atomic_apply_stride(object, operand) \
    (((__typeof__((object)->__val))0) + (operand))
#define atomic_compare_exchange_strong_explicit(object, expected,                          \
                                                desired, success, failure) __extension__({ \
    __typeof__(expected) __ep = (expected);                                                \
    __typeof__(*__ep) __e = *__ep;                                                         \
    (void)(success);                                                                       \
    (void)(failure);                                                                       \
    (bool)((*__ep = __sync_val_compare_and_swap(&(object)->__val,                          \
                                                __e, desired)) == __e);                    \
})
#define atomic_compare_exchange_weak_explicit(object, expected,          \
                                              desired, success, failure) \
    atomic_compare_exchange_strong_explicit(object, expected,            \
                                            desired, success, failure)
#ifdef __HAS_BUILTIN_SYNC_SWAP
/* Clang provides a full-barrier atomic exchange - use it if available. */
#define atomic_exchange_explicit(object, desired, order) \
    ((void)(order), __sync_swap(&(object)->__val, desired))
#else
/*
 * __sync_lock_test_and_set() is only an acquire barrier in theory (although in
 * practice it is usually a full barrier) so we need an explicit barrier before
 * it.
 */
#define atomic_exchange_explicit(object, desired, order) \
    __extension__({                                      \
        __typeof__(object) __o = (object);               \
        __typeof__(desired) __d = (desired);             \
        (void)(order);                                   \
        __sync_synchronize();                            \
        __sync_lock_test_and_set(&(__o)->__val, __d);    \
    })
#endif
#define atomic_fetch_add_explicit(object, operand, order)  \
    ((void)(order), __sync_fetch_and_add(&(object)->__val, \
                                         __atomic_apply_stride(object, operand)))
#define atomic_fetch_and_explicit(object, operand, order) \
    ((void)(order), __sync_fetch_and_and(&(object)->__val, operand))
#define atomic_fetch_or_explicit(object, operand, order) \
    ((void)(order), __sync_fetch_and_or(&(object)->__val, operand))
#define atomic_fetch_sub_explicit(object, operand, order)  \
    ((void)(order), __sync_fetch_and_sub(&(object)->__val, \
                                         __atomic_apply_stride(object, operand)))
#define atomic_fetch_xor_explicit(object, operand, order) \
    ((void)(order), __sync_fetch_and_xor(&(object)->__val, operand))
#define atomic_load_explicit(object, order) \
    ((void)(order), __sync_fetch_and_add(&(object)->__val, 0))
#define atomic_store_explicit(object, desired, order) \
    ((void)atomic_exchange_explicit(object, desired, order))
#endif
/*
 * Convenience functions.
 *
 * Don't provide these in kernel space. In kernel space, we should be
 * disciplined enough to always provide explicit barriers.
 */
#ifndef _KERNEL
#define atomic_compare_exchange_strong(object, expected, desired) \
    atomic_compare_exchange_strong_explicit(object, expected,     \
                                            desired, memory_order_seq_cst, memory_order_seq_cst)
#define atomic_compare_exchange_weak(object, expected, desired) \
    atomic_compare_exchange_weak_explicit(object, expected,     \
                                          desired, memory_order_seq_cst, memory_order_seq_cst)
#define atomic_exchange(object, desired) \
    atomic_exchange_explicit(object, desired, memory_order_seq_cst)
#define atomic_fetch_add(object, operand) \
    atomic_fetch_add_explicit(object, operand, memory_order_seq_cst)
#define atomic_fetch_and(object, operand) \
    atomic_fetch_and_explicit(object, operand, memory_order_seq_cst)
#define atomic_fetch_or(object, operand) \
    atomic_fetch_or_explicit(object, operand, memory_order_seq_cst)
#define atomic_fetch_sub(object, operand) \
    atomic_fetch_sub_explicit(object, operand, memory_order_seq_cst)
#define atomic_fetch_xor(object, operand) \
    atomic_fetch_xor_explicit(object, operand, memory_order_seq_cst)
#define atomic_load(object) \
    atomic_load_explicit(object, memory_order_seq_cst)
#define atomic_store(object, desired) \
    atomic_store_explicit(object, desired, memory_order_seq_cst)
#endif /* !_KERNEL */
/*
 * 7.17.8 Atomic flag type and operations.
 *
 * XXX: Assume atomic_bool can be used as an atomic_flag. Is there some
 * kind of compiler built-in type we could use?
 */
typedef struct
{
    atomic_bool __flag;
} atomic_flag;
#define ATOMIC_FLAG_INIT       \
    {                          \
        ATOMIC_VAR_INIT(false) \
    }
static __inline bool
atomic_flag_test_and_set_explicit(volatile atomic_flag *__object,
                                  memory_order __order)
{
    return (atomic_exchange_explicit(&__object->__flag, 1, __order));
}
static __inline void
atomic_flag_clear_explicit(volatile atomic_flag *__object, memory_order __order)
{
    atomic_store_explicit(&__object->__flag, 0, __order);
}
#ifndef _KERNEL
static __inline bool
atomic_flag_test_and_set(volatile atomic_flag *__object)
{
    return (atomic_flag_test_and_set_explicit(__object,
                                              memory_order_seq_cst));
}
static __inline void
atomic_flag_clear(volatile atomic_flag *__object)
{
    atomic_flag_clear_explicit(__object, memory_order_seq_cst);
}
#endif /* !_KERNEL */
#endif /* <atomic> unavailable */
#endif /* !_STDATOMIC_H_ */
