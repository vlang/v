/*
     * This file is part of FFmpeg.
     *
     * FFmpeg is free software; you can redistribute it and/or
     * modify it under the terms of the GNU Lesser General Public
     * License as published by the Free Software Foundation; either
     * version 2.1 of the License, or (at your option) any later version.
     *
     * FFmpeg is distributed in the hope that it will be useful,
     * but WITHOUT ANY WARRANTY; without even the implied warranty of
     * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
     * Lesser General Public License for more details.
     *
     * You should have received a copy of the GNU Lesser General Public
     * License along with FFmpeg; if not, write to the Free Software
     * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
     */

#ifndef COMPAT_ATOMICS_WIN32_STDATOMIC_H
#define COMPAT_ATOMICS_WIN32_STDATOMIC_H

#define WIN32_LEAN_AND_MEAN
#include <stddef.h>
#include <stdint.h>
#include <windows.h>

#ifdef _MSC_VER
#define cpu_relax() _mm_pause()
#else
#define cpu_relax() __asm__ __volatile__ ("pause")
#endif

#define ATOMIC_FLAG_INIT 0

#define ATOMIC_VAR_INIT(value) (value)

#define atomic_init(obj, value) \
    do                          \
    {                           \
        *(obj) = (value);       \
    } while (0)

#define kill_dependency(y) ((void)0)

// memory order policies - we use "sequentially consistent" by default

#define memory_order_relaxed 0
#define memory_order_consume 1
#define memory_order_acquire 2
#define memory_order_release 3
#define memory_order_acq_rel 4
#define memory_order_seq_cst 5

#ifdef _MSC_VER
#define atomic_thread_fence(order) \
    do { \
        switch (order) { \
            case memory_order_release: \
                _WriteBarrier(); \
                _ReadWriteBarrier(); \
                break; \
            case memory_order_acquire: \
                _ReadBarrier(); \
                _ReadWriteBarrier(); \
                break; \
            case memory_order_acq_rel: \
                _ReadBarrier(); \
                _WriteBarrier(); \
                _ReadWriteBarrier(); \
                break; \
            case memory_order_seq_cst: \
                MemoryBarrier(); \
                break; \
            default: /* relaxed, consume */ \
                break; \
        } \
    } while (0)
#else
#define atomic_thread_fence(order) do { \
    switch (order) { \
        case memory_order_relaxed: \
            break; \
        case memory_order_acquire: \
        case memory_order_consume: \
        case memory_order_release: \
        case memory_order_acq_rel: \
            __asm__ __volatile__ ("" : : : "memory"); \
            break; \
        case memory_order_seq_cst: \
            __asm__ __volatile__ ("mfence" : : : "memory"); \
            break; \
        default: \
            __asm__ __volatile__ ("mfence" : : : "memory"); \
            break; \
    } \
} while (0)
#endif

#define atomic_signal_fence(order) \
    ((void)0)

#define atomic_is_lock_free(obj) 0

typedef intptr_t atomic_flag;
typedef _Atomic bool                    atomic_bool;
typedef _Atomic char                    atomic_char;
typedef _Atomic signed char             atomic_schar;
typedef _Atomic unsigned char           atomic_uchar;
typedef _Atomic short                   atomic_short;
typedef _Atomic unsigned short          atomic_ushort;
typedef _Atomic int                     atomic_int;
typedef _Atomic unsigned int            atomic_uint;
typedef _Atomic long                    atomic_long;
typedef _Atomic unsigned long           atomic_ulong;
typedef _Atomic long long               atomic_llong;
typedef _Atomic unsigned long long      atomic_ullong;
typedef intptr_t atomic_wchar_t;
typedef intptr_t atomic_int_least8_t;
typedef intptr_t atomic_uint_least8_t;
typedef intptr_t atomic_int_least16_t;
typedef intptr_t atomic_uint_least16_t;
typedef intptr_t atomic_int_least32_t;
typedef intptr_t atomic_uint_least32_t;
typedef intptr_t atomic_int_least64_t;
typedef intptr_t atomic_uint_least64_t;
typedef intptr_t atomic_int_fast8_t;
typedef intptr_t atomic_uint_fast8_t;
typedef intptr_t atomic_int_fast16_t;
typedef intptr_t atomic_uint_fast16_t;
typedef intptr_t atomic_int_fast32_t;
typedef intptr_t atomic_uint_fast32_t;
typedef intptr_t atomic_int_fast64_t;
typedef intptr_t atomic_uint_fast64_t;
typedef _Atomic intptr_t                atomic_intptr_t;
typedef _Atomic uintptr_t               atomic_uintptr_t;
typedef _Atomic size_t                  atomic_size_t;
typedef intptr_t atomic_ptrdiff_t;
typedef intptr_t atomic_intmax_t;
typedef intptr_t atomic_uintmax_t;

#ifdef __TINYC__
/*
    For TCC it is missing the x64 version of _InterlockedExchangeAdd64 so we
    fake it (works the same) with InterlockedCompareExchange64 until it
    succeeds
*/

__CRT_INLINE LONGLONG _InterlockedExchangeAdd64(LONGLONG volatile *Addend, LONGLONG Value)
{
    LONGLONG Old;
    do
    {
        Old = *Addend;
    } while (InterlockedCompareExchange64(Addend, Old + Value, Old) != Old);
    return Old;
}

__CRT_INLINE LONG _InterlockedExchangeAdd(LONG volatile *Addend, LONG Value)
{
    LONG Old;
    do
    {
        Old = *Addend;
    } while (InterlockedCompareExchange(Addend, Old + Value, Old) != Old);
    return Old;
}

#define InterlockedIncrement64 _InterlockedExchangeAdd64

#endif

#define atomic_store(object, desired) \
    do                                \
    {                                 \
        MemoryBarrier();              \
        *(object) = (desired);        \
        MemoryBarrier();              \
    } while (0)

#define atomic_store_explicit(object, desired, order) \
    atomic_store(object, desired)

#define atomic_load(object) \
    (MemoryBarrier(), *(object))

#define atomic_load_explicit(object, order) \
    atomic_load(object)

#define atomic_exchange(object, desired) \
    InterlockedExchangePointer(object, desired)

#define atomic_exchange_explicit(object, desired, order) \
    atomic_exchange(object, desired)

static inline int atomic_compare_exchange_strong(intptr_t volatile *object, intptr_t volatile *expected,
                                                 intptr_t desired)
{
    intptr_t old = *expected;
    *expected = (intptr_t)InterlockedCompareExchangePointer(
        (PVOID *)object, (PVOID)desired, (PVOID)old);
    return *expected == old;
}

#define atomic_compare_exchange_strong_explicit(object, expected, desired, success, failure) \
    atomic_compare_exchange_strong(object, expected, desired)

#define atomic_compare_exchange_weak(object, expected, desired) \
    atomic_compare_exchange_strong(object, expected, desired)

#define atomic_compare_exchange_weak_explicit(object, expected, desired, success, failure) \
    atomic_compare_exchange_weak(object, expected, desired)

#ifdef _WIN64

#define atomic_fetch_add(object, operand) \
    InterlockedExchangeAdd64(object, operand)

#define atomic_fetch_sub(object, operand) \
    InterlockedExchangeAdd64(object, -(operand))

#define atomic_fetch_or(object, operand) \
    InterlockedOr64(object, operand)

#define atomic_fetch_xor(object, operand) \
    InterlockedXor64(object, operand)

#define atomic_fetch_and(object, operand) \
    InterlockedAnd64(object, operand)
#else
#define atomic_fetch_add(object, operand) \
    InterlockedExchangeAdd(object, operand)

#define atomic_fetch_sub(object, operand) \
    InterlockedExchangeAdd(object, -(operand))

#define atomic_fetch_or(object, operand) \
    InterlockedOr(object, operand)

#define atomic_fetch_xor(object, operand) \
    InterlockedXor(object, operand)

#define atomic_fetch_and(object, operand) \
    InterlockedAnd(object, operand)
#endif /* _WIN64 */

/* specialized versions with explicit object size */

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

static inline void atomic_store_u64(unsigned long long volatile * object, unsigned long long desired) {
    do {
        MemoryBarrier();
        *(object) = (desired);
        MemoryBarrier();
    } while (0);
}

static inline unsigned long long atomic_load_u64(unsigned long long volatile * object) {
    return (MemoryBarrier(), *(object));
}

#define atomic_exchange_u64(object, desired) \
    InterlockedExchange64(object, desired)

static inline int atomic_compare_exchange_strong_u64(unsigned long long volatile * object, unsigned long long volatile * expected,
                                                 unsigned long long desired)
{
	unsigned long long old = *expected;
    *expected = InterlockedCompareExchange64(object, desired, old);
    return *expected == old;
}

#define atomic_compare_exchange_weak_u64(object, expected, desired) \
    atomic_compare_exchange_strong_u64(object, expected, desired)

#define atomic_fetch_add_u64(object, operand) \
    InterlockedExchangeAdd64(object, operand)

#define atomic_fetch_sub_u64(object, operand) \
    InterlockedExchangeAdd64(object, -(operand))

#define atomic_fetch_or_u64(object, operand) \
    InterlockedOr64(object, operand)

#define atomic_fetch_xor_u64(object, operand) \
    InterlockedXor64(object, operand)

#define atomic_fetch_and_u64(object, operand) \
    InterlockedAnd64(object, operand)



static inline void atomic_store_u32(unsigned volatile * object, unsigned desired) {
    do {
        MemoryBarrier();
        *(object) = (desired);
        MemoryBarrier();
    } while (0);
}

static inline unsigned atomic_load_u32(unsigned volatile * object) {
    return (MemoryBarrier(), *(object));
}

#define atomic_exchange_u32(object, desired) \
    InterlockedExchange(object, desired)

static inline int atomic_compare_exchange_strong_u32(unsigned volatile * object, unsigned volatile * expected,
                                                 unsigned desired)
{
	unsigned old = *expected;
    *expected = InterlockedCompareExchange((void *)object, desired, old);
    return *expected == old;
}

#define atomic_compare_exchange_weak_u32(object, expected, desired) \
    atomic_compare_exchange_strong_u32(object, expected, desired)

#define atomic_fetch_add_u32(object, operand) \
    InterlockedExchangeAdd(object, operand)

#define atomic_fetch_sub_u32(object, operand) \
    InterlockedExchangeAdd(object, -(operand))

#define atomic_fetch_or_u32(object, operand) \
    InterlockedOr(object, operand)

#define atomic_fetch_xor_u32(object, operand) \
    InterlockedXor(object, operand)

#define atomic_fetch_and_u32(object, operand) \
    InterlockedAnd(object, operand)

#ifdef _MSC_VER

#define InterlockedExchangeAdd16 _InterlockedExchangeAdd16

#else

#define InterlockedExchange16 ManualInterlockedExchange16
#define InterlockedExchangeAdd16 ManualInterlockedExchangeAdd16

static inline uint16_t ManualInterlockedExchange16(volatile uint16_t* object, uint16_t desired) {
    __asm__ __volatile__ (
        "xchgw %0, %1"
        : "+r" (desired),
          "+m" (*object)
        :
        : "memory"
    );
    return desired;
}

static inline unsigned short ManualInterlockedExchangeAdd16(unsigned short volatile* Addend, unsigned short Value) {
    __asm__ __volatile__ (
        "lock xaddw %w[value], %[mem]"
        : [mem] "+m" (*Addend), [value] "+r" (Value)
        : : "memory"
    );
    return Value;
}
#endif

static inline void atomic_store_u16(unsigned short volatile * object, unsigned short desired) {
    do {
        MemoryBarrier();
        *(object) = (desired);
        MemoryBarrier();
    } while (0);
}

static inline unsigned short atomic_load_u16(unsigned short volatile * object) {
    return (MemoryBarrier(), *(object));
}

#define atomic_exchange_u16(object, desired) \
    InterlockedExchange16(object, desired)

static inline int atomic_compare_exchange_strong_u16(unsigned short volatile * object, unsigned short volatile * expected,
                                                 unsigned short desired)
{
	unsigned short old = *expected;
    *expected = InterlockedCompareExchange16(object, desired, old);
    return *expected == old;
}

#define atomic_compare_exchange_weak_u16(object, expected, desired) \
    atomic_compare_exchange_strong_u16((void*)object, expected, desired)

#define atomic_fetch_add_u16(object, operand) \
    InterlockedExchangeAdd16(object, operand)

#define atomic_fetch_sub_u16(object, operand) \
    InterlockedExchangeAdd16(object, -(operand))

#define atomic_fetch_or_u16(object, operand) \
    InterlockedOr16(object, operand)

#define atomic_fetch_xor_u16(object, operand) \
    InterlockedXor16(object, operand)

#define atomic_fetch_and_u16(object, operand) \
    InterlockedAnd16(object, operand)


#define atomic_fetch_add_explicit(object, operand, order) \
    atomic_fetch_add(object, operand)

#define atomic_fetch_sub_explicit(object, operand, order) \
    atomic_fetch_sub(object, operand)

#define atomic_fetch_or_explicit(object, operand, order) \
    atomic_fetch_or(object, operand)

#define atomic_fetch_xor_explicit(object, operand, order) \
    atomic_fetch_xor(object, operand)

#define atomic_fetch_and_explicit(object, operand, order) \
    atomic_fetch_and(object, operand)

#define atomic_flag_test_and_set(object) \
    atomic_exchange(object, 1)

#define atomic_flag_test_and_set_explicit(object, order) \
    atomic_flag_test_and_set(object)

#define atomic_flag_clear(object) \
    atomic_store(object, 0)

#define atomic_flag_clear_explicit(object, order) \
    atomic_flag_clear(object)

#ifdef _MSC_VER

#define InterlockedCompareExchange8 _InterlockedCompareExchange8
#define InterlockedExchangeAdd8 _InterlockedExchangeAdd8
#define InterlockedOr8 _InterlockedOr8
#define InterlockedXor8 _InterlockedXor8
#define InterlockedAnd8 _InterlockedAnd8

#else

#define InterlockedExchange8 ManualInterlockedExchange8
#define InterlockedCompareExchange8 ManualInterlockedCompareExchange8
#define InterlockedExchangeAdd8 ManualInterlockedExchangeAdd8
#define InterlockedOr8 ManualInterlockedOr8
#define InterlockedXor8 ManualInterlockedXor8
#define InterlockedAnd8 ManualInterlockedAnd8

static inline char ManualInterlockedExchange8(char volatile* object, char desired) {
    __asm__ __volatile__ (
        "xchgb %0, %1"
        : "+q" (desired), "+m" (*object)
        :
        : "memory"
    );
    return desired;
}

static inline unsigned char ManualInterlockedCompareExchange8(unsigned char volatile * dest, unsigned char exchange, unsigned char comparand) {
   unsigned char result;

    __asm__ volatile (
      "lock cmpxchgb %[exchange], %[dest]"
      : "=a" (result), [dest] "+m" (*dest)
      : [exchange] "q" (exchange), "a" (comparand)
      : "memory"
    );

    return result;
}

static inline unsigned char ManualInterlockedExchangeAdd8(unsigned char volatile * dest, unsigned char value) {
    unsigned char oldValue;
    unsigned char newValue;
    do {
        oldValue = *dest;
    } while (ManualInterlockedCompareExchange8(dest, oldValue + value, oldValue) != oldValue);
    return oldValue;
}

static inline unsigned char ManualInterlockedOr8(unsigned char volatile * dest, unsigned char value) {
    unsigned char oldValue;
    do {
        oldValue = *dest;
    } while (ManualInterlockedCompareExchange8(dest, oldValue | value, oldValue) != oldValue);
    return oldValue;
}

static inline unsigned char ManualInterlockedXor8(unsigned char volatile * dest, unsigned char value) {
    unsigned char oldValue;
    do {
        oldValue = *dest;
    } while (ManualInterlockedCompareExchange8(dest, oldValue ^ value, oldValue) != oldValue);
    return oldValue;
}

static inline unsigned char ManualInterlockedAnd8(unsigned char volatile * dest, unsigned char value) {
    unsigned char oldValue;
    do {
        oldValue = *dest;
    } while (ManualInterlockedCompareExchange8(dest, oldValue & value, oldValue) != oldValue);
    return oldValue;
}
#endif

static inline void atomic_store_byte(unsigned char volatile * object, unsigned char desired) {
    do {
        MemoryBarrier();
        *(object) = (desired);
        MemoryBarrier();
    } while (0);
}

static inline unsigned char atomic_load_byte(unsigned char volatile * object) {
    return (MemoryBarrier(), *(object));
}

#define atomic_exchange_byte(object, desired) \
    InterlockedExchange8(object, desired)

static inline int atomic_compare_exchange_strong_byte(unsigned char volatile * object, unsigned char volatile * expected,
                                                 unsigned char desired)
{
	unsigned char old = *expected;
    *expected = InterlockedCompareExchange8(object, desired, old);
    return *expected == old;
}

#define atomic_compare_exchange_weak_byte(object, expected, desired) \
    atomic_compare_exchange_strong_byte(object, expected, desired)

#define atomic_fetch_add_byte(object, operand) \
    InterlockedExchangeAdd8(object, operand)

#define atomic_fetch_sub_byte(object, operand) \
    InterlockedExchangeAdd8(object, -(operand))

#define atomic_fetch_or_byte(object, operand) \
    InterlockedOr8(object, operand)

#define atomic_fetch_xor_byte(object, operand) \
    InterlockedXor8(object, operand)

#define atomic_fetch_and_byte(object, operand) \
    InterlockedAnd8(object, operand)

#endif /* COMPAT_ATOMICS_WIN32_STDATOMIC_H */
