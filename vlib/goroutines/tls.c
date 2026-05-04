// Thread-local storage, atomic operations, and spinlock for goroutines scheduler.
#include <stddef.h>
#include <stdint.h>

#if defined(_WIN32) || defined(_WIN64)
#include <windows.h>
#else
#include <stdatomic.h>
#include <sched.h>
#endif

// Thread-local Machine pointer
#if defined(_WIN32) || defined(_WIN64)
static __declspec(thread) void *_goroutines_current_m = NULL;
#else
static _Thread_local void *_goroutines_current_m = NULL;
#endif

void *goroutines_get_current_m(void) {
    return _goroutines_current_m;
}

void goroutines_set_current_m(void *mp) {
    _goroutines_current_m = mp;
}

#if defined(_WIN32) || defined(_WIN64)
// Windows atomic operations using MSVC intrinsics / WinAPI

uint32_t goroutines_atomic_load_u32(volatile uint32_t *ptr) {
    return InterlockedCompareExchange((volatile LONG *)ptr, 0, 0);
}

void goroutines_atomic_store_u32(volatile uint32_t *ptr, uint32_t val) {
    InterlockedExchange((volatile LONG *)ptr, (LONG)val);
}

uint32_t goroutines_atomic_fetch_add_u32(volatile uint32_t *ptr, uint32_t val) {
    return (uint32_t)InterlockedExchangeAdd((volatile LONG *)ptr, (LONG)val);
}

int32_t goroutines_atomic_fetch_add_i32(volatile int32_t *ptr, int32_t val) {
    return (int32_t)InterlockedExchangeAdd((volatile LONG *)ptr, (LONG)val);
}

int32_t goroutines_atomic_fetch_sub_i32(volatile int32_t *ptr, int32_t val) {
    return (int32_t)InterlockedExchangeAdd((volatile LONG *)ptr, (LONG)(-val));
}

uint64_t goroutines_atomic_fetch_add_u64(volatile uint64_t *ptr, uint64_t val) {
    return (uint64_t)InterlockedExchangeAdd64((volatile LONG64 *)ptr, (LONG64)val);
}

int goroutines_atomic_cas_u32(volatile uint32_t *ptr, uint32_t *expected, uint32_t desired) {
    uint32_t old = (uint32_t)InterlockedCompareExchange((volatile LONG *)ptr, (LONG)desired, (LONG)*expected);
    if (old == *expected) return 1;
    *expected = old;
    return 0;
}

int goroutines_atomic_cas_ptr(void *volatile *ptr, void **expected, void *desired) {
    void *old = InterlockedCompareExchangePointer(ptr, desired, *expected);
    if (old == *expected) return 1;
    *expected = old;
    return 0;
}

void grt_spinlock_lock(volatile int32_t *lock) {
    while (InterlockedExchange((volatile LONG *)lock, 1) != 0) {
        YieldProcessor();
    }
}

void grt_spinlock_unlock(volatile int32_t *lock) {
    InterlockedExchange((volatile LONG *)lock, 0);
}

#else
// POSIX atomic operations using C11 stdatomic

uint32_t goroutines_atomic_load_u32(volatile uint32_t *ptr) {
    return atomic_load((_Atomic uint32_t *)ptr);
}

void goroutines_atomic_store_u32(volatile uint32_t *ptr, uint32_t val) {
    atomic_store((_Atomic uint32_t *)ptr, val);
}

uint32_t goroutines_atomic_fetch_add_u32(volatile uint32_t *ptr, uint32_t val) {
    return atomic_fetch_add((_Atomic uint32_t *)ptr, val);
}

int32_t goroutines_atomic_fetch_add_i32(volatile int32_t *ptr, int32_t val) {
    return atomic_fetch_add((_Atomic int32_t *)ptr, val);
}

int32_t goroutines_atomic_fetch_sub_i32(volatile int32_t *ptr, int32_t val) {
    return atomic_fetch_sub((_Atomic int32_t *)ptr, val);
}

uint64_t goroutines_atomic_fetch_add_u64(volatile uint64_t *ptr, uint64_t val) {
    return atomic_fetch_add((_Atomic uint64_t *)ptr, val);
}

int goroutines_atomic_cas_u32(volatile uint32_t *ptr, uint32_t *expected, uint32_t desired) {
    return atomic_compare_exchange_strong((_Atomic uint32_t *)ptr, expected, desired);
}

int goroutines_atomic_cas_ptr(void *volatile *ptr, void **expected, void *desired) {
    return atomic_compare_exchange_strong((_Atomic(void *) *)ptr, expected, desired);
}

void grt_spinlock_lock(volatile int32_t *lock) {
    while (atomic_exchange((_Atomic int32_t *)lock, 1) != 0) {
        // Spin with pause hint
        #if defined(__x86_64__) || defined(__i386__)
        __asm__ volatile("pause");
        #elif defined(__aarch64__)
        __asm__ volatile("yield");
        #endif
    }
}

void grt_spinlock_unlock(volatile int32_t *lock) {
    atomic_store((_Atomic int32_t *)lock, 0);
}

#endif
