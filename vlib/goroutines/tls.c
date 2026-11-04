// Thread-local storage, atomic operations, and spinlock for goroutines scheduler.
#include <stddef.h>
#include <stdint.h>

#if defined(_WIN32) || defined(_WIN64)
#if defined(__TINYC__)
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include "../../thirdparty/stdatomic/win/atomic.h"
#else
#include <windows.h>
#endif
#else
#include <stdatomic.h>
#include <sched.h>
#endif

// Thread-local Machine pointer
#if (defined(_WIN32) || defined(_WIN64)) && defined(__TINYC__)
// The bundled Windows TinyCC ignores __declspec(thread). Keep the Machine on
// the OS thread, so switching fibers preserves it and other workers cannot
// replace the main thread's Machine.
static volatile LONG _goroutines_m_tls_index = -1;

static DWORD goroutines_m_tls_index(void) {
    LONG index = InterlockedCompareExchange(&_goroutines_m_tls_index, -1, -1);
    if (index == -1) {
        DWORD candidate = TlsAlloc();
        if (candidate == TLS_OUT_OF_INDEXES) {
            fputs("goroutines: cannot allocate thread-local Machine slot\n", stderr);
            abort();
        }
        index = InterlockedCompareExchange(&_goroutines_m_tls_index, (LONG)candidate, -1);
        if (index == -1) {
            index = (LONG)candidate;
        } else {
            TlsFree(candidate);
        }
    }
    return (DWORD)index;
}

void *goroutines_get_current_m(void) {
    return TlsGetValue(goroutines_m_tls_index());
}

void goroutines_set_current_m(void *mp) {
    if (!TlsSetValue(goroutines_m_tls_index(), mp)) {
        fputs("goroutines: cannot set thread-local Machine slot\n", stderr);
        abort();
    }
}
#else
#if defined(_WIN32) || defined(_WIN64)
#if defined(__GNUC__)
static __thread void *_goroutines_current_m = NULL;
#else
static __declspec(thread) void *_goroutines_current_m = NULL;
#endif
#else
static _Thread_local void *_goroutines_current_m = NULL;
#endif

void *goroutines_get_current_m(void) {
    return _goroutines_current_m;
}

void goroutines_set_current_m(void *mp) {
    _goroutines_current_m = mp;
}
#endif

#if defined(_WIN32) || defined(_WIN64)
// Windows atomic operations using MSVC intrinsics / WinAPI

#if defined(__TINYC__)
// The bundled TinyCC import library predates ConvertFiberToThread.
int goroutines_convert_fiber_to_thread(void) {
    typedef BOOL (WINAPI *convert_fiber_to_thread_fn)(void);
    HMODULE kernel32 = GetModuleHandleA("kernel32.dll");
    if (kernel32 == NULL) return 0;
    convert_fiber_to_thread_fn convert =
        (convert_fiber_to_thread_fn)GetProcAddress(kernel32, "ConvertFiberToThread");
    return convert != NULL && convert();
}
#endif

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

void *goroutines_atomic_load_ptr(void *volatile *ptr) {
    return InterlockedCompareExchangePointer(ptr, NULL, NULL);
}

void goroutines_atomic_store_ptr(void *volatile *ptr, void *value) {
    InterlockedExchangePointer(ptr, value);
}

void *goroutines_atomic_exchange_ptr(void *volatile *ptr, void *value) {
    return InterlockedExchangePointer(ptr, value);
}

int goroutines_atomic_take_ptr(void *volatile *ptr, void *value) {
    return InterlockedCompareExchangePointer(ptr, NULL, value) == value;
}

void grt_spinlock_lock(volatile int32_t *lock) {
    while (InterlockedExchange((volatile LONG *)lock, 1) != 0) {
#if defined(__TINYC__)
        cpu_relax();
#else
        YieldProcessor();
#endif
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

void *goroutines_atomic_load_ptr(void *volatile *ptr) {
    return atomic_load((_Atomic(void *) *)ptr);
}

void goroutines_atomic_store_ptr(void *volatile *ptr, void *value) {
    atomic_store((_Atomic(void *) *)ptr, value);
}

void *goroutines_atomic_exchange_ptr(void *volatile *ptr, void *value) {
    return atomic_exchange((_Atomic(void *) *)ptr, value);
}

int goroutines_atomic_take_ptr(void *volatile *ptr, void *value) {
    return atomic_compare_exchange_strong((_Atomic(void *) *)ptr, &value, NULL);
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
