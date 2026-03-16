// Thread-local storage, atomic operations, and spinlock for goroutines scheduler.
#include <stddef.h>
#include <stdatomic.h>
#include <stdint.h>
#include <sched.h>

// Thread-local Machine pointer
static _Thread_local void *_goroutines_current_m = NULL;

void *goroutines_get_current_m(void) {
    return _goroutines_current_m;
}

void goroutines_set_current_m(void *mp) {
    _goroutines_current_m = mp;
}

// Atomic operations on uint32_t
uint32_t goroutines_atomic_load_u32(volatile uint32_t *ptr) {
    return atomic_load((_Atomic uint32_t *)ptr);
}

void goroutines_atomic_store_u32(volatile uint32_t *ptr, uint32_t val) {
    atomic_store((_Atomic uint32_t *)ptr, val);
}

uint32_t goroutines_atomic_fetch_add_u32(volatile uint32_t *ptr, uint32_t val) {
    return atomic_fetch_add((_Atomic uint32_t *)ptr, val);
}

// Atomic operations on int32_t
int32_t goroutines_atomic_fetch_add_i32(volatile int32_t *ptr, int32_t val) {
    return atomic_fetch_add((_Atomic int32_t *)ptr, val);
}

int32_t goroutines_atomic_fetch_sub_i32(volatile int32_t *ptr, int32_t val) {
    return atomic_fetch_sub((_Atomic int32_t *)ptr, val);
}

// Atomic operations on uint64_t
uint64_t goroutines_atomic_fetch_add_u64(volatile uint64_t *ptr, uint64_t val) {
    return atomic_fetch_add((_Atomic uint64_t *)ptr, val);
}

// Atomic CAS on pointer-sized values
int goroutines_atomic_cas_u32(volatile uint32_t *ptr, uint32_t *expected, uint32_t desired) {
    return atomic_compare_exchange_strong((_Atomic uint32_t *)ptr, expected, desired);
}

int goroutines_atomic_cas_ptr(void *volatile *ptr, void **expected, void *desired) {
    return atomic_compare_exchange_strong((_Atomic(void *) *)ptr, expected, desired);
}

// Spinlock - safe to use with ucontext (unlike pthreads mutex)
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
