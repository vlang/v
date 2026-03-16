#ifndef GOROUTINES_TLS_H
#define GOROUTINES_TLS_H

#include <stdint.h>

void *goroutines_get_current_m(void);
void goroutines_set_current_m(void *mp);

uint32_t goroutines_atomic_load_u32(volatile uint32_t *ptr);
void goroutines_atomic_store_u32(volatile uint32_t *ptr, uint32_t val);
uint32_t goroutines_atomic_fetch_add_u32(volatile uint32_t *ptr, uint32_t val);
int32_t goroutines_atomic_fetch_add_i32(volatile int32_t *ptr, int32_t val);
int32_t goroutines_atomic_fetch_sub_i32(volatile int32_t *ptr, int32_t val);
uint64_t goroutines_atomic_fetch_add_u64(volatile uint64_t *ptr, uint64_t val);
int goroutines_atomic_cas_u32(volatile uint32_t *ptr, uint32_t *expected, uint32_t desired);
int goroutines_atomic_cas_ptr(void *volatile *ptr, void **expected, void *desired);

void grt_spinlock_lock(volatile int32_t *lock);
void grt_spinlock_unlock(volatile int32_t *lock);

#endif
