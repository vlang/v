#ifndef V_BARE_PTHREAD_ABI_H
#define V_BARE_PTHREAD_ABI_H
#include <stdint.h>
#include <stddef.h>
#include <pthread.h>
#include <setjmp.h>
size_t v_pthread_attributes_size(void);
size_t v_pthread_jump_size(void);
uintptr_t v_pthread_call(uintptr_t, uintptr_t);
void v_pthread_destructor_call(uintptr_t, uintptr_t);
void v_pthread_once_call(uintptr_t);
uintptr_t v_pthread_tls_load(uint32_t);
void v_pthread_tls_store(uint32_t, uintptr_t);
uintptr_t v_pthread_tls_identity(void);
int32_t v_pthread_sched_read(uintptr_t);
void v_pthread_sched_write(uintptr_t, int32_t);
size_t v_rwlock_size(void);
size_t v_rwlock_attributes_size(void);
#endif
