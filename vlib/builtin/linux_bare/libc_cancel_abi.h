#ifndef V_BARE_CANCEL_ABI_H
#define V_BARE_CANCEL_ABI_H
#include <stdint.h>
#include <stddef.h>
#include <pthread.h>
uintptr_t v_cancel_implicit(void);
uintptr_t v_cancel_implicit_capacity(void);
intptr_t v_cancel_asm(uint32_t *, intptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t);
void v_cancel_window(uintptr_t);
uintptr_t v_cancel_context_pc(uintptr_t, uintptr_t);
uintptr_t v_cancel_handler_address(void);
uintptr_t v_cancel_jump_sp(uintptr_t);
uintptr_t v_cancel_buffer_get(uintptr_t, uint32_t);
void v_cancel_buffer_set(uintptr_t, uint32_t, uintptr_t);
int32_t v_cancel_unwinder_available(void);
void v_cancel_force_unwind(uintptr_t);
int64_t v_cancel_syscall(int64_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t);
uint32_t v_cancel_shield_enter(void);
void v_cancel_shield_leave(uint32_t);
int32_t v_cancel_is_pending(void);
void v_cancel_condition_fields(uintptr_t, uintptr_t);
int32_t v_cancel_mutex_lock(uintptr_t);
int32_t v_cancel_mutex_unlock(uintptr_t);
#endif
