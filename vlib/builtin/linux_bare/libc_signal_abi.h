#ifndef V_BARE_SIGNAL_ABI_H
#define V_BARE_SIGNAL_ABI_H
#include <stdint.h>
#include <signal.h>
void v_signal_action_get(uintptr_t,uintptr_t);
void v_signal_action_set(uintptr_t,uintptr_t);
uintptr_t v_signal_restorer(void);
void v_signal_mask_set(uintptr_t,uint64_t);
void v_signal_queue_info(uintptr_t,int,int,unsigned int,uintptr_t);
uintptr_t v_signal_description_buffer(void);
#endif
