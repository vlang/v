// Keep platform signal layouts in a cached native object, outside generated C.
#include <stddef.h>
#if defined(V_GCBOEHM_NATIVE)
#if defined(__TINYC__) && defined(__linux__)
#include <gc/gc.h>
#else
#include <gc.h>
#endif
#define CUSTOM_DEFINE_gcboehm
#endif
#define V_PARALLEL_CC 1
#define V_PARALLEL_CC_OUT_0 1
#include "segfault_handler_nix.h"

void* v_thread_signal_stack_enter(void* stack_top, size_t stack_size) {
#if defined(V_SEGFAULT_HANDLER_ACTIVE)
	stack_t old;
	memset(&old, 0, sizeof(old));
	if (sigaltstack(NULL, &old) != 0 || !(old.ss_flags & SS_DISABLE)) {
		return NULL;
	}
#if defined(__APPLE__)
	(void)stack_top;
	uintptr_t hi = (uintptr_t)pthread_get_stackaddr_np(pthread_self());
	stack_size = pthread_get_stacksize_np(pthread_self());
#else
	uintptr_t hi = (uintptr_t)stack_top;
#endif
	v_signal_stack_register(hi > stack_size ? hi - stack_size : 0, hi);
	stack_t ss;
	memset(&ss, 0, sizeof(ss));
	if (sigaltstack(NULL, &ss) == 0 && !(ss.ss_flags & SS_DISABLE)) {
		return ss.ss_sp;
	}
#else
	(void)stack_top;
	(void)stack_size;
#endif
	return NULL;
}

void v_thread_signal_stack_leave(void* alt_stack) {
#if defined(V_SEGFAULT_HANDLER_ACTIVE)
	if (alt_stack == NULL) {
		return;
	}
	stack_t ss;
	memset(&ss, 0, sizeof(ss));
	ss.ss_flags = SS_DISABLE;
	if (sigaltstack(&ss, NULL) == 0) {
		munmap(alt_stack, (size_t)((uintptr_t*)alt_stack)[3]);
	}
#else
	(void)alt_stack;
#endif
}
