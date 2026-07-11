#include <stdatomic.h>

static inline uintptr_t v_test_atomic_after_compat_include(void) {
	uintptr_t value = 0;
	atomic_store_explicit((_Atomic __typeof__(value) *)&value, 7, memory_order_release);
	return atomic_load_explicit((_Atomic __typeof__(value) *)&value, memory_order_acquire);
}
