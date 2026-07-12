#include <stdatomic.h>

static inline uintptr_t v_test_atomic_after_compat_include(void) {
	atomic_int value = ATOMIC_VAR_INIT(0);
	atomic_flag flag = ATOMIC_FLAG_INIT;
	atomic_store_explicit(&value, 7, memory_order_release);
	if (atomic_flag_test_and_set(&flag)) {
		return 0;
	}
	atomic_flag_clear(&flag);
	return (uintptr_t)atomic_load_explicit(&value, memory_order_acquire);
}
