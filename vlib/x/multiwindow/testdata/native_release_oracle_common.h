#ifndef V_MULTIWINDOW_NATIVE_RELEASE_ORACLE_COMMON_H
#define V_MULTIWINDOW_NATIVE_RELEASE_ORACLE_COMMON_H

#include <stdint.h>

static uint64_t v_multiwindow_test_release_oracle_next_sequence = UINT64_C(1);

static inline void v_multiwindow_test_release_oracle_reset_sequence(void) {
	v_multiwindow_test_release_oracle_next_sequence = UINT64_C(1);
}

static inline uint64_t v_multiwindow_test_release_oracle_take_sequence(void) {
	uint64_t sequence = v_multiwindow_test_release_oracle_next_sequence;
	if (sequence == UINT64_MAX) {
		v_multiwindow_test_release_oracle_next_sequence = UINT64_C(0);
	} else if (sequence != 0) {
		v_multiwindow_test_release_oracle_next_sequence = sequence + UINT64_C(1);
	}
	return sequence;
}

#endif
