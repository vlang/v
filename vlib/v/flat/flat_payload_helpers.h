#ifndef V_FLAT_PAYLOAD_HELPERS_H
#define V_FLAT_PAYLOAD_HELPERS_H

#include <stddef.h>

static inline void *v_flat_payload_ptr_get(void *base, size_t index) {
	return ((void **)base)[index];
}

static inline void v_flat_payload_ptr_set(void *base, size_t index, void *value) {
	((void **)base)[index] = value;
}

#endif
