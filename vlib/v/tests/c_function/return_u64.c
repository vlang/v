#include <stdint.h>

uint64_t bug_return_u64(void) {
	return UINT64_C(0xA5A5A5A5A5A5A5A5);
}
