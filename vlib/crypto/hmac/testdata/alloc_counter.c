#include <stdint.h>

static uint64_t hmac_test_allocations;

void vheap_alloc(void* pointer, uint64_t size) {
	(void)pointer;
	(void)size;
	hmac_test_allocations++;
}

void vheap_free(void* pointer) {
	(void)pointer;
}

uint64_t hmac_test_allocation_count(void) {
	return hmac_test_allocations;
}
