#include <stdint.h>

typedef union my_data {
	void *ptr;
	int fd;
	uint32_t u32;
	uint64_t u64;
} my_data_t;

struct my_event {
	uint32_t events;
	union my_data data;
};
