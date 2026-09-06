#include <stdint.h>

typedef struct _TestAnonRecord {
	int kind;
	union {
		uint32_t u;
		uint8_t a;
	} anon_u;
} TestAnonRecord;

struct TestAnonEvent {
	uint32_t events;
	TestAnonRecord record;
};
