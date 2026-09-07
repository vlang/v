#include <stdint.h>

typedef struct _TestAnonRecord {
	int kind;
	union {
		uint32_t u;
		uint8_t a;
		void *p;
	} anon_u;
} TestAnonRecord;

struct TestAnonEvent {
	uint32_t events;
	TestAnonRecord record;
	// Intentionally omitted from the V declaration. C structs may be partially declared.
	void *hidden;
};
