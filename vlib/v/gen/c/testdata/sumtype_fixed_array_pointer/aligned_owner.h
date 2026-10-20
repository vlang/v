#include <assert.h>

static int aligned_owner_calls;
static int aligned_owner_drops[4];

static int aligned_owner_next(void) { return ++aligned_owner_calls; }
static int aligned_owner_visits(void) { return aligned_owner_calls; }

static void aligned_owner_record(int code) {
	assert(code > 0 && code < 4);
	assert(aligned_owner_drops[code] == 0);
	aligned_owner_drops[code]++;
}

static int aligned_owner_count(int code) { return aligned_owner_drops[code]; }
