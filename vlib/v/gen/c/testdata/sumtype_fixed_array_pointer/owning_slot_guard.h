#include <assert.h>
static int slot_guard_drops[5];
static void slot_guard_drop(int code) { assert(code > 0 && code < 5); assert(slot_guard_drops[code] == 0); slot_guard_drops[code]++; }
static int slot_guard_count(int code) { return slot_guard_drops[code]; }

static int slot_guard_next_value;
static int slot_guard_next(void) { ++slot_guard_next_value; assert(slot_guard_next_value <= 2); return slot_guard_next_value; }
