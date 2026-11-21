module builtin

// Test-only implementations of the track_heap profiler hooks. Only compiled
// with -d track_heap_test_stub, so normal -d track_heap builds still link the
// user's own profiler.
#insert "@VEXEROOT/vlib/builtin/track_heap_test_helpers.h"
