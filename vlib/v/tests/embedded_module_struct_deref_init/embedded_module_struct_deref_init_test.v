module main

import embedded_module_struct_deref_init.perf

struct EventSource {
	id int
}

// The reference fields of an embedded struct from another module are initialized by
// the embedded value itself (`Performance: *implementation`).
struct ScopePerformance {
	EventSource
	perf.Performance
mut:
	handled bool
}

fn wrap_performance(implementation &perf.Performance) &ScopePerformance {
	return &ScopePerformance{
		Performance: *implementation
	}
}

fn test_embedded_module_struct_initialized_from_dereferenced_value() {
	scope := wrap_performance(perf.Performance.create(3))
	assert scope.timeline.entries == 3
	assert scope.limit == 250
	assert !scope.handled
}
