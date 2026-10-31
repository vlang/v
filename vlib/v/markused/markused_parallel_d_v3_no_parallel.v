module markused

fn markused_can_parallel_precollect(_ &CallCollector) bool {
	return false
}

// MarkusedPrecollect holds the call lists of one body collection.
@[heap]
struct MarkusedPrecollect {
mut:
	results []BodyCalls
}

// start_precollect_body_calls falls back to the serial analysis when v3 is built
// with the internal `v3_no_parallel` define.
fn start_precollect_body_calls(collector CallCollector, body_ids []int, body_modules []string, body_import_contexts []int, _ bool) &MarkusedPrecollect {
	mut pre := &MarkusedPrecollect{
		results: []BodyCalls{len: body_ids.len}
	}
	collector.collect_bodies_range(body_ids, body_modules, body_import_contexts, 0, body_ids.len, mut
		pre.results)
	return pre
}

fn (pre &MarkusedPrecollect) is_async() bool {
	return false
}

fn (mut pre MarkusedPrecollect) pool_busy() bool {
	return false
}

fn (mut pre MarkusedPrecollect) help() {}

fn (mut pre MarkusedPrecollect) finish() []BodyCalls {
	return pre.results
}

fn (mut pre MarkusedPrecollect) release() {}

@[unsafe]
fn markused_worker_scope_begin(_ bool) voidptr {
	return unsafe { nil }
}

fn markused_worker_scope_leave(_ voidptr) {}

fn markused_worker_scope_free(_ voidptr) {}
