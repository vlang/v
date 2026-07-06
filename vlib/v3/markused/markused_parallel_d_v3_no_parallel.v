module markused

// precollect_body_calls falls back to the serial analysis when v3 is built
// with the internal `v3_no_parallel` define.
fn precollect_body_calls(collector CallCollector, body_ids []int, body_modules []string, imports map[string]string) []BodyCalls {
	mut results := []BodyCalls{len: body_ids.len}
	collector.collect_bodies_range(body_ids, body_modules, imports, 0, body_ids.len, mut results)
	return results
}
