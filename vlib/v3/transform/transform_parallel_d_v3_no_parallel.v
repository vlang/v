module transform

// run_parallel_transform falls back to the serial transform when v3 is built
// with the internal `v3_no_parallel` define.
fn (mut t Transformer) run_parallel_transform(items []FnWorkItem, _ int, _ int) bool {
	t.transform_pure_items_serial(items)
	return false
}
