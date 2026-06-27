module transform

// run_parallel_transform is the serial fallback compiled when the build does not
// define `parallel` (no thread support). It transforms the closure-free function
// bodies on the calling thread and reports that no threading happened.
fn (mut t Transformer) run_parallel_transform(items []FnWorkItem, _base_nodes int, _base_children int) bool {
	t.transform_pure_items_serial(items)
	return false
}
