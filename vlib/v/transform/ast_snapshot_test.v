module transform

fn test_ast_snapshot_rejects_empty_or_inverted_ranges() {
	assert snapshot_ast_buffer(unsafe { nil }, 0, 0) == none
	value := u64(0)
	assert snapshot_ast_buffer(&value, 8, 4) == none
}
