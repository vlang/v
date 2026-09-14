module transform

// ast_snapshots_supported reports that every base-AST lane must be copied byte
// for byte here, so each lane costs a full clone.
fn ast_snapshots_supported() bool {
	return false
}

fn snapshot_ast_buffer(_data voidptr, _len u64, _capacity u64) ?AstBufferSnapshot {
	return none
}

fn release_ast_buffer_snapshot(_snapshot AstBufferSnapshot) {
}
