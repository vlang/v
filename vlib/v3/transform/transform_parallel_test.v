module transform

import v3.flat

fn test_deferred_worker_node_clone_preserves_skip_ownership_drops() {
	$if !v3_no_parallel ? {
		mut t := Transformer{
			deferred_base_writes:  [
				DeferredBaseWrite{
					idx:  7
					kind: 2
					node: flat.Node{
						kind:                 .for_stmt
						skip_ownership_drops: true
					}
				},
			]
			scoped_promoted_texts: map[string]string{}
		}
		t.clone_deferred_worker_writes_from(0)
		cloned := t.deferred_base_writes[0].node
		assert cloned.kind == .for_stmt
		assert cloned.skip_ownership_drops
	}
}
