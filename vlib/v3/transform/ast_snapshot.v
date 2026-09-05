module transform

import v3.flat

struct AstBufferSnapshot {
	data    voidptr
	address u64
	bytes   u64
}

struct TransformCloneStorage {
	scope    voidptr
	nodes    AstBufferSnapshot
	children AstBufferSnapshot
}

fn (t &Transformer) snapshot_ast_base(base_nodes int, base_children int) ?(&flat.FlatAst, TransformCloneStorage) {
	nodes_cap := base_nodes + base_nodes / 4
	children_cap := base_children + base_children / 4
	node_snapshot := snapshot_ast_buffer(t.a.nodes.data, u64(base_nodes) * sizeof(flat.Node), u64(nodes_cap) * sizeof(flat.Node)) or { return none }
	child_snapshot := snapshot_ast_buffer(t.a.children.data, u64(base_children) * sizeof(flat.NodeId), u64(children_cap) * sizeof(flat.NodeId)) or {
		release_ast_buffer_snapshot(node_snapshot)
		return none
	}
	mut nodes := []flat.Node{}
	mut children := []flat.NodeId{}
	// The mappings are owned by TransformCloneStorage. These array views have
	// no managed-array header and may grow into normal arena allocations.
	unsafe {
		nodes.data = node_snapshot.data
		nodes.len = base_nodes
		nodes.cap = nodes_cap
		nodes.flags = .nofree
		children.data = child_snapshot.data
		children.len = base_children
		children.cap = children_cap
		children.flags = .nofree
	}
	return t.ast_base_clone_with_storage(nodes, children), TransformCloneStorage{
		nodes: node_snapshot
		children: child_snapshot
	}
}

fn release_transform_clone_storage(storage &TransformCloneStorage) {
	release_ast_buffer_snapshot(storage.nodes)
	release_ast_buffer_snapshot(storage.children)
	transform_worker_scope_free(storage.scope)
}
