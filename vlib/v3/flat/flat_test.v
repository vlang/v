module flat

fn test_node_kind_has_one_canonical_representation() {
	mut ast := FlatAst.new()
	id := ast.add_node(Node{
		kind: .call
	})
	assert ast.nodes[int(id)].kind == .call
}

fn test_child_count_supports_large_generated_nodes() {
	assert child_count(50_000) == 50_000
}

fn test_node_uses_compact_header_and_uncommon_payload() {
	empty := Node{}
	assert empty.generic_params().len == 0
	mut with_params := Node{}
	with_params.set_generic_params(['T', 'U'])
	assert with_params.generic_params() == ['T', 'U']
	assert sizeof(NodeKind) == 1
	assert sizeof(Op) == 1
	// The former always-present []string field made Node 96 bytes on 64-bit.
	assert sizeof(Node) <= 72
}
