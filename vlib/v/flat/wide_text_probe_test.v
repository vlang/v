module flat

fn test_wide_text_interning_detaches_source_and_preserves_payload() {
	mut ast := FlatAst.new()
	mut source := []u8{len: 5}
	for i, ch in 'alpha'.bytes() { source[i] = ch }
	borrowed := unsafe { tos(source.data, source.len) }
	for i in 0 .. wide_text_probe_min_nodes {
		ast.add_node(Node{
			value:   borrowed
			typ:     borrowed
			payload: if i == 0 {
				node_payload_with_constraints([borrowed, 'U'], [borrowed, ''])
			} else {
				u32(0)
			}
		})
	}
	ast.intern_node_texts_range(0, ast.nodes.len)
	for i in 0 .. source.len { source[i] = `x` }
	for idx in [0, wide_text_probe_min_nodes - 1] {
		node := ast.nodes[idx]
		assert node.value == 'alpha'
		assert node.typ == 'alpha'
		assert node.value.str != borrowed.str
		assert ast.text(TextId(node.type_text_id())) == 'alpha'
	}
	assert ast.nodes[0].generic_params() == ['alpha', 'U']
	assert ast.nodes[0].generic_constraints() == ['alpha', '']
	assert ast.nodes[0].value.str == ast.nodes.last().value.str
}

fn test_wide_text_probe_repeated_passes_keep_canonical_text() {
	mut ast := FlatAst.new()
	_, canonical := ast.intern_text('alpha')
	for _ in 0 .. wide_text_probe_min_nodes {
		ast.add_node(Node{ value: canonical, typ: canonical })
	}
	for pass in 0 .. 3 {
		$if trace_free ? {
			eprintln('wide scratch begin')
		}
		ast.intern_node_texts_range(0, ast.nodes.len)
		$if trace_free ? {
			eprintln('wide scratch end')
		}
		assert ast.nodes[0].value == 'alpha'
		assert ast.nodes.last().typ == 'alpha'
		assert ast.text_count() == 1
		assert ast.nodes[0].value.str == canonical.str
		_ = pass
	}
}
