fn type_text_contains_qualified_import(text string, alias string) bool {
	if text.len <= alias.len || alias == '' {
		return false
	}
	mut start := 0
	needle := alias + '.'
	for start < text.len {
		relative := text[start..].index(needle) or { return false }
		index := start + relative
		if index == 0 || !is_type_symbol_byte(text[index - 1]) {
			return true
		}
		start = index + needle.len
	}
	return false
}

fn (mut tc TypeChecker) check_deprecated_byte_types() {
	mut identifier_offsets := map[u64]bool{}
	for node in tc.a.nodes {
		// Parameter positions span their names; synthetic receiver positions do not.
		if (node.kind == .ident || (node.kind == .param && node.op != .dot))
			&& node.value == 'byte' && node.pos.is_valid() {
			identifier_offsets[deprecated_byte_position_key(node.pos.id, node.pos.offset)] = true
		}
	}
	mut pending_file := ''
	for idx in tc.top_level_idx {
		node := tc.a.nodes[idx]
		if node.kind == .file {
			pending_file = node.value
			tc.enter_file(node.value)
			continue
		}
		if pending_file.len == 0 || !node.pos.is_valid() {
			continue
		}
		tc.check_deprecated_byte_types_in_file(flat.NodeId(idx), node.pos.id, pending_file, identifier_offsets)
		pending_file = ''
	}
}

fn deprecated_byte_position_key(file_id int, offset int) u64 {
	return (u64(u32(file_id)) << 32) | u64(u32(offset))
}
