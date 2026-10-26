module parser

import v.flat

// Keep the old spellings readable by the formatter, but compile them as their
// replacements and point the warning at the original token. Preserve the legacy
// signed type identity for compatibility with existing methods and aliases.
fn (mut p Parser) canonical_integer_type(name string, start int, end int) string {
	// These standard-library declarations retain the legacy signed type during
	// migration: builtin methods and strconv.Numeric must still accept it.
	if name.len != 5 || p.prefs.is_fmt
		|| (name == 'isize' && p.cur_module in ['builtin', 'strconv']) {
		return name
	}
	replacement := match name {
		'isize' { 'int' }
		'usize' { 'uint' }
		else { return name }
	}
	p.record_warning_span('`${name}` is deprecated; use `${replacement}` instead', start, end)
	return if name == 'usize' { replacement } else { name }
}

// Canonicalize type tokens inside explicit generic arguments before rebuilding
// their type name. Selector members keep their spelling, as they may be user names.
fn (mut p Parser) canonical_generic_type_arg(id flat.NodeId) flat.NodeId {
	node := p.a.nodes[int(id)]
	if node.kind == .ident {
		spelling := node.value_text()
		name := p.canonical_integer_type(spelling, node.pos.offset, node.pos.end)
		if name != spelling {
			return p.add_node(flat.Node{ ...node, value: name })
		}
		return id
	}
	if node.kind !in [.index, .prefix] {
		return id
	}
	mut children := []flat.NodeId{}
	for i in 0 .. int(node.children_count) {
		child := p.a.child(&node, i)
		canonical := p.canonical_generic_type_arg(child)
		if canonical != child && children.len == 0 {
			// Most generic arguments need no migration. Copy children only when
			// a deprecated spelling actually changes the tree.
			children = p.a.children[int(node.children_start)..int(node.children_start) + int(node.children_count)].clone()
		}
		if children.len > 0 {
			children[i] = canonical
		}
	}
	if children.len == 0 {
		return id
	}
	return p.add_node(flat.Node{ ...node, children_start: p.add_children(children) })
}
