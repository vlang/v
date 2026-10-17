module parser

import v.pref

fn test_struct_initializer_accepts_a_newline_before_its_brace() {
	for name in ['Edit', 'platform.Edit', 'Edit[int]', 'platform.Edit[int]'] {
		mut p := Parser.new(pref.new_preferences())
		a := p.parse_text('initializer.v', 'fn main() { edits << ${name}\n { offset, count } }')
		assert p.diagnostics.len == 0, p.diagnostics.str()
		assert a.nodes.count(it.kind == .struct_init) == 1, name
	}
}

fn test_newline_brace_keeps_control_flow_and_explicit_statement_boundaries() {
	for source in ['fn main() { if Enabled\n {} }', 'fn main() { _ = Edit;\n { println(1) } }'] {
		mut p := Parser.new(pref.new_preferences())
		a := p.parse_text('boundary.v', source)
		assert p.diagnostics.len == 0, p.diagnostics.str()
		assert a.nodes.count(it.kind == .struct_init) == 0
	}
}

fn test_address_of_collection_named_receiver_keeps_expression_diagnostics() {
	for name in ['array', 'map', 'chan'] {
		mut p := Parser.new(pref.new_preferences())
		a := p.parse_text('address.v', 'fn main() { ${name} := Holder{}; _ = &${name}.object }')
		assert p.diagnostics.len == 0, p.diagnostics.str()
		assert a.nodes.any(it.kind == .selector && it.value == 'object')
	}
}

fn test_nested_if_value_can_be_used_as_a_condition() {
	for condition in [
		'if sensitive { a != b } else { a == b }',
		'if sensitive { a != b }\nelse { a == b }',
		'if compare(Holder{object: 1}) { a != b } else { a == b }',
	] {
		mut p := Parser.new(pref.new_preferences())
		a := p.parse_text('condition.v', 'fn main() { if ${condition} { println(1) } }')
		assert p.diagnostics.len == 0, p.diagnostics.str()
		assert a.nodes.count(it.kind == .if_expr) == 2
	}
}

fn test_incomplete_repeated_if_keeps_its_diagnostic() {
	mut p := Parser.new(pref.new_preferences())
	p.parse_text('condition.v', 'fn main() { if if sensitive { println(1) } }')
	assert p.diagnostics.any(it.message.contains('did you write `if` twice'))
}
