module parser

import v.pref
import v.token

fn test_reference_match_patterns_keep_generic_types() {
	for name in ['&Entry[int]', '&&Entry[int]', '&mod.Entry[int]'] {
		source := '${name} {}'
		mut p := Parser.new(pref.new_preferences())
		mut files := token.FileSet.new()
		file := files.add_file('pattern.v', source.len)
		p.s.init(file, source)
		p.next()
		id := p.match_branch_cond()
		node := p.a.node(id)
		actual := match node.kind {
			.ident { node.value }
			.selector { p.a.child_node(node, 0).value + '.' + node.value }
			else { '' }
		}
		assert actual == name
		assert p.tok == .lcbr
		assert p.diagnostics.len == 0
	}
}

fn test_reference_value_pattern_is_parsed_as_an_expression() {
	source := '&value {}'
	mut p := Parser.new(pref.new_preferences())
	mut files := token.FileSet.new()
	file := files.add_file('pattern.v', source.len)
	p.s.init(file, source)
	p.next()
	id := p.match_branch_cond()
	node := p.a.node(id)
	assert node.kind == .prefix
	assert node.op == .amp
	assert p.a.child_node(node, 0).value == 'value'
	assert p.tok == .lcbr
	assert p.diagnostics.len == 0
}
