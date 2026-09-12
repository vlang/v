module c

import v3.flat
import v3.types

fn array_literal_length_test_gen() &FlatGen {
	mut ast := &flat.FlatAst{}
	mut tc := types.TypeChecker.new(ast)
	mut g := FlatGen.new()
	g.a = ast
	g.tc = &tc
	return &g
}

// TinyCC mis-accounts an unsized `(T[]){a, b}` whose elements are struct values
// instead of brace initializers, and aborts with an internal `initializer
// overflow`, so the literal has to carry its own length.
fn test_array_literal_compound_literal_carries_its_length() {
	mut g := array_literal_length_test_gen()
	first := g.a.add_node(flat.Node{
		kind:  .ident
		value: 'first'
		typ:   'string'
	})
	second := g.a.add_node(flat.Node{
		kind:  .ident
		value: 'second'
		typ:   'string'
	})
	start := g.a.children.len
	g.a.children << first
	g.a.children << second
	node := flat.Node{
		kind:           .array_init
		children_start: i32(start)
		children_count: 2
	}
	g.gen_array_literal_value(node, types.Type(types.string_))
	out := g.sb.str()
	assert out.contains('(string[2]){'), out
	assert !out.contains('(string[]){'), out
}
