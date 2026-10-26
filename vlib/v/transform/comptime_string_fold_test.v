module transform

import v.flat
import v.parser
import v.pref
import v.types

// These folds are what the wasm backend runs instead of the strings source
// bodies, so a wrong value here is invisible to a native build's own execution
// and only shows up in the wasm module. Each test pins the folded value against
// the value the source body produces.

fn string_fold_test_literal(mut a flat.FlatAst, value string) flat.NodeId {
	return a.add_node(flat.Node{
		kind:  .string_literal
		typ:   flat.source_type_string
		value: value
	})
}

// string_fold_test_call builds `receiver.method(args...)` over hand-made nodes,
// so a test does not repeat the node-plumbing boilerplate.
fn string_fold_test_call(mut a flat.FlatAst, method string, receiver flat.NodeId, args []flat.NodeId) flat.Node {
	callee_start := a.children.len
	a.add_child(receiver)
	callee := a.add_node(flat.Node{
		kind:           .selector
		value:          method
		children_start: callee_start
		children_count: 1
	})
	call_start := a.children.len
	a.add_child(callee)
	for arg in args {
		a.add_child(arg)
	}
	call := a.add_node(flat.Node{
		kind:           .call
		typ:            flat.source_type_string
		children_start: call_start
		children_count: flat.child_count(args.len + 1)
	})
	return a.nodes[int(call)]
}

fn string_fold_test_fold(mut t Transformer, node flat.Node) flat.NodeId {
	callee := *t.a.child_node(&node, 0)
	return t.try_fold_comptime_string_method(node, callee) or {
		assert false, 'the call should fold'
		return flat.empty_node
	}
}

fn test_comptime_string_repeat_fold_emits_the_literal() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	receiver := string_fold_test_literal(mut a, 'ab')
	count := a.add_node(flat.Node{ kind: .int_literal, value: '3' })
	node := string_fold_test_call(mut a, 'repeat', receiver, [count])
	result := string_fold_test_fold(mut t, node)
	assert a.node(result).kind == .string_literal
	assert a.node(result).value == 'ababab'
}

fn test_comptime_string_repeat_fold_accepts_a_negative_count() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	receiver := string_fold_test_literal(mut a, 'ab')
	// The parser spells `-2` as a prefix node, which the scalar folder skips.
	digit := a.add_node(flat.Node{ kind: .int_literal, value: '2' })
	negative := t.make_prefix(.minus, digit)
	node := string_fold_test_call(mut a, 'repeat', receiver, [negative])
	result := string_fold_test_fold(mut t, node)
	assert a.node(result).value == ''
}

fn test_comptime_string_index_fold_keeps_the_option_type() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	receiver := string_fold_test_literal(mut a, 'hello')
	hit := string_fold_test_call(mut a, 'index', receiver, [
		string_fold_test_literal(mut a, 'll'),
	])
	miss := string_fold_test_call(mut a, 'index', receiver, [
		string_fold_test_literal(mut a, 'zz'),
	])
	hit_value := string_fold_test_fold(mut t, hit)
	miss_value := string_fold_test_fold(mut t, miss)
	// A hit carries the ok flag and the payload; a miss carries only the flag.
	assert a.node(hit_value).kind == .struct_init
	assert a.node(hit_value).value_text() == '?int'
	assert int(a.node(hit_value).children_count) == 2
	assert a.node(miss_value).value_text() == '?int'
	assert int(a.node(miss_value).children_count) == 1
}

fn test_comptime_string_split_fold_builds_the_array_literal() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	receiver := string_fold_test_literal(mut a, 'a,b,c')
	node := string_fold_test_call(mut a, 'split', receiver, [
		string_fold_test_literal(mut a, ','),
	])
	result := string_fold_test_fold(mut t, node)
	assert a.node(result).kind == .array_literal
	assert a.node(result).typ == flat.source_type_id('[]string')
	mut parts := []string{}
	for child in a.children_of(a.nodes[int(result)]) {
		assert a.node(child).kind == .string_literal
		parts << a.node(child).value
	}
	assert parts == ['a', 'b', 'c']
}

fn test_comptime_array_str_fold_renders_the_printed_text() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	mut values := []flat.NodeId{}
	texts := ['a', "it's quoted", 'line\nbreak', r'back\slash', 'tab\tend']
	for text in texts {
		values << string_fold_test_literal(mut a, text)
	}
	arr := t.make_array_literal_typed(values, flat.source_type_id('[]string'))
	folded := t.try_fold_comptime_array_str(arr) or {
		assert false, 'a literal array of strings should render'
		return
	}
	assert a.node(folded).value == texts.str()
}

fn test_comptime_array_str_normalizes_integer_literal_spellings() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	mut values := []flat.NodeId{}
	for text in ['0x10', '1_000', '0b11', '0o17'] {
		values << a.add_node(flat.Node{ kind: .int_literal, typ: flat.source_type_int, value: text })
	}
	arr := t.make_array_literal_typed(values, flat.source_type_id('[]int'))
	folded := t.try_fold_comptime_array_str(arr) or {
		assert false, 'integer array literals should render in decimal'
		return
	}
	assert a.node(folded).value == [0x10, 1_000, 0b11, 0o17].str()
}

fn test_comptime_array_str_does_not_fold_an_unsupported_integer() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	value := a.add_node(flat.Node{ kind: .int_literal, typ: flat.source_type_int, value: '2147483648' })
	arr := t.make_array_literal_typed([value], flat.source_type_id('[]int'))
	if _ := t.try_fold_comptime_array_str(arr) {
		assert false, 'integers outside the supported int range must retain their runtime path'
	}
}

fn test_comptime_array_str_follows_the_declared_local() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.set_var_type('sp', flat.source_type_id('[]string'))
	t.comptime_array_locals['sp'] = [
		ComptimeStringScalar{flat.source_type_string, 'a'},
		ComptimeStringScalar{flat.source_type_string, 'b'},
	]
	ident := a.add_node(flat.Node{
		kind:  .ident
		typ:   flat.source_type_id('[]string')
		value: 'sp'
	})
	folded := t.try_fold_comptime_array_str(ident) or {
		assert false, 'a local of compile-time-known strings should render'
		return
	}
	assert a.node(folded).value == "['a', 'b']"
}

fn test_comptime_string_fold_values_match_the_source_bodies() {
	assert 'a,b,c'.split(',') == ['a', 'b', 'c']
	assert ''.split(',') == ['']
	assert 'abc'.split(',') == ['abc']
	assert 'a,b,'.split(',') == ['a', 'b', '']
	assert 'abc'.split('') == ['a', 'b', 'c']
	assert 'a,,b'.split(',,') == ['a', 'b']
	assert 'ab'.repeat(3) == 'ababab'
	assert 'ab'.repeat(0) == ''
	assert 'ab'.repeat(1) == 'ab'
	assert ''.repeat(5) == ''
	assert 'ab'.repeat(-2) == ''
	assert 'abc'.reverse() == 'cba'
	assert ''.reverse() == ''
	assert 'a'.reverse() == 'a'
	// reverse works on bytes, so a two-byte character comes back with its bytes
	// swapped rather than as one rune.
	assert 'é'.reverse().len == 2
	assert int('é'.reverse()[0]) == 0xA9
	if idx := 'hello'.index('ll') {
		assert idx == 2
	} else {
		assert false, 'expected a hit'
	}
	assert 'hello'.index('zz') == none
	// An empty needle is found at position 0, exactly as the source body answers
	// it now that the builtin changed its empty-needle rule.
	assert 'hello'.index('') or { -1 } == 0
	assert ''.index('a') == none
	assert 'hello'.index('hello') or { -1 } == 0
}

fn test_comptime_array_bindings_do_not_cross_function_boundaries() {
	mut p := parser.Parser.new(pref.new_preferences())
	mut a := p.parse_text('array_binding_functions.v', 'fn render(values []string) { println(values) }')
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.cur_module = 'main'
	t.cur_file = 'array_binding_functions.v'
	t.transformed_fns = []bool{len: a.nodes.len}
	t.prepare()
	// A surrounding function or constant initializer may own this same name.
	t.comptime_array_locals['values'] = [ComptimeStringScalar{flat.source_type_string, 'stale'}]
	mut fn_idx := -1
	for i, node in a.nodes {
		if node.kind == .fn_decl && node.value == 'render' { fn_idx = i }
	}
	assert fn_idx >= 0
	t.transform_fn_body(fn_idx)
	assert t.comptime_array_locals['values'][0].value == 'stale'
	for node in a.nodes {
		assert node.kind != .string_literal || node.value != "['stale']"
	}
}

fn test_comptime_array_bindings_restore_after_a_nested_statement_scope() {
	mut p := parser.Parser.new(pref.new_preferences())
	mut a := p.parse_text('array_binding_scope.v', "fn scope() {\n values := ['inner']\n println(values)\n}")
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.cur_module = 'main'
	t.cur_file = 'array_binding_scope.v'
	t.prepare()
	t.set_var_type('values', flat.source_type_id('[]string'))
	t.comptime_array_locals['values'] = [ComptimeStringScalar{flat.source_type_string, 'outer'}]
	mut body := []flat.NodeId{}
	for node in a.nodes {
		if node.kind == .fn_decl && node.value == 'scope' {
			body = a.children_of(&node).clone()
			break
		}
	}
	assert body.len == 2
	t.transform_stmts(body)
	assert t.comptime_array_locals['values'][0].value == 'outer'
	mut saw_inner := false
	for node in a.nodes {
		if node.kind == .string_literal && node.value == "['inner']" { saw_inner = true }
	}
	assert saw_inner
}
