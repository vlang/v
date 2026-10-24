module transform

import v.flat
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
	assert a.node(hit_value).value == '?int'
	assert int(a.node(hit_value).children_count) == 2
	assert a.node(miss_value).value == '?int'
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
	for text in ['a', 'b', 'c'] {
		values << string_fold_test_literal(mut a, text)
	}
	arr := t.make_array_literal_typed(values, flat.source_type_id('[]string'))
	folded := t.try_fold_comptime_array_str(arr) or {
		assert false, 'a literal array of strings should render'
		return
	}
	assert a.node(folded).value == "['a', 'b', 'c']"
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
