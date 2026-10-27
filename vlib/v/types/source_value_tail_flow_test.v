module types

import os
import v.flat
import v.parser
import v.pref

fn source_value_tail_checker(name string, source string) &TypeChecker {
	path := os.join_path(os.vtmp_dir(), 'source_value_tail_${name}_${os.getpid()}.v')
	os.write_file(path, source) or { panic(err) }
	defer { os.rm(path) or {} }
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	_ = tc.check_semantics_opt(false)
	return tc
}

fn test_type_expression_source_operands_keep_independent_annotations() {
	mut a := flat.FlatAst.new()
	name_id := flat.source_type_id('Widget')
	name := a.add_node(flat.Node{
		kind:         .ident
		source_value: name_id
		typ:          flat.source_type_int
	})
	a.children << name
	pointer := a.add_node(flat.Node{
		kind:           .prefix
		op:             .amp
		children_count: 1
	})
	array := a.add_node(flat.Node{
		kind:         .array_init
		source_value: name_id
		typ:          flat.source_type_id('[]string')
	})
	base := a.add_node(flat.Node{ kind: .ident, source_value: flat.source_type_id('Box') })
	index_start := a.children.len
	a.children << [base, name]
	application := a.add_node(flat.Node{
		kind:           .index
		children_start: i32(index_start)
		children_count: 2
	})
	tc := TypeChecker.new(&a)
	assert tc.type_expr_name(name) == name_id
	assert tc.generic_call_type_arg_name(name) == name_id
	assert tc.type_expr_name(pointer) == flat.source_type_id('&Widget')
	assert tc.type_expr_name(array) == flat.source_type_id('[]Widget')
	assert tc.type_expr_name(application) == flat.source_type_id('Box[Widget]')
	assert tc.generic_call_type_arg_name(application) == flat.source_type_id('Box[Widget]')
}

fn test_const_cast_source_operand_is_independent_of_its_annotation() {
	mut a := flat.FlatAst.new()
	literal := a.add_node(flat.Node{ kind: .int_literal, value: '42' })
	a.children << literal
	integer_cast := a.add_node(flat.Node{
		kind:           .cast_expr
		source_value:   flat.source_type_int
		typ:            flat.source_type_f64
		children_count: 1
	})
	float_cast := a.add_node(flat.Node{
		kind:           .cast_expr
		source_value:   flat.source_type_f64
		typ:            flat.source_type_int
		children_count: 1
	})
	tc := TypeChecker.new(&a)
	value := tc.const_int_expr(integer_cast, '', []string{}) or { -1 }
	assert value == 42
	assert tc.const_int_expr(float_cast, '', []string{}) == none
}

fn test_match_type_pattern_reads_numeric_source_operand() {
	a := flat.FlatAst.new()
	tc := TypeChecker.new(&a)
	pattern_id := flat.source_type_id('?int')
	pattern := flat.Node{
		kind:         .ident
		source_value: pattern_id
		typ:          flat.source_type_int
	}
	matched := tc.match_type_pattern(&pattern) or { u32(0) }
	assert matched == pattern_id
}

fn test_check_ident_accepts_source_backed_and_legacy_spellings() {
	mut a := flat.FlatAst.new()
	legacy := a.add_node(flat.Node{ kind: .ident, value: 'bound' })
	canonical := a.add_node(flat.Node{
		kind:         .ident
		source_value: flat.source_type_id('bound')
	})
	mut tc := TypeChecker.new(&a)
	tc.reset_node_caches(a.nodes.len)
	tc.cur_scope.insert('bound', Type(int_))
	tc.check_ident(legacy, a.nodes[int(legacy)])
	tc.check_ident(canonical, a.nodes[int(canonical)])
	assert tc.expr_key(legacy) == 'bound'
	assert tc.expr_key(canonical) == 'bound'
	assert tc.expr_key_composite(canonical) == 'bound'
	assert tc.expr_key_part(canonical) == 'bound'
	assert tc.expr_key_matches(canonical, 'bound')
	assert !tc.expr_key_matches(canonical, 'other')
	assert tc.errors.len == 0
	assert tc.expr_type(legacy)? == Type(int_)
	assert tc.expr_type(canonical)? == Type(int_)
}

fn test_call_generic_argument_queries_use_numeric_payload() {
	mut a := flat.FlatAst.new()
	callee := a.add_node(flat.Node{ kind: .ident, value: 'known' })
	a.children << callee
	mut fn_node := flat.Node{ kind: .fn_decl }
	fn_node.set_generic_params(['T'])
	fn_id := a.add_node(fn_node)
	mut call := flat.Node{ kind: .call, children_count: 1 }
	call.set_call_type_args([flat.source_type_id(' T ')])
	mut tc := TypeChecker.new(&a)
	tc.fn_context.node_id = int(fn_id)
	tc.fn_ret_types['known'] = Type(int_)
	assert tc.call_has_explicit_generic_args(call)
	assert tc.call_generic_args_have_placeholders(call)
	call.set_call_type_args([flat.source_type_int])
	assert tc.call_has_explicit_generic_args(call)
	assert !tc.call_generic_args_have_placeholders(call)
	call.set_call_type_args([]u32{})
	assert !tc.call_has_explicit_generic_args(call)
	assert !tc.call_generic_args_have_placeholders(call)
}

fn test_generic_call_rejects_source_backed_option_and_result_type_names() {
	for spelling in ['?int', '!int'] {
		mut a := flat.FlatAst.new()
		base := a.add_node(flat.Node{ kind: .ident, value: 'known' })
		type_arg := a.add_node(flat.Node{
			kind:         .ident
			source_value: flat.source_type_id(spelling)
			typ:          flat.source_type_f64
		})
		a.children << [base, type_arg]
		callee := a.add_node(flat.Node{ kind: .index, children_count: 2 })
		call_start := a.children.len
		a.children << callee
		call_id := a.add_node(flat.Node{
			kind:           .call
			children_start: i32(call_start)
			children_count: 1
		})
		mut tc := TypeChecker.new(&a)
		// These synthetic nodes have no source positions for diagnostic filtering.
		tc.diagnose_unknown_calls = true
		tc.reset_node_caches(a.nodes.len)
		tc.fn_ret_types['known'] = Type(int_)
		tc.check_call(call_id, a.nodes[int(call_id)])
		assert tc.errors.len == 1, tc.errors.str()
		assert tc.errors[0].kind == .call_arg_mismatch
		assert tc.errors[0].msg == 'cannot use Option type name as concrete type'
		assert tc.errors[0].node == type_arg
	}
}

fn test_generic_struct_init_inference_reads_source_operand_and_legacy_fallback() {
	mut a := flat.FlatAst.new()
	// Non-main declarations are registered with a module-qualified name.
	module_id := a.add_val(.module_decl, 'fixture')
	source_field := a.add_node(flat.Node{
		kind:  .field_decl
		value: 'value'
		typ:   flat.source_type_id('T')
	})
	decl_start := a.begin_children()
	a.add_child(source_field)
	declaration := a.add_node(flat.Node{
		kind:           .struct_decl
		value:          'Box'
		children_start: decl_start
		children_count: 1
	})
	value := a.add_node(flat.Node{ kind: .string_literal, value: 'hello' })
	field_start := a.begin_children()
	a.add_child(value)
	field := a.add_node(flat.Node{
		kind:           .field_init
		value:          'value'
		children_start: field_start
		children_count: 1
	})
	init_start := a.begin_children()
	a.add_child(field)
	original_id := flat.source_type_id(' Box ')
	checked_id := flat.source_type_id('UnrelatedCheckedType')
	legacy := flat.Node{
		kind:           .struct_init
		value:          ' Box '
		typ:            checked_id
		children_start: init_start
		children_count: 1
	}
	canonicalized := a.add_node(legacy)
	indexed := a.add_node(flat.Node{
		kind:           .struct_init
		source_value:   original_id
		typ:            checked_id
		children_start: init_start
		children_count: 1
	})
	assert legacy.value == ' Box '
	assert legacy.source_value == 0
	assert a.nodes[int(canonicalized)].value == ''
	assert a.nodes[int(canonicalized)].source_value == original_id
	assert a.nodes[int(indexed)].value == ''
	mut tc := TypeChecker.new(&a)
	tc.reset_node_caches(a.nodes.len)
	tc.cur_module = 'fixture'
	tc.top_level_idx = [i32(module_id), i32(declaration)]
	tc.structs[flat.source_type_id('fixture.Box')] = [StructField{ name: 'value', typ: unknown_type('T') }]
	tc.struct_generic_params[flat.source_type_id('fixture.Box')] = ['T']
	for node in [legacy, a.nodes[int(canonicalized)], a.nodes[int(indexed)]] {
		inferred := tc.infer_generic_struct_init_type(node) or {
			panic('failed to infer Box from original struct operand')
		}
		assert inferred.source_type_id() == flat.source_type_id('fixture.Box[string]')
		assert node.typ == checked_id
	}
}

fn test_parsed_generic_calls_reject_option_and_result_type_names() {
	for index, spelling in ['?int', '!int'] {
		tc := source_value_tail_checker('generic_call_${index}', 'module main
fn known[T]() {}
fn main() { known[${spelling}]() }
')
		assert tc.errors.len == 1, tc.errors.str()
		assert tc.errors[0].kind == .call_arg_mismatch
		assert tc.errors[0].msg == 'cannot use Option type name as concrete type'
		type_arg := tc.a.nodes[int(tc.errors[0].node)]
		assert type_arg.source_value_id() == flat.source_type_id(spelling)
	}
}

fn test_parsed_generic_struct_inference_preserves_module_registration() {
	for module_name in ['main', 'fixture'] {
		tc := source_value_tail_checker('struct_${module_name}', 'module ${module_name}
struct Box[T] { value T }
fn main() {
 box := Box{value: "hello"}
 _ = box.value
}
')
		assert tc.errors.len == 0, tc.errors.str()
		expected := if module_name == 'main' { 'Box[string]' } else { 'fixture.Box[string]' }
		mut found := false
		for index, node in tc.a.nodes {
			if node.kind != .struct_init {
				continue
			}
			found = true
			assert node.source_value_id() == flat.source_type_id('Box')
			inferred := tc.expr_type(flat.NodeId(index)) or { panic('missing inferred Box type') }
			assert inferred.source_type_id() == flat.source_type_id(expected)
		}
		assert found
	}
}
