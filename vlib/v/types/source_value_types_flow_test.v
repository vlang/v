module types

import v.flat
import v.token

fn test_written_type_constraints_use_source_operands_before_checked_annotations() {
	cast := flat.Node{
		kind:         .cast_expr
		typ:          flat.source_type_id('Resolved')
		source_value: flat.source_type_id('Box[T]')
	}
	assert written_type_texts(cast) == ['Box[T]']
	array := flat.Node{
		kind:         .array_init
		typ:          flat.source_type_id('[3]Box[T]')
		source_value: flat.source_type_id('Box[T]')
	}
	assert written_type_texts(array) == ['[3]Box[T]', 'Box[T]']
	type_arg := flat.Node{
		kind:         .ident
		pos:          token.Pos{ end: 6 }
		source_value: flat.source_type_id('Box[T]')
	}
	assert written_type_texts(type_arg) == ['Box[T]']
	assert written_type_texts(flat.Node{
		kind:           .sizeof_expr
		source_value:   flat.source_type_id('Box[T]')
		children_count: 1
	}).len == 0
}

fn test_source_operands_remain_visible_to_generic_body_dependency_checks() {
	mut a := flat.FlatAst.new()
	cast := a.add_node(flat.Node{
		kind:         .cast_expr
		typ:          flat.source_type_int
		source_value: flat.source_type_id('Box[T]')
	})
	literal := a.add_node(flat.Node{ kind: .string_literal, value: 'Box[T]' })
	tc := TypeChecker.new(&a)
	params := {
		'T': true
	}
	assert tc.subtree_depends_on_type_params(cast, map[string]bool{}, params)
	assert !tc.subtree_depends_on_type_params(literal, map[string]bool{}, params)
}

fn test_library_name_reachability_retains_calls_parsed_as_casts() {
	mut a := flat.FlatAst.new()
	a.add_node(flat.Node{ kind: .cast_expr, source_value: flat.source_type_id('helper.keep') })
	a.add_node(flat.Node{ kind: .string_literal, value: 'drop' })
	decl := a.add_node(flat.Node{ kind: .fn_decl, value: 'drop' })
	tc := TypeChecker.new(&a)
	mut reached := [false, false]
	mut queue := []int{}
	tc.reach_library_fns_named_in(LibraryFnBody{ fn_idx: int(decl) }, {
		'keep': [0]
		'drop': [1]
	}, mut reached, mut queue)
	assert reached == [true, false]
	assert queue == [0]
}

fn test_vls_twin_identity_keeps_source_operands_independent_of_checked_types() {
	mut stored := flat.Node{
		kind:         .cast_expr
		typ:          flat.source_type_int
		source_value: flat.source_type_id('Count')
	}
	legacy := flat.Node{ kind: .cast_expr, value: 'Count', typ: flat.source_type_u64 }
	assert vls_twin_key(stored) == vls_twin_key(legacy)
	stored.set_source_value_id(flat.source_type_id('OtherCount'))
	assert vls_twin_key(stored) != vls_twin_key(legacy)
}
