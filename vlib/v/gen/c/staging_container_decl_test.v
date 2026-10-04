module c

import v.flat
import v.types

fn staging_container_decl_source(kind flat.NodeKind, typ types.Type, marker string, captured bool) string {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	if captured {
		g.defer_capture_types['staged'] = typ
	}
	lhs := a.add_node(flat.Node{ kind: .ident, value: 'staged', typ: typ.name() })
	rhs := a.add_node(flat.Node{
		kind:  kind
		value: if typ is types.Array { typ.elem_type.name() } else { typ.name() }
		typ:   typ.name()
	})
	tc.register_synth_type(rhs, typ)
	start := a.children.len
	a.children << [lhs, rhs]
	decl := flat.Node{
		kind:           .decl_assign
		value:          marker
		typ:            typ.name()
		children_start: i32(start)
		children_count: 2
	}
	g.gen_decl_assign(decl)
	return g.sb.str()
}

fn test_staging_map_skips_default_allocation_and_keeps_ordinary_defaults() {
	typ := types.Type(types.Map{
		key_type:   types.Type(types.String{})
		value_type: types.Type(types.int_)
	})
	assert staging_container_decl_source(.map_init, typ, '__v3_zeroed_stack_value_decl', false) == 'map staged = {0};\n'
	ordinary := staging_container_decl_source(.map_init, typ, '', false)
	assert ordinary.contains('map staged = new_map'), ordinary
	captured := staging_container_decl_source(.map_init, typ, '__v3_zeroed_stack_value_decl', true)
	assert captured.starts_with('staged = new_map'), captured
}

fn test_staging_array_skips_default_allocation_and_keeps_ordinary_defaults() {
	typ := types.Type(types.Array{ elem_type: types.Type(types.int_) })
	assert staging_container_decl_source(.array_init, typ, '__v3_zeroed_stack_value_decl', false) == 'Array staged = {0};\n'
	ordinary := staging_container_decl_source(.array_init, typ, '', false)
	assert ordinary.contains('Array staged = array_new('), ordinary
	captured := staging_container_decl_source(.array_init, typ, '__v3_zeroed_stack_value_decl', true)
	assert captured.starts_with('staged = array_new('), captured
}

fn staging_map_choice(choice int, left map[string]int, right map[string]int) map[string]int {
	return if choice == 0 {
		copied := left.clone()
		copied
	} else if choice == 1 {
		right.clone()
	} else {
		{
			'fallback': choice
		}
	}
}

fn staging_optional_values(present bool) ?[]int {
	if !present {
		return none
	}
	return [3, 4]
}

fn staging_array_choice(present bool, nested bool) []int {
	return if values := staging_optional_values(present) {
		if nested {
			values.clone()
		} else {
			[values[0]]
		}
	} else {
		[9]
	}
}

fn test_staging_containers_preserve_nested_and_optional_branch_values() {
	left := {
		'left': 1
	}
	right := {
		'right': 2
	}
	assert staging_map_choice(0, left, right) == left
	assert staging_map_choice(1, left, right) == right
	assert staging_map_choice(2, left, right) == {
		'fallback': 2
	}
	assert staging_array_choice(true, true) == [3, 4]
	assert staging_array_choice(true, false) == [3]
	assert staging_array_choice(false, true) == [9]
	assert staging_array_choice(false, false) == [9]
}
