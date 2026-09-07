module c

import v.ast
import v.pref

fn test_boehm_keepalive_pointer_alias_does_not_recurse_into_its_parent() {
	mut table := ast.new_table()
	node_idx := table.register_sym(ast.TypeSymbol{
		kind: .struct
		name: 'C.Node'
		cname: 'Node'
		mod: 'main'
		language: .c
		info: ast.Struct{}
	})
	node_type := ast.idx_to_type(node_idx)
	alias_idx := table.register_sym(ast.TypeSymbol{
		kind: .alias
		name: 'C.NodePtr'
		cname: 'NodePtr'
		mod: 'main'
		language: .c
		info: ast.Alias{
			parent_type: node_type.ref()
			language: .c
		}
	})
	table.type_symbols[node_idx].info = ast.Struct{
		fields: [ast.StructField{
			name: 'next'
			typ: ast.idx_to_type(alias_idx)
		}]
	}
	pref_ := pref.new_preferences()
	mut reflection_strings := map[string]int{}
	mut g := Gen{
		pref: pref_
		table: table
		anon_fn: unsafe { nil }
		reflection_strings: &reflection_strings
		generic_parts_cache: []i8{len: table.type_symbols.len}
		unwrap_generic_cache: map[u64]ast.Type{}
	}
	assert g.c_type_has_ptr(node_type)
	assert !g.type_has_pointer_bearing_c_union(node_type)
}

fn test_boehm_keepalive_repeated_union_hierarchy_is_memoized() {
	mut table := ast.new_table()
	mut union_type := ast.u32_type
	for depth in 0 .. 20 {
		union_idx := table.register_sym(ast.TypeSymbol{
			kind: .struct
			name: 'C.RepeatedUnion${depth}'
			cname: 'RepeatedUnion${depth}'
			mod: 'main'
			language: .c
			info: ast.Struct{
				is_union: true
				fields: [
					ast.StructField{
						name: 'left'
						typ: union_type
					},
					ast.StructField{
						name: 'right'
						typ: union_type
					},
				]
			}
		})
		union_type = ast.idx_to_type(union_idx)
	}
	pref_ := pref.new_preferences()
	mut reflection_strings := map[string]int{}
	mut g := Gen{
		pref: pref_
		table: table
		anon_fn: unsafe { nil }
		reflection_strings: &reflection_strings
		generic_parts_cache: []i8{len: table.type_symbols.len}
		unwrap_generic_cache: map[u64]ast.Type{}
	}
	assert !g.c_type_has_ptr(union_type)
	assert !g.type_has_pointer_bearing_c_union(union_type)
}
