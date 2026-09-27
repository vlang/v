module c

import v.ast
import v.pref

fn map_layout_test_gen(mut table ast.Table) Gen {
	pref_ := pref.new_preferences()
	mut reflection_strings := map[string]int{}
	return Gen{
		pref: pref_
		table: table
		anon_fn: unsafe { nil }
		reflection_strings: &reflection_strings
		generic_parts_cache: []i8{len: table.type_symbols.len}
		unwrap_generic_cache: map[u64]ast.Type{}
	}
}

fn test_map_internal_field_follows_the_loaded_builtin_layout() {
	mut old_table := ast.new_table()
	old_table.type_symbols[ast.map_type_idx].info = ast.Struct{
		fields: [ast.StructField{
			name: 'len'
			typ: ast.int_type
		}]
	}
	old_gen := map_layout_test_gen(mut old_table)
	assert !old_gen.map_uses_pointer_header()
	assert old_gen.map_internal_field('len') == 'len'
	assert old_gen.map_internal_field('key_values') == 'key_values'

	mut pointer_table := ast.new_table()
	pointer_table.type_symbols[ast.map_type_idx].info = ast.Struct{
		fields: [ast.StructField{
			name: 'data'
			typ: ast.voidptr_type
		}]
	}
	pointer_gen := map_layout_test_gen(mut pointer_table)
	assert pointer_gen.map_uses_pointer_header()
	assert pointer_gen.map_internal_field('len') == 'data->count'
	assert pointer_gen.map_internal_field('key_values') == 'data->key_values'
}
