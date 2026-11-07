module ssa

import v.flat
import v.types

fn test_pointer_base_source_ids_strip_only_leading_pointer_syntax() {
	for spelling in ['', 'pkg.Item', ' &pkg.Item', '?&pkg.Item', '[]&pkg.Item'] {
		type_id := flat.source_type_id(spelling)
		assert ssa_pointer_base_source_type(type_id) == type_id
	}
	assert ssa_pointer_base_source_type(flat.source_type_id('&&pkg.Item')) == flat.source_type_id('pkg.Item')
	assert ssa_pointer_base_source_type(flat.source_type_id('&')) == 0
}

fn test_numeric_primitive_dispatch_keeps_exact_annotation_spelling() {
	mut m := Module.new()
	m.target = TargetData{ ptr_size: 8 }
	mut b := Builder{
		m:         m
		i32_type:  TypeID(41)
		i64_type:  TypeID(43)
		void_type: TypeID(42)
	}
	assert (b.primitive_type_id(flat.source_type_int) or { TypeID(-1) }) == TypeID(43)
	assert (b.primitive_type_id(u32(0)) or { TypeID(-1) }) == TypeID(42)
	assert (b.primitive_type_id(flat.source_type_void) or { TypeID(-1) }) == TypeID(42)
	assert b.primitive_type_id(flat.source_type_id(' int ')) == none
	assert b.primitive_type_id(flat.source_type_id('pkg.int')) == none
}

fn test_function_type_ids_follow_aliases_and_stop_at_cycles() {
	a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.type_aliases.set_id(flat.source_type_id('Callable'), flat.source_type_id('fn (int) string'))
	tc.type_aliases.set_id(flat.source_type_id('Alias'), flat.source_type_id('Callable'))
	tc.type_aliases.set_id(flat.source_type_id('Cycle'), flat.source_type_id('Cycle'))
	b := Builder{ tc: &tc }
	assert b.is_function_type_name(flat.source_type_id('Alias'))
	assert !b.is_function_type_name(flat.source_type_id('Cycle'))
	assert !b.is_function_type_name(flat.source_type_id(' fn (int) string '))
	assert !b.is_function_type_name(flat.source_type_id('pkg.Unknown'))
}

fn test_native_int_and_isize_follow_target_pointer_width() {
	for width in [4, 8] {
		mut m := Module.new()
		m.target = TargetData{ ptr_size: width }
		mut b := Builder{
			m:        m
			i32_type: m.type_store.get_int(32)
			i64_type: m.type_store.get_int(64)
		}
		for source in [flat.source_type_int, flat.source_type_isize] {
			typ := b.primitive_type_id(source) or { panic('missing integer type') }
			assert m.type_store.types[typ].width == width * 8
		}
		assert (b.primitive_type_id(flat.source_type_i32) or { TypeID(-1) }) == b.i32_type
	}
}
