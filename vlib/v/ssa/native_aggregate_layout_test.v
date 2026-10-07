module ssa

fn test_native_aggregate_layout_has_no_field_count_limit() {
	mut m := Module.new()
	i8_type := m.type_store.get_int(8)
	i64_type := m.type_store.get_int(64)
	for field_count in [256, 257, 288, 300] {
		mut fields := []TypeID{len: field_count, init: i64_type}
		fields[0] = i8_type
		structure := m.type_store.register(Type{ kind: .struct_t, fields: fields })
		assert m.type_size(structure) == field_count * 8
		assert m.type_align(structure) == 8
		assert m.struct_field_offset(structure, field_count - 1) == (field_count - 1) * 8
	}
	mut packed_fields := [i8_type]
	packed_fields << []TypeID{len: 256, init: i64_type}
	packed := m.type_store.register(Type{
		kind:      .struct_t
		fields:    packed_fields
		is_packed: true
	})
	assert m.type_size(packed) == 2049
	assert m.struct_field_offset(packed, 256) == 2041
	wide_field := m.type_store.get_array(i64_type, 4)
	mut union_fields := []TypeID{len: 300, init: i8_type}
	union_fields[299] = wide_field
	union_type := m.type_store.register(Type{
		kind:     .struct_t
		fields:   union_fields
		is_union: true
	})
	assert m.type_size(union_type) == 32
	assert m.struct_field_offset(union_type, 299) == 0
}

fn test_native_many_mixed_fields_keep_layout_after_freezing() {
	mut m := Module.new()
	i1_type := m.type_store.get_int(1)
	i8_type := m.type_store.get_int(8)
	i32_type := m.type_store.get_int(32)
	i64_type := m.type_store.get_int(64)
	pointer := m.type_store.get_ptr(i8_type)
	array_header := m.type_store.get_tuple([pointer, i32_type, i32_type, i32_type, i32_type, i32_type])
	mut fields := []TypeID{cap: 300}
	for _ in 0 .. 75 {
		fields << [i64_type, array_header, pointer, i1_type]
	}
	structure := m.type_store.register(Type{ kind: .struct_t, fields: fields })
	assert m.type_size(structure) == 4200
	assert m.struct_field_offset(structure, 299) == 4192
	m.freeze_type_layouts()
	assert m.type_size(structure) == 4200
	assert m.type_align(structure) == 8
	assert m.struct_field_offset(structure, 299) == 4192
	assert m.struct_field_size(structure, 299) == 1
}
