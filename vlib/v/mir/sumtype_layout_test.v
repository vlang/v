module mir

import v.ssa

fn test_tagged_union_layout_matches_ssa() {
	mut store := ssa.TypeStore.new()
	tag := store.get_int(32)
	value := store.get_int(64)
	pair := store.register(ssa.Type{ kind: .struct_t, fields: [value, value] })
	fixed := store.get_array(value, 3)
	payload := store.register(ssa.Type{
		kind:     .struct_t
		fields:   [value, pair, fixed]
		is_union: true
	})
	sum := store.register(ssa.Type{ kind: .struct_t, fields: [tag, payload] })
	m := Module{ type_store: store }
	assert m.type_size(payload) == 24
	assert m.type_size(sum) == 32
	assert m.type_align(sum) == 8
}
