struct AppendMapValue {
mut:
	n int
	m map[string]i16
}

struct NestedAppendMapValue {
mut:
	inner AppendMapValue
}

fn test_append_struct_shares_map_storage() {
	mut value := AppendMapValue{ n: 1 }
	mut before := []AppendMapValue{}
	before << value
	value.n++
	value.m['modified'] = 1
	assert before[0].n == 1
	assert before[0].m.len == 1
	assert before[0].m['modified'] == 1
	assert value.n == 2
	assert value.m['modified'] == 1
}

fn test_append_dereferenced_struct_shares_map_storage() {
	mut value := &AppendMapValue{ n: 1 }
	mut before := []AppendMapValue{}
	before << *value
	value.n++
	value.m['modified'] = 1
	assert before[0].n == 1
	assert before[0].m.len == 1
	assert before[0].m['modified'] == 1
	assert value.n == 2
	assert value.m['modified'] == 1
}

fn test_append_nested_struct_shares_map_storage() {
	mut value := NestedAppendMapValue{
		inner: AppendMapValue{ n: 1 }
	}
	mut before := []NestedAppendMapValue{}
	before << value
	value.inner.n++
	value.inner.m['modified'] = 1
	assert before[0].inner.n == 1
	assert before[0].inner.m.len == 1
	assert before[0].inner.m['modified'] == 1
	assert value.inner.n == 2
	assert value.inner.m['modified'] == 1
}

fn test_append_struct_pointer_keeps_reference_semantics() {
	mut value := &AppendMapValue{ n: 1 }
	mut references := []&AppendMapValue{}
	references << value
	value.n++
	value.m['modified'] = 1
	assert references[0].n == 2
	assert references[0].m['modified'] == 1
}

fn test_append_struct_with_explicit_map_clone_keeps_snapshot() {
	mut value := AppendMapValue{ n: 1, m: {
		'original': i16(1)
	} }
	mut before := []AppendMapValue{}
	before << AppendMapValue{ n: value.n, m: value.m.clone() }
	value.n++
	value.m['original'] = 2
	value.m['modified'] = 3
	assert before[0].n == 1
	assert before[0].m == {
		'original': i16(1)
	}
	assert value.m == {
		'original': i16(2)
		'modified': i16(3)
	}
}
