struct AppendMapValue {
mut:
	n int
	m map[string]i16
}

struct NestedAppendMapValue {
mut:
	inner AppendMapValue
}

fn test_append_struct_keeps_empty_map_snapshot() {
	mut value := AppendMapValue{ n: 1 }
	mut before := []AppendMapValue{}
	before << value
	value.n++
	value.m['modified'] = 1
	assert before[0].n == 1
	assert before[0].m.len == 0
	assert value.n == 2
	assert value.m['modified'] == 1
}

fn test_append_dereferenced_struct_keeps_empty_map_snapshot() {
	mut value := &AppendMapValue{ n: 1 }
	mut before := []AppendMapValue{}
	before << *value
	value.n++
	value.m['modified'] = 1
	assert before[0].n == 1
	assert before[0].m.len == 0
	assert value.n == 2
	assert value.m['modified'] == 1
}

fn test_append_nested_struct_keeps_empty_map_snapshot() {
	mut value := NestedAppendMapValue{
		inner: AppendMapValue{ n: 1 }
	}
	mut before := []NestedAppendMapValue{}
	before << value
	value.inner.n++
	value.inner.m['modified'] = 1
	assert before[0].inner.n == 1
	assert before[0].inner.m.len == 0
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
