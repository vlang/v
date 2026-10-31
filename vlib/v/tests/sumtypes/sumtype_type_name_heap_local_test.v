// `t = &val` moves `val` to the heap, so it is declared as a pointer and read as `*val`.
// Its `type_name()` and `type_idx()` must still read the sum value, not the pointer.
type Value = int | string | map[string]Value

fn find(mut table map[string]Value, keys []string) !&map[string]Value {
	mut t := unsafe { &table }
	unsafe {
		for k in keys {
			if val := t[k] {
				if val is map[string]Value {
					t = &val
				} else {
					return error('`${k}` is not a map but `${val.type_name()}`')
				}
			}
		}
	}
	return t
}

fn heap_local_type_idx(v Value) int {
	mut val := v
	p := &val
	assert p.type_name() == val.type_name()
	return val.type_idx()
}

fn test_type_name_of_a_heap_escaped_sum_type_local() {
	mut m := map[string]Value{}
	m['a'] = Value(5)
	m['b'] = Value('x')
	m['c'] = Value(map[string]Value{})
	if _ := find(mut m, ['a']) {
		assert false
	} else {
		assert err.msg() == '`a` is not a map but `int`'
	}
	if _ := find(mut m, ['b']) {
		assert false
	} else {
		assert err.msg() == '`b` is not a map but `string`'
	}
	found := find(mut m, ['c']) or { panic(err) }
	assert found.len == 0
}

fn test_type_idx_of_a_heap_escaped_sum_type_local() {
	assert heap_local_type_idx(Value(1)) == Value(2).type_idx()
	assert heap_local_type_idx(Value('s')) == Value('t').type_idx()
	assert heap_local_type_idx(Value(1)) != heap_local_type_idx(Value('s'))
}
