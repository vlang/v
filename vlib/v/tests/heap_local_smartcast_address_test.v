// The address of a smartcast local is the address of the narrowed value inside it. That
// holds too when the local was moved to the heap because the address is kept.
type Value = []Value | int | string

fn grow_guard_binding(mut table map[string]Value, key string, item int) {
	unsafe {
		if val := table[key] {
			if val is []Value {
				mut arr := &val
				arr << Value(item)
				table[key] = arr
			}
		}
	}
}

fn grow_local(mut table map[string]Value, key string, item int) {
	unsafe {
		val := table[key] or { return }
		if val is []Value {
			mut arr := &val
			arr << Value(item)
			table[key] = arr
		}
	}
}

fn items_of(table map[string]Value, key string) []int {
	mut items := []int{}
	val := table[key] or { return items }
	if val is []Value {
		for item in val {
			if item is int {
				items << item
			}
		}
	}
	return items
}

fn test_address_of_smartcast_guard_binding() {
	mut table := map[string]Value{}
	table['a'] = Value([]Value{})
	grow_guard_binding(mut table, 'a', 7)
	grow_guard_binding(mut table, 'a', 8)
	grow_guard_binding(mut table, 'missing', 9)
	assert items_of(table, 'a') == [7, 8]
	assert 'missing' !in table
}

fn test_address_of_smartcast_local() {
	mut table := map[string]Value{}
	table['a'] = Value([]Value{})
	table['n'] = Value(1)
	grow_local(mut table, 'a', 3)
	grow_local(mut table, 'a', 4)
	grow_local(mut table, 'n', 5)
	assert items_of(table, 'a') == [3, 4]
	assert table['n'] or { Value(0) } == Value(1)
}
