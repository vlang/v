type IndexedHandler = fn (int) int

type IndexedTablePointer = &IndexedHandler

struct IndexedCounter {
mut:
	count int
}

struct IndexedFunctionTable {
	handlers &IndexedHandler
}

fn indexed_add(value int) int { return value + 1 }

fn indexed_multiply(value int) int { return value * 10 }

fn indexed_callbacks[T](value T) T { return value }

fn indexed_call_parameter(table &IndexedHandler, index int, value int) int {
	return unsafe { table[index](value) }
}

fn indexed_next(mut calls IndexedCounter) int {
	calls.count++
	return 1
}

fn indexed_table_once(table &IndexedHandler, mut calls IndexedCounter) &IndexedHandler {
	calls.count++
	return table
}

fn (table IndexedFunctionTable) pick[T](value T) T {
	return value
}

fn test_raw_function_pointer_table_preserves_indexes() {
	callbacks := [IndexedHandler(indexed_add), IndexedHandler(indexed_multiply)]!
	table := unsafe { &IndexedHandler(&callbacks[0]) }
	index := 1
	assert unsafe { table[0](7) } == 8
	assert unsafe { table[index](7) } == 70
	assert unsafe { table[index - 1](7) } == 8
	assert indexed_call_parameter(table, 0, 4) == 5
	assert indexed_call_parameter(table, 1, 4) == 40
	mut sum := 0
	for i in 0 .. 2 { sum += unsafe { table[i](3) } }
	assert sum == 34
}

fn test_indexed_pointer_selector_and_alias_keep_callable_values() {
	callbacks := [IndexedHandler(indexed_add), IndexedHandler(indexed_multiply)]!
	pointer := unsafe { &IndexedHandler(&callbacks[0]) }
	alias := IndexedTablePointer(pointer)
	index := 1
	assert unsafe { alias[index](5) } == 50
	table := IndexedFunctionTable{ handlers: pointer }
	assert unsafe { table.handlers[index](6) } == 60
	assert unsafe { table.handlers[0](6) } == 7
	assert table.pick[int](17) == 17
	assert table.pick[string]('generic method') == 'generic method'
}

fn test_indexed_function_pointer_side_effects_run_once() {
	callbacks := [IndexedHandler(indexed_add), IndexedHandler(indexed_multiply)]!
	table := unsafe { &IndexedHandler(&callbacks[0]) }
	mut calls := IndexedCounter{}
	assert unsafe { table[indexed_next(mut calls)](4) } == 40
	assert calls.count == 1
	mut bases := IndexedCounter{}
	assert unsafe { indexed_table_once(table, mut bases)[indexed_next(mut calls)](5) } == 50
	assert bases.count == 1
	assert calls.count == 2
}

fn test_pointer_local_shadows_generic_function_only_in_its_scope() {
	callbacks := [IndexedHandler(indexed_add), IndexedHandler(indexed_multiply)]!
	{
		indexed_callbacks := unsafe { &IndexedHandler(&callbacks[0]) }
		index := 1
		assert unsafe { indexed_callbacks[index](8) } == 80
		assert unsafe { indexed_callbacks[0](8) } == 9
		selected := unsafe { indexed_callbacks[index] }
		assert selected(6) == 60
	}
	assert indexed_callbacks[int](23) == 23
	assert indexed_callbacks[string]('generic function') == 'generic function'
}

fn indexed_indirect(table &&IndexedHandler) &&IndexedHandler {
	return table
}

fn indexed_read_pointer(value &int) int {
	return unsafe { *value }
}

fn test_callable_pointer_indirection_is_distinct_from_pointer_arguments() {
	callbacks := [IndexedHandler(indexed_add), IndexedHandler(indexed_multiply)]!
	table := unsafe { &IndexedHandler(&callbacks[0]) }
	indirect := &table
	assert unsafe { indexed_indirect(indirect)[0][1](9) } == 90
	read := fn (value &int) int { return indexed_read_pointer(value) }
	value := 31
	assert read(&value) == 31
}

fn indexed_pointer_factory[T](value &T) &T {
	return value
}

fn test_explicit_generic_function_can_still_return_a_pointer() {
	value := 47
	pointer := indexed_pointer_factory[int](&value)
	assert unsafe { *pointer } == 47
}
