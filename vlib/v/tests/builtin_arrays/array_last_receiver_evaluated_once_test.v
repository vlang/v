struct LastOnceItem {
	id int
}

struct LastOnceSource {
mut:
	calls int
	items []LastOnceItem
}

fn (mut s LastOnceSource) next_items() []LastOnceItem {
	s.calls++
	return [LastOnceItem{s.calls}, LastOnceItem{s.calls + 10}]
}

fn (mut s LastOnceSource) stored_items() &[]LastOnceItem {
	s.calls++
	return &s.items
}

fn last_once_numbers(mut s LastOnceSource) []int {
	s.calls++
	return [s.calls, s.calls + 10]
}

// `last()` reads its receiver for the array and for its length; a receiver
// that is a call must still run only once.
fn test_last_evaluates_a_function_call_receiver_once() {
	mut source := LastOnceSource{}
	assert last_once_numbers(mut source).last() == 11
	assert source.calls == 1
	assert last_once_numbers(mut source).first() == 2
	assert source.calls == 2
}

fn test_last_evaluates_a_method_call_receiver_once() {
	mut source := LastOnceSource{}
	last := source.next_items().last()
	assert last.id == 11
	assert source.calls == 1
	assert source.next_items().last().id == 12
	assert source.calls == 2
}

fn test_last_evaluates_a_reference_returning_receiver_once() {
	mut source := LastOnceSource{
		items: [LastOnceItem{1}, LastOnceItem{2}]
	}
	assert source.stored_items().last().id == 2
	assert source.calls == 1
}

struct LastOnceRows {
mut:
	calls int
}

fn (mut rows LastOnceRows) [] (index int) []int {
	rows.calls++
	return [index, rows.calls]
}

fn test_last_evaluates_an_overloaded_index_receiver_once() {
	mut rows := LastOnceRows{}
	assert rows[0].last() == 1
	assert rows.calls == 1
	assert rows[1].last() == 2
	assert rows.calls == 2
}
