// Regression test for https://github.com/vlang/v/issues/27012
// A nested `or { call_returning_result_void() or { ... } }` used as a
// statement (outer call result discarded) used to emit invalid C of the
// form `*(T*) _t.data = ;` and fail to compile.

struct Item {
	id int
}

fn get_item() !Item {
	return Item{
		id: 1
	}
}

fn get_item_err() !Item {
	return error('outer')
}

fn do_void() ! {
	return error('inner')
}

fn test_nested_or_block_with_void_result_inner_succeeds() {
	mut traces := []string{}
	get_item() or {
		traces << 'outer: ${err}'
		do_void() or { traces << 'inner: ${err}' }
	}
	// Outer call succeeded -> or-block is never entered.
	assert traces.len == 0
}

fn test_nested_or_block_with_void_result_outer_errors() {
	mut traces := []string{}
	get_item_err() or {
		traces << 'outer: ${err}'
		do_void() or { traces << 'inner: ${err}' }
	}
	assert traces.len == 2
	assert traces[0] == 'outer: outer'
	assert traces[1] == 'inner: inner'
}
