// Method attributes are compile-time data: iterating them inside a
// `$for method in T.methods` loop must not build a runtime array (a heap
// allocation on every pass), and their count and contents must be usable in
// comptime conditions.
struct Routes {}

@['GET /users']
@[inline]
fn (r Routes) list() int {
	return 1
}

@['GET /users/:id']
@['PUT /users/:id']
fn (r Routes) item() int {
	return 2
}

fn (r Routes) helper() int {
	return 3
}

fn test_for_in_method_attrs_visits_every_attribute_in_order() {
	mut seen := []string{}
	$for method in Routes.methods {
		for attr in method.attrs {
			seen << '${method.name}:${attr}'
		}
	}
	assert seen == ['list:GET /users', 'list:inline', 'item:GET /users/:id', 'item:PUT /users/:id']
}

fn test_for_in_method_attrs_with_index() {
	mut seen := []string{}
	$for method in Routes.methods {
		for i, attr in method.attrs {
			seen << '${method.name}${i}=${attr}'
		}
	}
	assert seen == ['list0=GET /users', 'list1=inline', 'item0=GET /users/:id', 'item1=PUT /users/:id']
}

fn test_method_attrs_len_and_contains() {
	mut lens := []int{}
	mut inline := []string{}
	$for method in Routes.methods {
		lens << method.attrs.len
		if method.attrs.contains('inline') {
			inline << method.name
		}
	}
	assert lens == [2, 2, 0]
	assert inline == ['list']
}

fn test_break_and_continue_in_method_attrs_loop() {
	mut first := []string{}
	mut puts := []string{}
	$for method in Routes.methods {
		for attr in method.attrs {
			first << attr
			break
		}
		for attr in method.attrs {
			if !attr.starts_with('PUT ') {
				continue
			}
			puts << attr
		}
	}
	assert first == ['GET /users', 'GET /users/:id']
	assert puts == ['PUT /users/:id']
}

fn test_method_attrs_in_comptime_conditions() {
	mut routed := []string{}
	mut puts := []string{}
	$for method in Routes.methods {
		$if method.attrs.len > 0 {
			routed << method.name
		}
		$if 'PUT /users/:id' in method.attrs {
			puts << method.name
		}
	}
	assert routed == ['list', 'item']
	assert puts == ['item']
}

fn count_route_attrs() int {
	mut n := 0
	$for method in Routes.methods {
		for attr in method.attrs {
			if attr.len > 4 && attr[3] == ` ` {
				n++
			}
		}
	}
	return n
}

fn test_iterating_method_attrs_allocates_nothing() {
	assert count_route_attrs() == 3
	$if gcboehm ? {
		before := gc_heap_usage().total_bytes
		mut n := 0
		for _ in 0 .. 10_000 {
			n += count_route_attrs()
		}
		assert n == 30_000
		assert gc_heap_usage().total_bytes - before < 1024
	}
}
