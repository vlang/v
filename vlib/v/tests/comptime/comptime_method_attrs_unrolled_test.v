// Method attributes are compile-time data. Inside `$for method in T.methods`,
// reading them must not build a runtime array (a heap allocation on every
// pass): `for attr in method.attrs` is unrolled, `.len` and `.contains` fold
// to constants. Their count and contents also work in comptime conditions.
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

// ── the values, as the array form gave them ─────────────────────────────────

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

fn route_attrs[T]() []string {
	mut seen := []string{}
	$for method in T.methods {
		for attr in method.attrs {
			if attr.starts_with('GET ') {
				seen << '${method.name} ${attr}'
			}
		}
	}
	return seen
}

fn test_for_in_method_attrs_in_a_generic_function() {
	assert route_attrs[Routes]() == ['list GET /users', 'item GET /users/:id']
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

struct Escaped {}

@['it\'s']
@[doc: 'it\'s']
@['tab\tx']
fn (e Escaped) one() {}

// `contains` folds against the decoded values the array form holds, not their source spelling.
fn test_method_attrs_contains_matches_decoded_attrs() {
	mut hits := []string{}
	$for method in Escaped.methods {
		if method.attrs.contains("it's") {
			hits << 'if'
		}
		if !method.attrs.contains("it\\'s") {
			hits << 'not raw'
		}
		quoted := method.attrs.contains("doc: 'it's'")
		tab := method.attrs.contains('tab\tx')
		assert quoted
		assert tab
		for attr in method.attrs {
			assert method.attrs.contains(attr)
		}
	}
	assert hits == ['if', 'not raw']
}

// break/continue keep the array form; they must still work.
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

// ── comptime conditions (they used to be silently false) ────────────────────

fn test_comptime_if_method_attrs_len_gt() {
	mut routed := []string{}
	$for method in Routes.methods {
		$if method.attrs.len > 0 {
			routed << method.name
		}
	}
	assert routed == ['list', 'item']
}

fn test_comptime_if_method_attrs_len_eq() {
	mut plain := []string{}
	$for method in Routes.methods {
		$if method.attrs.len == 0 {
			plain << method.name
		}
	}
	assert plain == ['helper']
}

fn test_comptime_if_attr_in_method_attrs() {
	mut puts := []string{}
	$for method in Routes.methods {
		$if 'PUT /users/:id' in method.attrs {
			puts << method.name
		}
	}
	assert puts == ['item']
}

fn test_comptime_if_attr_not_in_method_attrs() {
	mut not_inline := []string{}
	$for method in Routes.methods {
		$if 'inline' !in method.attrs {
			not_inline << method.name
		}
	}
	assert not_inline == ['item', 'helper']
}

fn test_comptime_if_method_attrs_contains() {
	mut inline := []string{}
	mut not_inline := []string{}
	$for method in Routes.methods {
		$if method.attrs.contains('inline') {
			inline << method.name
		}
		$if !method.attrs.contains('inline') {
			not_inline << method.name
		}
	}
	assert inline == ['list']
	assert not_inline == ['item', 'helper']
}

// An index past the last attribute reads as '', like a missing param name.
fn test_comptime_if_method_attrs_index() {
	mut first_get := []string{}
	mut at_most_two := []string{}
	$for method in Routes.methods {
		$if method.attrs[0] == 'GET /users/:id' {
			first_get << method.name
		}
		$if method.attrs[0] != '' && method.attrs[2] == '' {
			at_most_two << method.name
		}
	}
	assert first_get == ['item']
	assert at_most_two == ['list', 'item']
}

// A member access on an indexed attribute stays attached to the substituted value.
fn test_comptime_if_method_attrs_index_member_access() {
	mut gets := []string{}
	mut puts := []string{}
	mut plain := []string{}
	$for method in Routes.methods {
		$if method.attrs[0].starts_with('GET ') {
			gets << method.name
		}
		$if method.attrs[1].starts_with('PUT ') {
			puts << method.name
		}
		$if method.attrs[0].len == 0 {
			plain << method.name
		}
	}
	assert gets == ['list', 'item']
	assert puts == ['item']
	assert plain == ['helper']
}

// ── allocation: none per pass ───────────────────────────────────────────────

fn count_for_in() int {
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

fn count_for_in_indexed() int {
	mut n := 0
	$for method in Routes.methods {
		for i, attr in method.attrs {
			n += i + attr.len
		}
	}
	return n
}

fn count_len_and_contains() int {
	mut n := 0
	$for method in Routes.methods {
		n += method.attrs.len
		if method.attrs.contains('PUT /users/:id') {
			n += 10
		}
	}
	return n
}

fn count_generic[T]() int {
	mut n := 0
	$for method in T.methods {
		for attr in method.attrs {
			n += attr.len
		}
	}
	return n
}

// allocated returns the bytes the collector handed out over 10_000 calls of f.
fn allocated(f fn () int, want int) u64 {
	got := f() // outside the assert: -prod drops asserts
	assert got == want
	before := gc_heap_usage().total_bytes
	mut sum := 0
	for _ in 0 .. 10_000 {
		sum += f()
	}
	assert sum == want * 10_000
	return gc_heap_usage().total_bytes - before
}

fn test_for_in_method_attrs_allocates_nothing() {
	$if gcboehm ? {
		bytes := allocated(count_for_in, 3)
		assert bytes < 1024, 'count_for_in allocated ${bytes} bytes over 10_000 calls'
	}
}

fn test_for_in_method_attrs_with_index_allocates_nothing() {
	$if gcboehm ? {
		bytes := allocated(count_for_in_indexed, 46)
		assert bytes < 1024, 'count_for_in_indexed allocated ${bytes} bytes over 10_000 calls'
	}
}

fn test_method_attrs_len_and_contains_allocate_nothing() {
	$if gcboehm ? {
		bytes := allocated(count_len_and_contains, 14)
		assert bytes < 1024, 'count_len_and_contains allocated ${bytes} bytes over 10_000 calls'
	}
}

fn test_for_in_method_attrs_in_a_generic_function_allocates_nothing() {
	$if gcboehm ? {
		bytes := allocated(count_generic[Routes], 44)
		assert bytes < 1024, 'count_generic[Routes] allocated ${bytes} bytes over 10_000 calls'
	}
}
