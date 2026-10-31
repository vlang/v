// Locals moved to the heap because their address escapes, in the forms review found:
// through `!`/`?` unwraps, in compile-time `$if` and `$for` bodies, in `select` cases, and
// fixed arrays declared through an alias. A later local of the same name that is not moved
// (a pointer) must keep its own address.

@[noinline]
fn use_the_stack(n int) int {
	mut buf := [64]u64{}
	for i in 0 .. 64 {
		buf[i] = u64(i + n)
	}
	return if n > 0 { use_the_stack(n - 1) + int(buf[n % 64]) } else { 0 }
}

fn set_out(p &&u64, v &u64) {
	unsafe {
		*p = v
	}
}

fn pointer_result(p &u64) !&u64 {
	return unsafe { p }
}

fn pointer_option(p &u64) ?&u64 {
	return unsafe { p }
}

fn append_result_unwrap(mut values []&u64, data u64) ! {
	from_result := data
	values << pointer_result(&from_result)!
}

fn append_option_unwrap(mut values []&u64, data u64) ? {
	from_option := data
	values << pointer_option(&from_option)?
}

struct Pair {
	a int
	b int
}

fn in_comptime_if[T](mut values []&u64, target &u64) u64 {
	$if T is int {
		x := u64(5)
		values << &x
	}
	mut x := unsafe { &u64(nil) }
	set_out(&x, target)
	return *x
}

fn in_comptime_for(mut values []&u64, target &u64) u64 {
	$for field in Pair.fields {
		x := u64(field.name.len)
		values << &x
	}
	mut x := unsafe { &u64(nil) }
	set_out(&x, target)
	return *x
}

fn in_select(mut values []&u64, ch chan u64, target &u64) u64 {
	select {
		v := <-ch {
			x := v
			values << &x
		}
	}
	mut x := unsafe { &u64(nil) }
	set_out(&x, target)
	return *x
}

type Buffer = [16]u8

fn aliased_fixed_array(mut values []&u8) int {
	mut buf := Buffer{}
	buf[0] = 7
	values << unsafe { &buf[0] }
	return int(sizeof(buf))
}

fn test_heap_locals_in_the_forms_review_found() {
	mut values := []&u64{}
	target := u64(99)
	append_result_unwrap(mut values, 10) or { panic(err) }
	append_option_unwrap(mut values, 11) or { panic('none') }
	assert in_comptime_if[int](mut values, &target) == 99
	assert in_comptime_for(mut values, &target) == 99
	ch := chan u64{cap: 1}
	ch <- 20
	assert in_select(mut values, ch, &target) == 99
	mut bytes := []&u8{}
	assert aliased_fixed_array(mut bytes) == 16
	_ = use_the_stack(10)
	assert values.map(*it) == [u64(10), 11, 5, 1, 1, 20]
	assert *bytes[0] == 7
}
