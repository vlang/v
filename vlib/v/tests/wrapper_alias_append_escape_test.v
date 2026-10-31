// A pointer alias declared inside the block or branch that gives another alias its
// value (V1 does not parse a declaration inside an `unsafe` value block).

@[noinline]
fn use_the_stack(n int) int {
	mut buf := [64]u64{}
	for i in 0 .. 64 {
		buf[i] = u64(i + n)
	}
	return if n > 0 { use_the_stack(n - 1) + int(buf[n % 64]) } else { 0 }
}

fn append_through_wrapper_aliases(mut values []&u64, data u64, c bool) {
	first := data
	p := unsafe {
		q := &first
		q
	}
	values << p
	second := data + 1
	r := if c {
		s := &second
		s
	} else {
		unsafe { nil }
	}
	values << r
}

// The appended pointer can be an alias declared inside the block or branch that gives
// another alias its value.
fn test_address_appended_through_an_alias_declared_in_a_value_wrapper() {
	mut values := []&u64{}
	append_through_wrapper_aliases(mut values, 50, true)
	_ = use_the_stack(10)
	assert values.map(*it) == [u64(50), 51]
}

struct AddressHolder {
	p &u64 = unsafe { nil }
	q &u64 = unsafe { nil }
}

fn hold_through_a_field_wrapper(data u64) AddressHolder {
	local := data
	h := AddressHolder{
		p: unsafe {
			q := &local
			q
		}
	}
	return h
}

fn hold_through_a_struct_update(data u64) AddressHolder {
	local := data
	other := data + 1
	h := AddressHolder{
		p: &local
	}
	updated := AddressHolder{
		...h
		q: &other
	}
	return updated
}

fn append_from_lock_values(mut values []&u64, data u64) {
	shared guard := [1]int{}
	first := data
	values << rlock guard {
		&first
	}
	second := data + 1
	p := rlock guard {
		&second
	}
	values << p
}

fn append_from_comptime_values[T](mut values []&u64, data u64) {
	local := data
	values << $if T is int {
		&local
	} $else {
		unsafe { nil }
	}
}

fn append_from_dump(mut values []&u64, data u64) {
	local := data
	values << dump(&local)
}

// The address can be retained through a wrapper in a struct field, a struct update, a
// lock body, a compile-time branch or `dump`.
fn test_addresses_retained_through_nested_value_wrappers() {
	held := hold_through_a_field_wrapper(60)
	updated := hold_through_a_struct_update(70)
	mut values := []&u64{}
	append_from_lock_values(mut values, 80)
	append_from_comptime_values[int](mut values, 90)
	append_from_dump(mut values, 95)
	_ = use_the_stack(10)
	assert *held.p == 60
	assert *updated.p == 70
	assert *updated.q == 71
	assert values.map(*it) == [u64(80), 81, 90, 95]
}
