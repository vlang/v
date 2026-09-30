fn append_address(mut values []&char, data int) {
	if data != 0 {
		num := u64(data) * 3
		values << &char(&num)
	}
}

@[noinline]
fn use_the_stack(n int) int {
	mut buf := [64]u64{}
	for i in 0 .. 64 {
		buf[i] = u64(i + n)
	}
	return if n > 0 { use_the_stack(n - 1) + int(buf[n % 64]) } else { 0 }
}

// The address of a local appended to an array outlives the function: the local is
// allocated on the heap (db.pg's ORM binds its parameters this way).
fn test_address_of_a_local_appended_to_a_mut_array_parameter() {
	mut values := []&char{}
	append_address(mut values, 31)
	append_address(mut values, 45)
	_ = use_the_stack(10)
	assert unsafe { *(&u64(values[0])) } == 93
	assert unsafe { *(&u64(values[1])) } == 135
}

type Scalar = i16 | i64 | string

fn append_scalar(mut values []&char, data Scalar) {
	match data {
		i16 {
			num := u16(data)
			values << &char(&num)
		}
		i64 {
			num := u64(data)
			values << &char(&num)
		}
		string {
			values << &char(data.str)
		}
	}
}

// Each branch declares its own `num`: every one of them is moved to the heap.
fn test_same_named_locals_in_match_branches() {
	mut values := []&char{}
	append_scalar(mut values, Scalar(i64(45)))
	append_scalar(mut values, Scalar(i16(7)))
	append_scalar(mut values, Scalar(i64(31)))
	_ = use_the_stack(10)
	assert unsafe { *(&u64(values[0])) } == 45
	assert unsafe { *(&u16(values[1])) } == 7
	assert unsafe { *(&u64(values[2])) } == 31
}
