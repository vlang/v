fn double() (bool, [4]i16) {
	return true, [4]i16{}
}

fn foo() [4]i16 {
	_, b := double()
	for i in b {
		println(i)
	}
	return b
}

fn test_main() {
	arr := foo()
	assert arr == [4]i16{}
}

struct Sel {
	hashes [4]u32
}

fn (s &Sel) ancestor_hashes() [4]u32 {
	return s.hashes
}

// Iterating a fixed array returned *directly* from a call must not double the
// `.ret_arr` access (regression: produced `call(...).ret_arr.ret_arr[i]`).
fn test_for_in_fixed_array_return_from_call() {
	s := Sel{
		hashes: [u32(1), 2, 0, 4]!
	}
	mut nonzero := 0
	mut sum := u32(0)
	for hash in s.ancestor_hashes() {
		if hash != 0 {
			nonzero++
		}
		sum += hash
	}
	assert nonzero == 3
	assert sum == 7
}
