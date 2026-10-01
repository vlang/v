// A `shared` fixed-array field is pointer-backed wrapper storage, not an inline
// array member, so an omitted one has to be initialized inside the compound
// literal, where the wrapper is allocated. Copying it afterwards, the way a plain
// fixed-array default is copied, would memcpy into a null pointer.
//
// Note: a `shared` fixed-array field with no default is not zero-initialized, on
// master as much as here. That is a separate bug, so nothing below relies on it.
struct Holder {
mut:
	plain [2]int        = [1, 2]!
	vals  shared [2]int = [3, 4]!
	other int
}

fn test_an_omitted_shared_fixed_array_default_is_applied() {
	h := Holder{}
	assert h.plain == [1, 2]!
	rlock h.vals {
		assert h.vals[0] == 3
		assert h.vals[1] == 4
	}
}

fn test_a_shared_fixed_array_default_survives_locking_and_mutation() {
	h := Holder{}
	lock h.vals {
		h.vals[0] = 9
	}
	rlock h.vals {
		assert h.vals[0] == 9
		assert h.vals[1] == 4
	}
}

fn test_a_plain_fixed_array_default_beside_a_shared_one() {
	h := Holder{
		other: 5
	}
	assert h.other == 5
	assert h.plain[1] == 2
	rlock h.vals {
		assert h.vals[1] == 4
	}
}
