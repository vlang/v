fn test_gated_array() {
	a := [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
	assert a#[-1..] == [9]
	assert a#[..-9] == [0]
	assert a#[-9..-7] == [1, 2]
	assert a#[-2..] == [8, 9]

	// fixed array
	a1 := [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]!
	assert a1#[-1..] == [9]
	assert a1#[..-9] == [0]
	assert a1#[-9..-7] == [1, 2]
	assert a1#[-2..] == [8, 9]

	// empty array
	assert a#[-3..-4] == []   // start > end
	assert a#[20..] == []     // start > array.len
	assert a#[-20..-10] == [] // start+len < 0
	assert a#[20..-9] == []   // start > end  && start > end
}

fn test_gated_string() {
	a := "0123456789"
	assert a#[-1..] == "9"
	assert a#[..-9] == "0"
	assert a#[-9..-7] == "12"
	assert a#[-2..] == "89"

	// empty string
	assert a#[-3..-4] == ""   // start > end
	assert a#[20..] == ""     // start > array.len
	assert a#[-20..-10] == "" // start+len < 0
	assert a#[20..-9] == ""   // start > end  && start > end
}