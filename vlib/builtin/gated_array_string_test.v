fn test_gated_arrays() {
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
	assert a#[-3..-4] == [] // start > end
	assert a#[20..] == [] // start > array.len
	assert a#[-20..-10] == [] // start+len < 0
	assert a#[20..-9] == [] // start > end  && start > end
}

fn test_gated_strings() {
	a := '0123456789'
	assert a#[-1..] == '9'
	assert a#[..-9] == '0'
	assert a#[-9..-7] == '12'
	assert a#[-2..] == '89'

	// empty string
	assert a#[-3..-4] == '' // start > end
	assert a#[20..] == '' // start > array.len
	assert a#[-20..-10] == '' // start+len < 0
	assert a#[20..-9] == '' // start > end  && start > end

	//
	// test negative indexes in slices from github discussion
	//
	s := '0123456789'

	// normal behaviour
	assert s#[1..3] == '12'
	assert s#[..3] == '012'
	assert s#[8..] == '89'

	// negative indexes behaviour
	assert s#[-2..] == '89'
	assert s#[..-8] == '01'
	assert s#[2..-2] == '234567'
	assert s#[-12..-16] == ''
	assert s#[-8..-2] == '234567'

	// out of bound both indexes
	assert s#[12..14] == ''
	assert s#[-12..16] == '0123456789'
}

fn test_gated_mixed_strings() {
	//
	// test negative indexes in slices
	//
	a := [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]

	// normal behaviour
	assert a#[1..3].str() == '[1, 2]'
	assert a#[..3].str() == '[0, 1, 2]'
	assert a#[8..].str() == '[8, 9]'

	// negative indexes behaviour
	assert a#[-2..].str() == '[8, 9]'
	assert a#[..-8].str() == '[0, 1]'
	assert a#[2..-2].str() == '[2, 3, 4, 5, 6, 7]'
	assert a#[-12..-16].str() == '[]'
	assert a#[-8..-2].str() == '[2, 3, 4, 5, 6, 7]'

	// out of bound both indexes
	assert a#[12..14].str() == '[]'
	assert a#[-12..16].str() == a.str()
}
