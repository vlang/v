struct Init {
	len int
}

fn test_array_init() {
	b := [1, 2, 3]
	mut a := []int{cap: b.len}
	a << 1
	'$a, $a.len, $a.cap' == '[1], 1, 3'

	c := Init{len: 3}
	mut d := []string{cap: c.len}
	d << 'aaa'
	d << 'bbb'
	'$d, $d.len, $d.cap' == "['aaa', 'bbb'], 2, 3"
}

fn test_array_init_with_default() {
	a1 := []int{len: 4, init: 2}
	assert '$a1' == '[2, 2, 2, 2]'

	a2 := []int{len: 3, init: 12345}
	assert '$a2' == '[12345, 12345, 12345]'

	b1 := []string{len: 3, init: 'abc'}
	assert '$b1' == "['abc', 'abc', 'abc']"

	b2 := []string{len: 2, init: '111'}
	assert '$b2' == "['111', '111']"
}

fn test_array_init_with_len_no_default() {
	a1 := []int{len: 4}
	assert '$a1' == '[0, 0, 0, 0]'

	a2 := []string{len: 4}
	assert '$a2' == "['', '', '', '']"

	a3 := []bool{len: 3}
	assert '$a3' == '[false, false, false]'
}
