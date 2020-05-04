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
