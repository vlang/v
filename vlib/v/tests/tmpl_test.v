fn one() string {
	name := 'Peter'
	age := 25
	numbers := [1, 2, 3]
	return $tmpl('vlib/v/tests/tmpl/1.txt')
}

fn test_tmpl() {
	assert one().trim_space() == 'name: Peter

age: 25

numbers: [1, 2, 3]


1

2

3'
}
