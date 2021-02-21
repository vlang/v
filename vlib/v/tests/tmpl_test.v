fn one() string {
	name := 'Peter'
	age := 25
	numbers := [1, 2, 3]
	return $tmpl('tmpl/1.txt')
}

fn test_tmpl() {
	assert one().trim_space() == 'name: Peter

age: 25

numbers: [1, 2, 3]


1

2

3'
}

fn test_tmpl_in_anon_fn() {
	anon := fn (name string, age int, numbers []int) string {
		return $tmpl('tmpl/1.txt')
	}

	println(anon('Peter', 25, [1, 2, 3]))
	assert anon('Peter', 25, [1, 2, 3]).trim_space() == 'name: Peter

age: 25

numbers: [1, 2, 3]


1

2

3'
}
