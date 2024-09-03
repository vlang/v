fn test_main() {
	a := chan [2]int{cap: 10}
	c := [1, 2]!
	dump(a.try_push(&c))
	mut d := [0, 0]!
	dump(a.try_pop(mut d))
	assert dump(d) == [1, 2]!
}
