fn filter_in_map_test() {
	x := [['']]
	y := x.map(it.filter(it != ''))
	assert y[0].len == 0
}