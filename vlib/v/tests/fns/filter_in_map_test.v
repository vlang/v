fn test_filter_in_map() {
	x := [['']]
	y := x.map(it.filter(it != ''))
	assert y[0].len == 0
}
