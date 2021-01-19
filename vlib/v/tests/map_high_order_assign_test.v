fn test_high_order_map_assign() {
	mut m := map[string]map[string]int
	m['hello']['hi'] = 1
	println(m)
	assert '$m' == "{'hello': {'hi': 1}}"
}
