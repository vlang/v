fn marshal[T](val T) string {
	return marshal_map(val)
}

fn marshal_map[T](val map[string]T) string {
	return typeof(val).name
}

fn test_main() {
	assert marshal({
		'a': 1
	}) == 'map[string]int'
	assert marshal({
		'a': true
	}) == 'map[string]bool'
	assert marshal({
		'a': 1.2
	}) == 'map[string]f64'
}
