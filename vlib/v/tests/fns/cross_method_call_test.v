fn test_main() {
	mut l_map := map[int]bool{}
	mut r_map := map[int]bool{}

	l_map[0] = false
	l_map[1] = false

	r_map[0] = true
	r_map[1] = true

	l_map, r_map = r_map.move(), l_map.move()

	assert l_map[0] == true
	assert l_map[0] == true
	assert r_map[0] == false
	assert r_map[0] == false
}
