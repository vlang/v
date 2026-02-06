fn test_main() {
	a := [?int(1), 0, 2]!
	b := [?int(none), none]!
	assert '${a}' == '${[?int(1), 0, 2]!}'
	assert '${b}' == '${[?int(none), none]!}'
}
