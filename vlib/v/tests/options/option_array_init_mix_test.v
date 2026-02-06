fn test_main() {
	a := [?string('a'), 'b']!
	println(a)
	assert '${a}' == "[Option('a'), Option('b')]"

	b := [?int(1), 7]!
	println(b)
	assert '${b}' == '[Option(1), Option(7)]'
}
