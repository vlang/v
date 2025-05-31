fn test_main() {
	k := [10, 20, 30]
	println(k.map(it.hex()))
	dump(k.map(it.hex))
	a := k.map(it.hex)
	assert dump(a[0]()) == 'a'
	assert dump(a[1]()) == '14'
	assert dump(a[2]()) == '1e'
}
