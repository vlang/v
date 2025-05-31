fn test_main() {
	var := ?int(none)
	println(var or { 100 })
	println(var or { 100 })
	assert dump(var or { 100 }) == 100
	assert dump(var or { 100 }) == 100
	assert true
}
