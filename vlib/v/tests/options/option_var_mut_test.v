fn test_main() {
	mut entrykey := ?string(none)
	mut res := false
	if entrykey != none {
		println('entrykey is a string')
		println(entrykey.len)
		println(entrykey)
		res = entrykey.len > 0
	} else {
		assert true
	}
	assert !res
}
