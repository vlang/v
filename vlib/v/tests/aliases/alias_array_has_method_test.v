type Alias = []int

fn (a Alias) last() int {
	return 22
}

fn test_alias_array_has_method() {
	ret := Alias([0]).last()
	println(ret)
	assert ret == 22
}
