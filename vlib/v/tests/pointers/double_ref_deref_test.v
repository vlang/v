fn process(foo &&int) int {
	return **foo
}

fn test_ref_deref() {
	foo := 12
	bar := &foo
	fiz := process(&bar)
	assert fiz == 12
}
