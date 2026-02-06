fn test_main() {
	assert some_func2(14)? + 'c' == 'aabcac'
}

fn some_func(i int) ?string {
	return 'a'
}

fn some_func2(i int) ?string {
	return match i {
		12 { some_func(1)? + 'b' }
		13 { some_func(1)? + 'b' + 'c' }
		14 { 'a' + some_func(1)? + 'b' + 'c' + some_func(1)? }
		else { none }
	}
}
