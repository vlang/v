fn test_anon_fn_call() {
	anon_fn := fn () string {
		return test()
	}
	assert anon_fn() == 'Test'
}

fn test() string {
	return 'Test'
}
