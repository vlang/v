import time

fn test() ?i64 {
	return bar()
}

fn test2() ?i64 {
	return none
}

fn bar() ?time.Duration {
	return time.Duration(0)
}

fn baz() ?time.Duration {
	return none
}

fn test_main() {
	assert test() != none
	assert test2() == none
}
