fn call<T>(v T) {
}

fn simple<T>(p T) T {
	return p
}

fn test_infer() {
	call<int>(2) // needed first
	call(3)
	i := 4
	simple<int>(i) // needed first
	r := simple(i)
	assert r == 4
}
