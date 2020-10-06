fn call<T>(v T) {
}

fn simple<T>(p T) T {
	return p
}

fn test_infer() {
	call(3)
	i := 4
	r := simple(i)
	assert r == 4
}

fn test_explicit_calls_should_also_work(){
	call<int>(2)
	assert true    
	simple<int>(5)
	assert true    
}
