struct PtrNilFoo {
	x int
}

fn test_struct_pointer_comparison_with_nil_is_nil_safe() {
	p := &PtrNilFoo{
		x: 42
	}
	same_value := &PtrNilFoo{
		x: 42
	}
	other := &PtrNilFoo{
		x: 7
	}
	nil_p := unsafe { &PtrNilFoo(nil) }
	nil_q := unsafe { &PtrNilFoo(nil) }

	assert p == same_value
	assert p != other
	assert p != nil_p
	assert nil_p != p
	assert nil_p == nil_q
}
