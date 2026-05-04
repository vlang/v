module main

struct DollarOffsetStruct {
	a u8
	b int
	c f64
}

fn test_dollar_prefixed_pseudo_functions() {
	// vfmt off
	mut n := 1
	ptr := &n
	assert $typeof(n).name == 'int'
	assert $typeof[string]().name == 'string'
	assert $sizeof(n) == sizeof(n)
	assert $sizeof[DollarOffsetStruct]() == sizeof(DollarOffsetStruct)
	assert $isreftype(ptr)
	assert $isreftype[&int]()
	assert !$isreftype[int]()
	assert $__offsetof(DollarOffsetStruct, b) > $__offsetof(DollarOffsetStruct, a)
	assert $__offsetof(DollarOffsetStruct, c) > $__offsetof(DollarOffsetStruct, b)
	assert $dump(n) == n
	// vfmt on
}
