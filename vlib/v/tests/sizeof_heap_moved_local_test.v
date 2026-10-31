struct SizeofStage {
mut:
	a [10]i32
	b f32
}

fn fill_sizeof_stage(mut s SizeofStage) {
	s.b = 2.5
}

fn keep_sizeof_stage(s &SizeofStage) f32 {
	return s.b
}

fn test_sizeof_of_a_local_whose_address_is_taken() {
	mut stage := SizeofStage{}
	fill_sizeof_stage(mut stage)
	assert keep_sizeof_stage(&stage) == 2.5
	assert sizeof(stage) == sizeof(SizeofStage)
	assert sizeof(stage) == 44
	mut n := 7
	p := &n
	assert sizeof(n) == sizeof(int)
	assert unsafe { *p } == 7
}
