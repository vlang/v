fn test_fixed_array_can_be_assigned() {
	x := 2.32
	mut v := [8]f64
	v = [1.0, x, 3.0,4.0,5.0,6.0,7.0,8.0]!!
	assert v[1] == x
}

fn test_fixed_array_can_be_used_in_declaration() {
	x := 2.32
	v := [1.0, x, 3.0,4.0,5.0,6.0,7.0,8.0]!!
	assert v[1] == x
}


struct Context {
	pub mut:
	vb [8]f64
}

fn test_fixed_array_can_be_assigned_to_a_struct_field() {
	mut ctx := Context{}
	x := 2.32
	ctx.vb = [1.1, x, 3.3, 4.4, 5.0, 6.0, 7.0, 8.9]!!
	assert ctx.vb[1] == x
	assert ctx.vb[7] == 8.9
	/*
	println( ctx.vb[0] )
	println( ctx.vb[1] )
	println( ctx.vb[2] )
	println( ctx.vb[3] )
	*/
}
