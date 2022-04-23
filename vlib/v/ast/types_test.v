import v.ast

fn test_idx() {
	mut t := ast.new_type(ast.void_type_idx)
	assert t.idx() == ast.void_type_idx
	t = ast.new_type(ast.i8_type_idx)
	assert t.idx() == ast.i8_type_idx
}

fn test_muls() {
	mut t := ast.new_type(ast.void_type_idx)
	idx := t.idx()
	assert t.nr_muls() == 0
	for i in 0 .. 32 {
		t = t.set_nr_muls(i)
		assert t.nr_muls() == i
	}
	t = t.set_nr_muls(0)
	assert t.nr_muls() == 0
	assert t.is_ptr() == false
	t = t.ref()
	assert t.nr_muls() == 1
	assert t.is_ptr() == true
	t = t.ref()
	assert t.nr_muls() == 2
	assert t.is_ptr() == true
	t = t.deref()
	assert t.nr_muls() == 1
	assert t.is_ptr() == true
	t = t.deref()
	assert t.nr_muls() == 0
	assert t.is_ptr() == false
	assert t.idx() == idx
}

fn test_flags() {
	mut t := ast.new_type(ast.void_type_idx)
	idx := t.idx()
	nr_muls := t.nr_muls()
	t = t.set_flag(ast.TypeFlag.optional)
	assert t.has_flag(ast.TypeFlag.optional) == true
	assert t.has_flag(ast.TypeFlag.variadic) == false
	assert t.has_flag(ast.TypeFlag.generic) == false
	t = t.set_flag(ast.TypeFlag.variadic)
	assert t.has_flag(ast.TypeFlag.optional) == true
	assert t.has_flag(ast.TypeFlag.variadic) == true
	assert t.has_flag(ast.TypeFlag.generic) == false
	t = t.set_flag(ast.TypeFlag.generic)
	assert t.has_flag(ast.TypeFlag.optional) == true
	assert t.has_flag(ast.TypeFlag.variadic) == true
	assert t.has_flag(ast.TypeFlag.generic) == true
	assert t.idx() == idx
	assert t.nr_muls() == nr_muls
	t = t.clear_flag(ast.TypeFlag.optional)
	assert t.has_flag(ast.TypeFlag.optional) == false
	assert t.has_flag(ast.TypeFlag.variadic) == true
	assert t.has_flag(ast.TypeFlag.generic) == true
	t = t.clear_flag(ast.TypeFlag.variadic)
	assert t.has_flag(ast.TypeFlag.optional) == false
	assert t.has_flag(ast.TypeFlag.variadic) == false
	assert t.has_flag(ast.TypeFlag.generic) == true
	t = t.clear_flag(ast.TypeFlag.generic)
	assert t.has_flag(ast.TypeFlag.optional) == false
	assert t.has_flag(ast.TypeFlag.variadic) == false
	assert t.has_flag(ast.TypeFlag.generic) == false
	assert t.idx() == idx
	assert t.nr_muls() == nr_muls
}

fn test_derive() {
	mut t := ast.new_type(ast.i8_type_idx)
	t = t.set_flag(ast.TypeFlag.generic)
	t = t.set_flag(ast.TypeFlag.variadic)
	t = t.set_nr_muls(10)
	mut t2 := ast.new_type(ast.i16_type_idx)
	t2 = t2.derive(t)
	assert t2.has_flag(ast.TypeFlag.optional) == false
	assert t2.has_flag(ast.TypeFlag.variadic) == true
	assert t2.has_flag(ast.TypeFlag.generic) == true
	assert t2.nr_muls() == 10
}

fn test_flip_signedness() {
	assert ast.i8_type.flip_signedness() == ast.byte_type
	assert ast.u16_type.flip_signedness() == ast.i16_type
	assert ast.isize_type.flip_signedness() == ast.usize_type
}
