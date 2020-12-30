import v.table

fn test_idx() {
	mut t := table.new_type(table.void_type_idx)
	assert t.idx() == table.void_type_idx
	t = table.new_type(table.i8_type_idx)
	assert t.idx() == table.i8_type_idx
}

fn test_muls() {
	mut t := table.new_type(table.void_type_idx)
	idx := t.idx()
	assert t.nr_muls() == 0
	for i in 0 .. 32 {
		t = t.set_nr_muls(i)
		assert t.nr_muls() == i
	}
	t = t.set_nr_muls(0)
	assert t.nr_muls() == 0
	assert t.is_ptr() == false
	t = t.to_ptr()
	assert t.nr_muls() == 1
	assert t.is_ptr() == true
	t = t.to_ptr()
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
	mut t := table.new_type(table.void_type_idx)
	idx := t.idx()
	nr_muls := t.nr_muls()
	t = t.set_flag(table.TypeFlag.optional)
	assert t.has_flag(table.TypeFlag.optional) == true
	assert t.has_flag(table.TypeFlag.variadic) == false
	assert t.has_flag(table.TypeFlag.generic) == false
	t = t.set_flag(table.TypeFlag.variadic)
	assert t.has_flag(table.TypeFlag.optional) == true
	assert t.has_flag(table.TypeFlag.variadic) == true
	assert t.has_flag(table.TypeFlag.generic) == false
	t = t.set_flag(table.TypeFlag.generic)
	assert t.has_flag(table.TypeFlag.optional) == true
	assert t.has_flag(table.TypeFlag.variadic) == true
	assert t.has_flag(table.TypeFlag.generic) == true
	assert t.idx() == idx
	assert t.nr_muls() == nr_muls
	t = t.clear_flag(table.TypeFlag.optional)
	assert t.has_flag(table.TypeFlag.optional) == false
	assert t.has_flag(table.TypeFlag.variadic) == true
	assert t.has_flag(table.TypeFlag.generic) == true
	t = t.clear_flag(table.TypeFlag.variadic)
	assert t.has_flag(table.TypeFlag.optional) == false
	assert t.has_flag(table.TypeFlag.variadic) == false
	assert t.has_flag(table.TypeFlag.generic) == true
	t = t.clear_flag(table.TypeFlag.generic)
	assert t.has_flag(table.TypeFlag.optional) == false
	assert t.has_flag(table.TypeFlag.variadic) == false
	assert t.has_flag(table.TypeFlag.generic) == false
	assert t.idx() == idx
	assert t.nr_muls() == nr_muls
}

fn test_derive() {
	mut t := table.new_type(table.i8_type_idx)
	t = t.set_flag(table.TypeFlag.generic)
	t = t.set_flag(table.TypeFlag.variadic)
	t = t.set_nr_muls(10)
	mut t2 := table.new_type(table.i16_type_idx)
	t2 = t2.derive(t)
	assert t2.has_flag(table.TypeFlag.optional) == false
	assert t2.has_flag(table.TypeFlag.variadic) == true
	assert t2.has_flag(table.TypeFlag.generic) == true
	assert t2.nr_muls() == 10
}
