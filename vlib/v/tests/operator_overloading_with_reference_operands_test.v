struct Resources {
mut:
	hru u64
	sru u64
	cru u64
	mru u64
}

fn (a &Resources) < (b &Resources) bool {
	return a.hru < b.hru && a.sru < b.sru && a.cru < b.cru && a.mru < b.mru
}

fn (a &Resources) == (b &Resources) bool {
	return a.hru == b.hru
}

fn test_struct_with_reference_operands_for_the_overloaded_operators_do_work() {
	aa := Resources{}
	bb := Resources{}
	assert dump(aa < bb) == false
	assert dump(aa == bb) == true
	assert dump(aa != bb) == false
	assert dump(aa > bb) == false
	assert dump(aa <= bb) == true
	assert dump(aa >= bb) == true
}

// Issue: https://github.com/vlang/v/issues/15859
struct Foo {
	id u32
	x  u32
	y  u32
}

fn (f &Foo) == (o &Foo) bool {
	return f.id == o.id
}

fn test_eq_operator_with_reference_operands() {
	a := Foo{1, 4, 5}
	b := Foo{1, 9, 10}
	if a == b {
		assert true
		return
	}
	assert false
}
