type Foo = int | string

fn test_optional_none_assign_sumtype() {
	x := ?Foo(none)
	// assert x.err == none
	// assert !x.has_value
}

fn test_optional_none_assign() {
	x := ?int(none)
	// assert x.err == none
	// assert !x.has_valu
}


fn test_optional_value_assign_sumtype() {
	x := ?Foo('test')
}

fn test_optional_value_assign() {
	x := ?int(0)
}
// TODO: make this working next
/*
fn test_optional_none_reassign() {
	mut x := ?Foo('test')
	x = none
}
*/
