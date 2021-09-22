type Foo = int | string

fn test_optional_none_assign() {
	x := ?Foo(none)
	// assert x.err == none
	// assert !x.has_value
}

fn test_optional_none_assign_nonsumtype() {
	x := ?int(none)
	// assert x.err == none
	// assert !x.has_value
}

// TODO: make this working next
/*
fn test_optional_value_assign() {
	x := ?Foo('test')
}

fn test_optional_none_reassign() {
	mut x := ?Foo('test')
	x = none
}
*/
