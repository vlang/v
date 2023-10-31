// NB: the struct here deliberately has 9 member fields, to trigger
// the V heuristic, that decides to pass a Foo receiver automatically
// by reference, when a struct is too large.
struct Foo {
	one   int
	two   int
	three int
	four  int
	five  int
	six   int
	seven int
	eight int
	nine  int
}

fn (_ Foo) == (_ Foo) bool {
	return true
}

fn test_op() {
	a := Foo{
		one: 1
	}
	b := Foo{
		two: 2
	}
	assert a == b
	dump(a)
	dump(b)
}

struct ManyFields {
mut:
	f01 int
	f02 int
	f03 int
	f04 int
	f05 int
	f06 int
	f07 int
	f08 int
	f09 int
	f10 int
	f11 int
	f12 int
}

fn (mf ManyFields) inc() ManyFields {
	mut res := mf
	res.f01 += 1
	return res
}

fn test_a_struct_with_many_fields_can_be_used_as_receiver_directly_without_assigning_to_an_intermediate_variable() {
	x := ManyFields{}.inc()
	assert x.f01 == 1
}
